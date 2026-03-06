"""
maestro_inference.py
Module Python d'inference pour le modele MAESTRO de l'IGNF.
Appele depuis R via reticulate.

Le modele MAESTRO est un Masked Autoencoder (MAE) multi-modal pre-entraine
sur des donnees d'observation de la Terre :
  - aerial : RGBI 0.2m (4 bandes, patch 16x16)
  - dem    : DSM+DTM 0.2m (2 bandes, patch 32x32)
  - spot   : RGB 1.6m (3 bandes, patch 16x16)
  - s1_asc : Sentinel-1 ascending (2 bandes VV+VH, patch 2x2)
  - s1_des : Sentinel-1 descending (2 bandes VV+VH, patch 2x2)
  - s2     : Sentinel-2 (10 bandes, patch 2x2)

Chaque modalite a son propre encodeur. Les tokens sont ensuite fusionnes
dans un encodeur cross-modal (encoder_inter).

Les modalites manquantes sont simplement ignorees (pas de masking actif).
"""

import types
import sys
import torch
import torch.nn as nn
import numpy as np
from pathlib import Path
import json


# Classes d'essences forestieres PureForest (13 classes)
ESSENCES = [
    "Chene decidue",       # 0 - Quercus spp. (deciduous)
    "Chene vert",          # 1 - Quercus ilex
    "Hetre",               # 2 - Fagus sylvatica
    "Chataignier",         # 3 - Castanea sativa
    "Pin maritime",        # 4 - Pinus pinaster
    "Pin sylvestre",       # 5 - Pinus sylvestris
    "Pin laricio/noir",    # 6 - Pinus nigra
    "Pin d'Alep",          # 7 - Pinus halepensis
    "Epicea",              # 8 - Picea abies
    "Sapin",               # 9 - Abies alba
    "Douglas",             # 10 - Pseudotsuga menziesii
    "Meleze",              # 11 - Larix spp.
    "Peuplier",            # 12 - Populus spp.
]

# Configuration des modalites MAESTRO
MODALITIES = {
    "aerial": {"in_channels": 4, "patch_size": (16, 16)},
    "dem":    {"in_channels": 2, "patch_size": (32, 32)},
    "spot":   {"in_channels": 3, "patch_size": (16, 16)},
    "s1_asc": {"in_channels": 2, "patch_size": (2, 2)},
    "s1_des": {"in_channels": 2, "patch_size": (2, 2)},
    "s2":     {"in_channels": 10, "patch_size": (2, 2)},
}

# Le checkpoint utilise "s1" pour le state_dict de l'encodeur S1
# mais "s1_asc"/"s1_des" pour patch_embed et mask_token
S1_ENCODER_KEY = "s1"


# ---------------------------------------------------------------------------
# Architecture MAESTRO (conforme au checkpoint IGNF)
# ---------------------------------------------------------------------------

class PatchifyBands(nn.Module):
    """Patch embedding par modalite: Conv2d + LayerNorm.

    Cles checkpoint:
      patchify_bands.0.conv.weight  (embed_dim, in_ch, patch_h, patch_w)
      patchify_bands.0.conv.bias    (embed_dim,)
      patchify_bands.0.norm.weight  (embed_dim,)
      patchify_bands.0.norm.bias    (embed_dim,)
    """

    def __init__(self, in_channels, embed_dim, patch_size):
        super().__init__()
        if isinstance(patch_size, int):
            patch_size = (patch_size, patch_size)
        self.patchify_bands = nn.ModuleList([
            nn.ModuleDict({
                "conv": nn.Conv2d(in_channels, embed_dim,
                                  kernel_size=patch_size, stride=patch_size),
                "norm": nn.LayerNorm(embed_dim),
            })
        ])

    def forward(self, x):
        # x: (B, C, H, W)
        x = self.patchify_bands[0]["conv"](x)   # (B, D, H', W')
        B, D, H, W = x.shape
        x = x.permute(0, 2, 3, 1)               # (B, H', W', D)
        x = self.patchify_bands[0]["norm"](x)
        x = x.reshape(B, H * W, D)              # (B, N, D)
        return x


class Attention(nn.Module):
    """Multi-head self-attention avec pre-norm.

    Cles checkpoint:
      norm.weight, norm.bias           (embed_dim,)
      to_qkv.weight                    (3*embed_dim, embed_dim)  -- pas de bias
      to_out.0.weight, to_out.0.bias   (embed_dim, embed_dim)
    """

    def __init__(self, embed_dim, num_heads):
        super().__init__()
        self.num_heads = num_heads
        self.head_dim = embed_dim // num_heads
        self.scale = self.head_dim ** -0.5

        self.norm = nn.LayerNorm(embed_dim)
        self.to_qkv = nn.Linear(embed_dim, 3 * embed_dim, bias=False)
        self.to_out = nn.ModuleList([nn.Linear(embed_dim, embed_dim)])

    def forward(self, x):
        B, N, D = x.shape
        residual = x
        x = self.norm(x)

        qkv = self.to_qkv(x).reshape(B, N, 3, self.num_heads, self.head_dim)
        qkv = qkv.permute(2, 0, 3, 1, 4)  # (3, B, heads, N, head_dim)
        q, k, v = qkv.unbind(0)

        attn = (q @ k.transpose(-2, -1)) * self.scale
        attn = attn.softmax(dim=-1)

        out = (attn @ v).transpose(1, 2).reshape(B, N, D)
        out = self.to_out[0](out)
        return residual + out


class FeedForward(nn.Module):
    """Feed-forward avec pre-norm et GELU.

    Cles checkpoint:
      net.0.weight, net.0.bias   LayerNorm(embed_dim)
      net.1.weight, net.1.bias   Linear(embed_dim -> mlp_dim)
      net.4.weight, net.4.bias   Linear(mlp_dim -> embed_dim)
      (net.2 = GELU, net.3 = Dropout -- pas de parametres)
    """

    def __init__(self, embed_dim, mlp_dim, dropout=0.0):
        super().__init__()
        self.net = nn.Sequential(
            nn.LayerNorm(embed_dim),        # net.0
            nn.Linear(embed_dim, mlp_dim),  # net.1
            nn.GELU(),                      # net.2
            nn.Dropout(dropout),            # net.3
            nn.Linear(mlp_dim, embed_dim),  # net.4
        )

    def forward(self, x):
        return x + self.net(x)


class TransformerEncoder(nn.Module):
    """Encodeur Transformer: N couches [Attention, FeedForward] + norm finale.

    Cles checkpoint:
      layers.N.0 = Attention
      layers.N.1 = FeedForward
      norm.weight, norm.bias = LayerNorm finale
    """

    def __init__(self, embed_dim, depth, num_heads, mlp_ratio=4.0):
        super().__init__()
        mlp_dim = int(embed_dim * mlp_ratio)
        self.layers = nn.ModuleList([
            nn.ModuleList([
                Attention(embed_dim, num_heads),
                FeedForward(embed_dim, mlp_dim),
            ])
            for _ in range(depth)
        ])
        self.norm = nn.LayerNorm(embed_dim)

    def forward(self, x):
        for attn, ff in self.layers:
            x = attn(x)
            x = ff(x)
        x = self.norm(x)
        return x


class MAESTROModel(nn.Module):
    """Modele MAESTRO multi-modal reconstruit pour l'inference.

    Charge tous les encodeurs par modalite depuis le checkpoint:
      - model.patch_embed.<mod>  (Conv2d + LayerNorm)
      - model.encoder.<mod>     (9 couches transformer chacun)
      - model.encoder_inter     (3 couches transformer cross-modal)

    A l'inference, seules les modalites fournies sont utilisees.
    Les tokens de toutes les modalites presentes sont concatenes
    puis passes dans encoder_inter.
    """

    def __init__(self, embed_dim=768, encoder_depth=9, inter_depth=3,
                 num_heads=12, mlp_ratio=4.0, modalities=None):
        super().__init__()

        if modalities is None:
            modalities = MODALITIES

        # Patch embeddings par modalite
        self.patch_embed = nn.ModuleDict()
        for mod_name, cfg in modalities.items():
            self.patch_embed[mod_name] = PatchifyBands(
                cfg["in_channels"], embed_dim, cfg["patch_size"]
            )

        # Encodeurs par modalite
        # Note: s1_asc et s1_des partagent le meme encodeur "s1"
        self.encoder = nn.ModuleDict()
        encoder_names = set()
        for mod_name in modalities:
            enc_name = S1_ENCODER_KEY if mod_name.startswith("s1_") else mod_name
            if enc_name not in encoder_names:
                self.encoder[enc_name] = TransformerEncoder(
                    embed_dim, encoder_depth, num_heads, mlp_ratio
                )
                encoder_names.add(enc_name)

        # Encodeur cross-modal
        self.encoder_inter = TransformerEncoder(
            embed_dim, inter_depth, num_heads, mlp_ratio
        )

        self._modalities = modalities

    def _get_encoder_name(self, mod_name):
        """Retourne le nom de l'encodeur pour une modalite."""
        return S1_ENCODER_KEY if mod_name.startswith("s1_") else mod_name

    def forward(self, inputs):
        """
        Args:
            inputs: dict[str, Tensor] avec les modalites presentes.
                    Ex: {"aerial": (B,4,H,W), "dem": (B,2,H,W)}
                    Ou un seul Tensor (B,C,H,W) pour aerial uniquement.

        Returns:
            Tensor (B, N_total, D) des tokens fusionnes.
        """
        # Support pour un seul tenseur (retrocompatibilite aerial)
        if isinstance(inputs, torch.Tensor):
            inputs = {"aerial": inputs}

        all_tokens = []
        for mod_name, x in inputs.items():
            if mod_name not in self.patch_embed:
                continue
            tokens = self.patch_embed[mod_name](x)
            enc_name = self._get_encoder_name(mod_name)
            tokens = self.encoder[enc_name](tokens)
            all_tokens.append(tokens)

        if not all_tokens:
            raise ValueError("Aucune modalite valide fournie")

        # Concatener tous les tokens des modalites presentes
        x = torch.cat(all_tokens, dim=1)  # (B, N_total, D)

        # Encodeur cross-modal
        x = self.encoder_inter(x)

        return x


class MAESTROClassifier(nn.Module):
    """Classificateur d'essences forestieres base sur MAESTRO.

    Utilise les encodeurs MAESTRO pre-entraines (multi-modal) avec une tete
    de classification par pooling moyen des tokens.
    """

    def __init__(self, embed_dim=768, encoder_depth=9, inter_depth=3,
                 num_heads=12, n_classes=13, modalities=None):
        super().__init__()

        self.model = MAESTROModel(
            embed_dim=embed_dim,
            encoder_depth=encoder_depth,
            inter_depth=inter_depth,
            num_heads=num_heads,
            modalities=modalities,
        )

        # Tete de classification (mean pooling -> projection)
        self.head = nn.Sequential(
            nn.LayerNorm(embed_dim),
            nn.Linear(embed_dim, 256),
            nn.GELU(),
            nn.Dropout(0.1),
            nn.Linear(256, n_classes),
        )

    def forward(self, inputs):
        """
        Args:
            inputs: dict[str, Tensor] ou Tensor (aerial seul).
        """
        features = self.model(inputs)     # (B, N_total, D)
        pooled = features.mean(dim=1)     # (B, D)
        logits = self.head(pooled)        # (B, n_classes)
        return logits


# ---------------------------------------------------------------------------
# Utilitaire pour resoudre les symlinks casses du cache HuggingFace (Windows)
# ---------------------------------------------------------------------------

def _resoudre_chemin_hf(chemin):
    """Resout le chemin reel d'un fichier du cache HuggingFace.

    Sur Windows sans mode developpeur, hfhub cree des symlinks qui echouent.
    Le fichier dans snapshots/ n'existe pas reellement. Le blob reel est dans
    le dossier blobs/ du meme modele. Cette fonction le retrouve.
    """
    import os
    chemin = Path(chemin)

    # Si le fichier est accessible, pas besoin de resolution
    try:
        with open(str(chemin), "rb") as f:
            f.read(1)
        return chemin
    except (PermissionError, FileNotFoundError, OSError):
        pass

    # Remonter jusqu'au dossier snapshots/
    parts = chemin.parts
    for i, part in enumerate(parts):
        if part == "snapshots":
            model_dir = Path(*parts[:i])
            break
    else:
        raise FileNotFoundError(
            "Impossible de trouver le dossier du modele HF pour: %s" % chemin)

    blobs_dir = model_dir / "blobs"
    if not blobs_dir.exists():
        raise FileNotFoundError("Dossier blobs introuvable: %s" % blobs_dir)

    # Chercher le pointer file qui reference le blob
    # Les fichiers pointer HF contiennent juste le hash sha256
    pointer_path = chemin
    if pointer_path.exists():
        try:
            content = pointer_path.read_text(encoding="utf-8").strip()
            # Si c'est un hash (64 chars hex), le blob est blobs/<hash>
            if len(content) == 64:
                blob_candidate = blobs_dir / content
                if blob_candidate.exists():
                    print("  [Windows] Utilisation du blob via pointer: %s"
                          % blob_candidate)
                    return blob_candidate
        except Exception:
            pass

    # Fallback: prendre le plus gros blob (= le checkpoint)
    blobs = list(blobs_dir.iterdir())
    if not blobs:
        raise FileNotFoundError("Aucun blob dans: %s" % blobs_dir)

    biggest = max(blobs, key=lambda p: p.stat().st_size)
    size_mb = biggest.stat().st_size / 1e6
    print("  [Windows] Symlink casse, utilisation directe du blob (%.0f Mo)"
          % size_mb)
    return biggest


# Module stubs pour charger le checkpoint pickle
# ---------------------------------------------------------------------------

def _install_maestro_stubs():
    """Installe des faux modules 'maestro.*' pour que torch.load unpickle."""
    mod_paths = [
        "maestro", "maestro.conf", "maestro.conf.mask",
        "maestro.conf.model", "maestro.conf.data",
        "maestro.conf.train", "maestro.conf.experiment",
        "maestro.model", "maestro.model.mae",
    ]

    class _Stub:
        def __init__(self, *args, **kwargs):
            self.__dict__.update(kwargs)
        def __setstate__(self, state):
            if isinstance(state, dict):
                self.__dict__.update(state)

    for mod_path in mod_paths:
        if mod_path not in sys.modules:
            m = types.ModuleType(mod_path)
            sys.modules[mod_path] = m
            parts = mod_path.rsplit(".", 1)
            if len(parts) == 2 and parts[0] in sys.modules:
                setattr(sys.modules[parts[0]], parts[1], m)

    stub_names = [
        "MaskConfig", "ModelConfig", "DataConfig", "TrainConfig",
        "ExperimentConfig", "Config", "MAE", "MaskedAutoencoder",
    ]
    for mod_path in mod_paths:
        mod = sys.modules[mod_path]
        for attr in stub_names:
            setattr(mod, attr, _Stub)


# ---------------------------------------------------------------------------
# Chargement du modele
# ---------------------------------------------------------------------------

def charger_modele(chemin_poids, n_classes=13, device="cpu", **kwargs):
    """
    Charge le modele MAESTRO avec les poids pre-entraines.

    Args:
        chemin_poids: Chemin vers le fichier de poids (.ckpt, .pt, .pth, .safetensors)
        n_classes: Nombre de classes de sortie (13 pour PureForest)
        device: 'cpu' ou 'cuda'
        modalites: Liste des modalites a charger. Par defaut toutes.
                   Ex: ["aerial", "dem"] ou ["aerial", "dem", "s2"]

    Returns:
        Modele PyTorch en mode evaluation
    """
    modalites = kwargs.get("modalites", None)
    chemin = Path(chemin_poids)
    device = torch.device(device)

    # Configuration MAESTRO medium (deduite du checkpoint)
    embed_dim = 768
    encoder_depth = 9
    inter_depth = 3
    num_heads = 12

    # Selectionner les modalites
    if modalites is None:
        mod_config = dict(MODALITIES)
    else:
        mod_config = {k: MODALITIES[k] for k in modalites if k in MODALITIES}
        if not mod_config:
            raise ValueError(
                "Aucune modalite valide. Choix: %s" % list(MODALITIES.keys())
            )

    mod_names = list(mod_config.keys())
    print("  Architecture: MAESTRO medium (embed_dim=%d, encoder=%d layers, "
          "inter=%d layers, heads=%d)" % (
              embed_dim, encoder_depth, inter_depth, num_heads))
    print("  Modalites: %s" % ", ".join(
        "%s (%dch, patch %s)" % (k, v["in_channels"], v["patch_size"])
        for k, v in mod_config.items()
    ))
    print("  Sortie: %d classes d'essences" % n_classes)

    # Creer le modele
    modele = MAESTROClassifier(
        embed_dim=embed_dim,
        encoder_depth=encoder_depth,
        inter_depth=inter_depth,
        num_heads=num_heads,
        n_classes=n_classes,
        modalities=mod_config,
    )

    # Charger les poids
    if chemin.suffix == ".safetensors":
        try:
            from safetensors.torch import load_file
            state_dict = load_file(str(chemin), device=str(device))
        except ImportError:
            raise ImportError(
                "Le package 'safetensors' est requis. "
                "Installez-le avec : pip install safetensors"
            )
    else:
        # Installer les stubs pour le unpickle du checkpoint MAESTRO
        _install_maestro_stubs()
        # Resoudre le chemin reel (Windows: symlinks HF cache casses)
        chemin_reel = _resoudre_chemin_hf(chemin)
        import io
        with open(str(chemin_reel), "rb") as f:
            buffer = io.BytesIO(f.read())
        checkpoint = torch.load(buffer, map_location=device,
                                weights_only=False)

        # Extraire le state_dict du checkpoint PyTorch Lightning
        if isinstance(checkpoint, dict):
            if "state_dict" in checkpoint:
                state_dict = checkpoint["state_dict"]
            elif "model" in checkpoint:
                state_dict = checkpoint["model"]
            elif "model_state_dict" in checkpoint:
                state_dict = checkpoint["model_state_dict"]
            else:
                state_dict = checkpoint
        else:
            state_dict = checkpoint

    # Filtrer: garder les cles pour les modalites selectionnees + encoder_inter
    prefixes_gardees = ["model.encoder_inter."]
    for mod_name in mod_names:
        prefixes_gardees.append("model.patch_embed.%s." % mod_name)
        enc_name = S1_ENCODER_KEY if mod_name.startswith("s1_") else mod_name
        prefixes_gardees.append("model.encoder.%s." % enc_name)

    prefixes_tuple = tuple(prefixes_gardees)
    filtered_sd = {k: v for k, v in state_dict.items()
                   if k.startswith(prefixes_tuple)}

    print("  Checkpoint: %d cles totales, %d cles utilisees"
          % (len(state_dict), len(filtered_sd)))

    # Charger les poids (mode non strict pour la tete de classification)
    missing, unexpected = modele.load_state_dict(filtered_sd, strict=False)

    # Analyser les cles manquantes
    missing_head = [k for k in missing if k.startswith("head.")]
    missing_other = [k for k in missing if not k.startswith("head.")]
    if missing_other:
        print("  [ATTENTION] Cles manquantes (non-head): %d" % len(missing_other))
        for k in missing_other[:10]:
            print("    - %s" % k)
    if missing_head:
        print("  [INFO] Tete de classification non pre-entrainee "
              "(%d cles, attendu)" % len(missing_head))
    if unexpected:
        print("  [INFO] Cles inattendues (ignorees): %d" % len(unexpected))

    modele = modele.to(device)
    modele.eval()
    n_params = sum(p.numel() for p in modele.parameters())
    n_pretrained = sum(p.numel() for k, p in modele.named_parameters()
                       if not k.startswith("head."))
    print("  Modele charge sur %s (%s parametres, %s pre-entraines)" % (
        device, format(n_params, ","), format(n_pretrained, ",")))

    return modele


# ---------------------------------------------------------------------------
# Fonctions de prediction
# ---------------------------------------------------------------------------

def _normaliser_optique(img, max_val=255.0):
    """Normalise les bandes optiques [0,max_val]->[0,1]."""
    if img.max() > 1.0:
        img = img / max_val
    return img


def _normaliser_mnt(mnt):
    """Normalise le MNT en min-max par batch."""
    mnt_min = mnt.min()
    mnt_max = mnt.max()
    if mnt_max > mnt_min:
        return (mnt - mnt_min) / (mnt_max - mnt_min)
    return torch.zeros_like(mnt)


def _preparer_inputs(donnees, device):
    """
    Prepare les inputs multi-modaux pour le modele.

    Args:
        donnees: dict de numpy arrays ou tenseurs par modalite.
                 Ex: {"aerial": (B,4,H,W), "dem": (B,2,H,W)}
                 Ou un seul array (B,C,H,W) pour aerial seul.

    Returns:
        dict[str, Tensor] normalise et sur le bon device.
    """
    if isinstance(donnees, (np.ndarray, torch.Tensor)):
        donnees = {"aerial": donnees}

    inputs = {}
    for mod_name, data in donnees.items():
        if isinstance(data, np.ndarray):
            t = torch.from_numpy(data.copy()).float()
        else:
            t = data.float()

        # Si (H, W, C) -> (C, H, W)
        if t.dim() == 3 and t.shape[-1] <= 10:
            t = t.permute(2, 0, 1)
        # Si (C, H, W) -> (1, C, H, W)
        if t.dim() == 3:
            t = t.unsqueeze(0)

        # Normalisation selon la modalite
        if mod_name in ("aerial", "spot"):
            t = _normaliser_optique(t)
        elif mod_name == "dem":
            t = _normaliser_mnt(t)
        elif mod_name.startswith("s1_"):
            # S1 dB: typiquement [-25, 0] -> normaliser
            t = _normaliser_mnt(t)
        elif mod_name == "s2":
            # S2 reflectance: typiquement [0, 10000] -> [0, 1]
            t = _normaliser_optique(t, max_val=10000.0)

        inputs[mod_name] = t.to(device)

    return inputs


def predire_patch(modele, image_np, device="cpu"):
    """
    Predit l'essence forestiere pour un patch d'image.

    Args:
        modele: Modele MAESTROClassifier
        image_np: Array numpy (H, W, C) ou (C, H, W) ou dict de modalites
        device: 'cpu' ou 'cuda'

    Returns:
        dict avec 'classe' (int), 'essence' (str), 'probabilites' (array)
    """
    device = torch.device(device)
    inputs = _preparer_inputs(image_np, device)

    with torch.no_grad():
        logits = modele(inputs)
        probs = torch.softmax(logits, dim=1)
        classe = torch.argmax(probs, dim=1).item()

    return {
        "classe": classe,
        "essence": ESSENCES[classe] if classe < len(ESSENCES) else "Classe_%d" % classe,
        "probabilites": probs.cpu().numpy().flatten().tolist(),
    }


def predire_multimodal(modele, donnees, device="cpu"):
    """
    Predit l'essence forestiere a partir de donnees multi-modales.

    Args:
        modele: Modele MAESTROClassifier
        donnees: dict de numpy arrays par modalite.
                 Ex: {"aerial": (B,4,H,W), "dem": (B,2,H,W)}
        device: 'cpu' ou 'cuda'

    Returns:
        dict avec 'classes' (list), 'essences' (list), 'probabilites' (array)
    """
    device = torch.device(device)
    inputs = _preparer_inputs(donnees, device)

    with torch.no_grad():
        logits = modele(inputs)
        probs = torch.softmax(logits, dim=1)
        classes = torch.argmax(probs, dim=1).cpu().numpy().tolist()

    essences = [
        ESSENCES[c] if c < len(ESSENCES) else "Classe_%d" % c
        for c in classes
    ]

    return {
        "classes": classes,
        "essences": essences,
        "probabilites": probs.cpu().numpy().tolist(),
    }


def predire_batch(modele, images_np, device="cpu"):
    """
    Predit les essences forestieres pour un batch de patches.

    Args:
        modele: Modele MAESTROClassifier
        images_np: Array numpy (B, C, H, W), valeurs 0-255
        device: 'cpu' ou 'cuda'

    Returns:
        Liste de predictions (codes de classes)
    """
    device = torch.device(device)
    inputs = _preparer_inputs(images_np, device)

    with torch.no_grad():
        logits = modele(inputs)
        preds = torch.argmax(logits, dim=1).cpu().numpy()

    return preds.tolist()


def predire_batch_from_values(modele, values_np, patch_h=250, patch_w=250,
                               device="cpu"):
    """
    Predit les essences depuis des matrices de valeurs terra (H*W, C).

    Cette fonction est appelee depuis R via reticulate. Les donnees arrivent
    sous forme (B, H*W, C) depuis terra::values() et sont reorganisees
    en (B, C, H, W) pour PyTorch.

    Args:
        modele: Modele MAESTROClassifier
        values_np: Array numpy (B, H*W, C) depuis terra::values()
        patch_h: Hauteur du patch en pixels
        patch_w: Largeur du patch en pixels
        device: 'cpu' ou 'cuda'

    Returns:
        Liste de predictions (codes de classes)
    """
    device_t = torch.device(device)

    arr = np.array(values_np, dtype=np.float32)

    # Reshape (B, H*W, C) -> (B, H, W, C) -> (B, C, H, W)
    B = arr.shape[0]
    C = arr.shape[-1]
    arr = arr.reshape(B, patch_h, patch_w, C)
    arr = np.transpose(arr, (0, 3, 1, 2))  # (B, C, H, W)

    # Si 4 bandes -> aerial RGBI
    # Si 5 bandes -> aerial RGBI + DEM (1 bande)
    # Si 6 bandes -> aerial RGBI + DEM (2 bandes DSM+DTM)
    if C <= 4:
        inputs = {"aerial": torch.from_numpy(arr)}
        inputs["aerial"] = _normaliser_optique(inputs["aerial"])
    elif C == 5:
        # 4 RGBI + 1 MNT -> on duplique le MNT pour avoir 2 canaux DEM
        aerial = torch.from_numpy(arr[:, :4])
        dem_1ch = torch.from_numpy(arr[:, 4:5])
        dem = torch.cat([dem_1ch, dem_1ch], dim=1)  # (B, 2, H, W)
        inputs = {
            "aerial": _normaliser_optique(aerial),
            "dem": _normaliser_mnt(dem),
        }
    elif C >= 6:
        aerial = torch.from_numpy(arr[:, :4])
        dem = torch.from_numpy(arr[:, 4:6])
        inputs = {
            "aerial": _normaliser_optique(aerial),
            "dem": _normaliser_mnt(dem),
        }

    inputs = {k: v.to(device_t) for k, v in inputs.items()}

    with torch.no_grad():
        logits = modele(inputs)
        preds = torch.argmax(logits, dim=1).cpu().numpy()

    return preds.tolist()
