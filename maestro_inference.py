"""
maestro_inference.py
Module Python d'inference pour le modele MAESTRO de l'IGNF.
Appele depuis R via reticulate.

Le modele MAESTRO est un Masked Autoencoder (MAE) multi-modal pre-entraine
sur des donnees d'observation de la Terre (aerien RGBI, SPOT, Sentinel, MNT).

Ce module reconstruit l'encodeur aerial du checkpoint MAESTRO et ajoute
une tete de classification pour les essences forestieres.

Architecture du checkpoint (PyTorch Lightning):
  - model.patch_embed.<modality>.patchify_bands.0.conv/norm
  - model.encoder.<modality>.layers.N.0 = Attention (PreNorm)
  - model.encoder.<modality>.layers.N.1 = FeedForward (PreNorm)
  - model.encoder.<modality>.norm = LayerNorm finale
  - model.encoder_inter = encodeur cross-modal (3 couches)
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


# ---------------------------------------------------------------------------
# Architecture MAESTRO (conforme au checkpoint IGNF)
# ---------------------------------------------------------------------------

class PatchifyBands(nn.Module):
    """Patch embedding par modalite: Conv2d + LayerNorm.

    Clés checkpoint:
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

    Clés checkpoint:
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

    Clés checkpoint:
      net.0.weight, net.0.bias   LayerNorm(embed_dim)
      net.1.weight, net.1.bias   Linear(embed_dim -> mlp_dim)
      net.4.weight, net.4.bias   Linear(mlp_dim -> embed_dim)
      (net.2 = GELU, net.3 = Dropout -- pas de parametres)
    """

    def __init__(self, embed_dim, mlp_dim, dropout=0.0):
        super().__init__()
        self.net = nn.Sequential(
            nn.LayerNorm(embed_dim),      # net.0
            nn.Linear(embed_dim, mlp_dim),  # net.1
            nn.GELU(),                     # net.2
            nn.Dropout(dropout),           # net.3
            nn.Linear(mlp_dim, embed_dim),  # net.4
        )

    def forward(self, x):
        return x + self.net(x)


class TransformerEncoder(nn.Module):
    """Encodeur Transformer: N couches [Attention, FeedForward] + norm finale.

    Clés checkpoint:
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
    """Modele MAESTRO reconstruit pour l'inference aerienne.

    On charge uniquement:
      - model.patch_embed.aerial  (Conv2d 4ch -> 768, patch 16x16)
      - model.encoder.aerial      (9 couches transformer)
      - model.encoder_inter        (3 couches transformer, optionnel)

    Les autres modalites (spot, dem, s1, s2) sont ignorees.
    """

    def __init__(self, in_channels=4, embed_dim=768, patch_size=16,
                 encoder_depth=9, inter_depth=3, num_heads=12,
                 mlp_ratio=4.0, use_inter=True):
        super().__init__()

        # Patch embedding (aerial)
        self.patch_embed = nn.ModuleDict({
            "aerial": PatchifyBands(in_channels, embed_dim, patch_size),
        })

        # Encodeur aerial
        self.encoder = nn.ModuleDict({
            "aerial": TransformerEncoder(embed_dim, encoder_depth,
                                         num_heads, mlp_ratio),
        })

        # Encodeur inter-modal (utilise comme couches supplementaires)
        self.use_inter = use_inter
        if use_inter:
            self.encoder_inter = TransformerEncoder(
                embed_dim, inter_depth, num_heads, mlp_ratio
            )

    def forward(self, x):
        # x: (B, C, H, W) -- aerial RGBI
        x = self.patch_embed["aerial"](x)     # (B, N, D)
        x = self.encoder["aerial"](x)         # (B, N, D)
        if self.use_inter:
            x = self.encoder_inter(x)          # (B, N, D)
        return x


class MAESTROClassifier(nn.Module):
    """Classificateur d'essences forestieres base sur MAESTRO.

    Utilise l'encodeur aerial MAESTRO pre-entraine avec une tete de
    classification par pooling moyen des tokens.
    """

    def __init__(self, in_channels=4, embed_dim=768, patch_size=16,
                 encoder_depth=9, inter_depth=3, num_heads=12,
                 n_classes=13, use_inter=True):
        super().__init__()

        self.model = MAESTROModel(
            in_channels=in_channels,
            embed_dim=embed_dim,
            patch_size=patch_size,
            encoder_depth=encoder_depth,
            inter_depth=inter_depth,
            num_heads=num_heads,
            use_inter=use_inter,
        )

        # Tete de classification (mean pooling -> projection)
        self.head = nn.Sequential(
            nn.LayerNorm(embed_dim),
            nn.Linear(embed_dim, 256),
            nn.GELU(),
            nn.Dropout(0.1),
            nn.Linear(256, n_classes),
        )

    def forward(self, x):
        features = self.model(x)          # (B, N, D)
        pooled = features.mean(dim=1)     # (B, D)
        logits = self.head(pooled)        # (B, n_classes)
        return logits


# ---------------------------------------------------------------------------
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

def charger_modele(chemin_poids, n_classes=13, device="cpu", in_channels=4):
    """
    Charge le modele MAESTRO avec les poids pre-entraines.

    Args:
        chemin_poids: Chemin vers le fichier de poids (.ckpt, .pt, .pth, .safetensors)
        n_classes: Nombre de classes de sortie (13 pour PureForest)
        device: 'cpu' ou 'cuda'
        in_channels: Nombre de bandes d'entree (4=RGBI aerien)

    Returns:
        Modele PyTorch en mode evaluation
    """
    chemin = Path(chemin_poids)
    device = torch.device(device)

    # Configuration MAESTRO medium (deduite du checkpoint)
    embed_dim = 768
    encoder_depth = 9
    inter_depth = 3
    num_heads = 12
    patch_size = 16

    print("  Architecture: MAESTRO medium (embed_dim=%d, encoder=%d layers, "
          "inter=%d layers, heads=%d, patch=%d)" % (
              embed_dim, encoder_depth, inter_depth, num_heads, patch_size))
    print("  Entree: %d bandes (aerial RGBI), patches %dx%d" % (
        in_channels, patch_size, patch_size))
    print("  Sortie: %d classes d'essences" % n_classes)

    # Creer le modele
    modele = MAESTROClassifier(
        in_channels=in_channels,
        embed_dim=embed_dim,
        patch_size=patch_size,
        encoder_depth=encoder_depth,
        inter_depth=inter_depth,
        num_heads=num_heads,
        n_classes=n_classes,
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
        checkpoint = torch.load(str(chemin), map_location=device,
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

    # Filtrer: ne garder que les cles pour aerial + encoder_inter
    prefixes_gardees = (
        "model.patch_embed.aerial.",
        "model.encoder.aerial.",
        "model.encoder_inter.",
    )
    filtered_sd = {}
    for k, v in state_dict.items():
        if k.startswith(prefixes_gardees):
            filtered_sd[k] = v

    print("  Checkpoint: %d cles totales, %d cles utilisees (aerial + inter)"
          % (len(state_dict), len(filtered_sd)))

    # Charger les poids (mode non strict pour la tete de classification)
    missing, unexpected = modele.load_state_dict(filtered_sd, strict=False)

    # Les cles manquantes attendues sont uniquement la tete de classification
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

def _normaliser_image(img, n_optical=4):
    """Normalise les bandes optiques [0,255]->[0,1]."""
    if img[:, :n_optical].max() > 1.0:
        img[:, :n_optical] = img[:, :n_optical] / 255.0
    return img


def predire_patch(modele, image_np, device="cpu"):
    """
    Predit l'essence forestiere pour un patch d'image.

    Args:
        modele: Modele MAESTROClassifier
        image_np: Array numpy (H, W, C) ou (C, H, W), valeurs 0-255
        device: 'cpu' ou 'cuda'

    Returns:
        dict avec 'classe' (int), 'essence' (str), 'probabilites' (array)
    """
    device = torch.device(device)

    if isinstance(image_np, np.ndarray):
        img = torch.from_numpy(image_np.copy()).float()
    else:
        img = image_np.float()

    # Si (H, W, C) -> (C, H, W)
    if img.dim() == 3 and img.shape[-1] <= 5:
        img = img.permute(2, 0, 1)

    if img.dim() == 3:
        img = img.unsqueeze(0)

    img = _normaliser_image(img)
    img = img.to(device)

    with torch.no_grad():
        logits = modele(img)
        probs = torch.softmax(logits, dim=1)
        classe = torch.argmax(probs, dim=1).item()

    return {
        "classe": classe,
        "essence": ESSENCES[classe] if classe < len(ESSENCES) else "Classe_%d" % classe,
        "probabilites": probs.cpu().numpy().flatten().tolist(),
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

    batch = torch.from_numpy(np.array(images_np).copy()).float()

    # Si (B, H, W, C) -> (B, C, H, W)
    if batch.dim() == 4 and batch.shape[-1] <= 5:
        batch = batch.permute(0, 3, 1, 2)

    batch = _normaliser_image(batch)
    batch = batch.to(device)

    with torch.no_grad():
        logits = modele(batch)
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

    batch = torch.from_numpy(arr)
    batch = _normaliser_image(batch)
    batch = batch.to(device_t)

    with torch.no_grad():
        logits = modele(batch)
        preds = torch.argmax(logits, dim=1).cpu().numpy()

    return preds.tolist()
