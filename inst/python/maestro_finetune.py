"""
maestro_finetune.py
Fine-tuning du modele MAESTRO sur le dataset TreeSatAI.

Le checkpoint pre-entraine MAESTRO ne contient que les encodeurs (MAE self-supervised).
La tete de classification est aleatoire. Ce script:
  1. Telecharge le dataset TreeSatAI depuis Zenodo ou Hugging Face
  2. Charge les encodeurs pre-entraines MAESTRO
  3. Entraine la tete de classification (et optionnellement fine-tune les encodeurs)
  4. Sauvegarde un checkpoint avec tete entrainee

Usage depuis R:
  reticulate::source_python("maestro_finetune.py")
  finetuner(checkpoint_path, data_dir, output_path, ...)

Usage en ligne de commande:
  python maestro_finetune.py --checkpoint model.ckpt --data_dir TreeSatAI/ \
    --output finetuned.pt --epochs 30 --lr 1e-3
"""

import os
import sys
import types
import json
import argparse
from pathlib import Path

import warnings
# Supprimer les warnings tifffile GDAL_NODATA (inoffensifs, images uint8 avec nodata=-9999)
warnings.filterwarnings("ignore", message=".*GDAL_NODATA.*")

import torch
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import Dataset, DataLoader
import numpy as np

# Importer l'architecture MAESTRO depuis maestro_inference.py
_this_dir = Path(__file__).resolve().parent
sys.path.insert(0, str(_this_dir))
from maestro_inference import (
    MAESTROClassifier, MODALITIES, ESSENCES, S1_ENCODER_KEY,
    _install_maestro_stubs, _resoudre_chemin_hf,
    _normaliser_optique, _normaliser_mnt,
)


# ============================================================================
# Classes TreeSatAI regroupees (8 classes)
# ============================================================================
#
# Schema simplifie pour le fine-tuning sur TreeSatAI :
#   0 = Chene         (Quercus spp.)
#   1 = Hetre         (Fagus sylvatica)
#   2 = Pin           (Pinus spp.)
#   3 = Epicea        (Picea abies)
#   4 = Douglas       (Pseudotsuga menziesii) -- sempervirent
#   5 = Meleze        (Larix spp.)            -- caduc, phenologie differente
#   6 = Feuillus div. (Betula, Populus, Alnus, Fraxinus, Acer, Castanea, ...)
#
# On passera aux 13 classes PureForest quand le LiDAR sera integre.
# La detection coupe/vide est geree en amont via FLAIR.

ESSENCES_TREESATAI = [
    "Chene",              # 0
    "Hetre",              # 1
    "Pin",                # 2
    "Epicea",             # 3
    "Douglas/Sapin",      # 4 - Pseudotsuga + Abies (resineux sempervirents sombres)
    "Meleze",             # 5 - caduc, phenologie distincte
    "Feuillus divers",    # 6
]

N_CLASSES_TREESATAI = len(ESSENCES_TREESATAI)  # 7

# Mapping TreeSatAI 20 especes -> 8 classes regroupees
TREESATAI_TO_NEMETON = {
    # Chene (0)
    "Quercus": 0,
    "Quercus robur": 0,
    "Quercus petraea": 0,
    "Quercus rubra": 0,
    # Hetre (1)
    "Fagus": 1,
    "Fagus sylvatica": 1,
    # Pin (2)
    "Pinus": 2,
    "Pinus sylvestris": 2,
    # Epicea (3)
    "Picea": 3,
    "Picea abies": 3,
    # Douglas/Sapin (4) -- resineux sempervirents sombres
    "Pseudotsuga": 4,
    "Pseudotsuga menziesii": 4,
    "Abies": 4,
    # Meleze (5) -- caduc
    "Larix": 5,
    "Larix decidua": 5,
    "Larix kaempferi": 5,
    # Feuillus divers (6) -- tous les feuillus non Chene/Hetre
    "Castanea": 6,
    "Populus": 6,
    "Betula": 6,
    "Betula pendula": 6,
    "Betula pubescens": 6,
    "Alnus": 6,
    "Alnus glutinosa": 6,
    "Fraxinus": 6,
    "Fraxinus excelsior": 6,
    "Acer": 6,
    "Acer pseudoplatanus": 6,
    "Tilia": 6,
    "Carpinus": 6,
    "Robinia": 6,
    "Salix": 6,
    "Prunus": 6,
    "Sorbus": 6,
    "Taxus": 6,
}

# Ancien mapping PureForest 13 classes (conserve pour migration future)
TREESATAI_TO_PUREFOREST = {
    "Quercus": 0, "Quercus robur": 0, "Quercus petraea": 0, "Quercus rubra": 0,
    "Fagus": 2, "Fagus sylvatica": 2, "Castanea": 3,
    "Pinus": 5, "Pinus sylvestris": 5,
    "Picea": 8, "Picea abies": 8, "Abies": 9,
    "Pseudotsuga": 10, "Pseudotsuga menziesii": 10,
    "Larix": 11, "Larix decidua": 11, "Larix kaempferi": 11,
    "Populus": 12,
    "Betula": -1, "Betula pendula": -1, "Betula pubescens": -1,
    "Alnus": -1, "Alnus glutinosa": -1,
    "Fraxinus": -1, "Fraxinus excelsior": -1,
    "Acer": -1, "Acer pseudoplatanus": -1,
    "Tilia": -1, "Carpinus": -1, "Robinia": -1, "Salix": -1,
}

# Noms des 20 classes TreeSatAI (ordre standard Zenodo)
TREESATAI_CLASSES = [
    "Abies", "Acer", "Alnus", "Betula", "Carpinus",
    "Castanea", "Fagus", "Fraxinus", "Larix", "Picea",
    "Pinus", "Populus", "Prunus", "Pseudotsuga", "Quercus",
    "Robinia", "Salix", "Sorbus", "Taxus", "Tilia",
]

# Mapping index TreeSatAI -> index classes regroupees (8 classes)
TREESATAI_IDX_TO_NEMETON = {}
for i, name in enumerate(TREESATAI_CLASSES):
    nm = TREESATAI_TO_NEMETON.get(name, -1)
    TREESATAI_IDX_TO_NEMETON[i] = nm

# Mapping index TreeSatAI -> index PureForest (13 classes, pour migration future)
TREESATAI_IDX_TO_PUREFOREST = {}
for i, name in enumerate(TREESATAI_CLASSES):
    pf = TREESATAI_TO_PUREFOREST.get(name, -1)
    TREESATAI_IDX_TO_PUREFOREST[i] = pf


# ============================================================================
# Dataset TreeSatAI
# ============================================================================

class TreeSatAIDataset(Dataset):
    """Dataset TreeSatAI pour fine-tuning MAESTRO.

    Charge les patches aerial (CIR: NIR, G, B) et optionnellement S1/S2.

    Structure attendue du dossier TreeSatAI:
      data_dir/
        aerial/
          train/ ou test/
            <class_name>/
              *.tif (304x304 ou 60x60 patches)
        s1/
          train/ ou test/  (6x6 patches, VV+VH+VV/VH)
        s2/
          train/ ou test/  (6x6 patches, 10 bandes)
        labels_train.csv ou labels.csv (optionnel)

    Les images aerial CIR ont 4 bandes (N, G, B, ?). On les reordonne en RGBI.
    """

    def __init__(self, data_dir, split="train", modalities=None,
                 patch_size=304, target_patch_size=250,
                 class_mapping="nemeton"):
        """
        Args:
            data_dir: Chemin vers le dossier TreeSatAI
            split: "train" ou "test"
            modalities: Liste des modalites ["aerial", "s1", "s2"]
            patch_size: Taille des patches TreeSatAI (304 pour 60m@0.2m)
            target_patch_size: Taille cible pour MAESTRO (250)
            class_mapping: "nemeton" (8 classes), "pureforest" (13 classes),
                           ou None (20 classes TreeSatAI originales)
        """
        self.data_dir = Path(data_dir)
        self.split = split
        self.modalities = modalities or ["aerial"]
        self.patch_size = patch_size
        self.target_patch_size = target_patch_size
        self.class_mapping = class_mapping

        self.samples = []  # (file_path, treesatai_label, mapped_label)
        self._scan_files()

    def _scan_files(self):
        """Scanne les fichiers aerial par classe."""
        aerial_dir = self.data_dir / "aerial" / self.split
        if not aerial_dir.exists():
            # Essayer sans sous-dossier train/test
            aerial_dir = self.data_dir / "aerial"

        if not aerial_dir.exists():
            raise FileNotFoundError(
                "Dossier aerial introuvable: %s" % aerial_dir)

        # Choisir le mapping selon la config
        if self.class_mapping == "nemeton":
            idx_map = TREESATAI_IDX_TO_NEMETON
        elif self.class_mapping == "pureforest":
            idx_map = TREESATAI_IDX_TO_PUREFOREST
        else:
            idx_map = None  # Pas de mapping, classes originales

        for class_dir in sorted(aerial_dir.iterdir()):
            if not class_dir.is_dir():
                continue
            class_name = class_dir.name

            # Trouver l'index TreeSatAI
            if class_name in TREESATAI_CLASSES:
                tsa_idx = TREESATAI_CLASSES.index(class_name)
            else:
                # Essayer correspondance partielle
                tsa_idx = -1
                for i, c in enumerate(TREESATAI_CLASSES):
                    if c.lower() == class_name.lower():
                        tsa_idx = i
                        break

            if tsa_idx < 0:
                print("  [WARN] Classe '%s' non reconnue, ignoree" % class_name)
                continue

            # Mapper le label
            if idx_map is not None:
                mapped_label = idx_map.get(tsa_idx, -1)
                if mapped_label < 0:
                    continue  # Ignorer les especes non-mappables
                label = mapped_label
            else:
                label = tsa_idx

            # Scanner les fichiers .tif
            tif_files = sorted(class_dir.glob("*.tif"))
            for f in tif_files:
                self.samples.append((f, tsa_idx, label))

        mapping_name = self.class_mapping or "originales"
        print("  TreeSatAI %s: %d samples (%d classes %s)" % (
            self.split, len(self.samples),
            len(set(s[2] for s in self.samples)), mapping_name))

    def __len__(self):
        return len(self.samples)

    def _load_aerial(self, path):
        """Charge un patch aerial et le convertit en RGBI."""
        try:
            from PIL import Image
            import tifffile
        except ImportError:
            try:
                import tifffile
            except ImportError:
                raise ImportError(
                    "tifffile ou PIL requis: pip install tifffile Pillow")

        import warnings
        try:
            with warnings.catch_warnings():
                warnings.filterwarnings("ignore", message=".*GDAL_NODATA.*")
                img = tifffile.imread(str(path))
        except Exception:
            from PIL import Image
            img = np.array(Image.open(str(path)))

        # TreeSatAI aerial: CIR = (NIR, R, G) ou (NIR, G, B)
        # On veut RGBI = (R, G, B, NIR) pour MAESTRO
        if img.ndim == 2:
            # Grayscale -> fake 4 bandes
            img = np.stack([img, img, img, img], axis=-1)
        elif img.shape[-1] == 3:
            # CIR (NIR, R, G) -> RGBI (R, G, B=0, NIR)
            nir = img[..., 0].copy()
            r = img[..., 1].copy()
            g = img[..., 2].copy()
            b = np.zeros_like(r)  # Pas de bande bleue dans CIR
            img = np.stack([r, g, b, nir], axis=-1)
        elif img.shape[-1] >= 4:
            # Suppose (NIR, R, G, B) -> (R, G, B, NIR)
            nir = img[..., 0].copy()
            r = img[..., 1].copy()
            g = img[..., 2].copy()
            b_ch = img[..., 3].copy()
            img = np.stack([r, g, b_ch, nir], axis=-1)

        # Resize/crop au target_patch_size
        h, w = img.shape[:2]
        if h != self.target_patch_size or w != self.target_patch_size:
            # Centre crop
            ch = (h - self.target_patch_size) // 2
            cw = (w - self.target_patch_size) // 2
            if ch >= 0 and cw >= 0:
                img = img[ch:ch+self.target_patch_size,
                         cw:cw+self.target_patch_size]
            else:
                # Pad si trop petit
                padded = np.zeros((self.target_patch_size,
                                   self.target_patch_size,
                                   img.shape[-1]), dtype=img.dtype)
                py = (self.target_patch_size - h) // 2
                px = (self.target_patch_size - w) // 2
                padded[py:py+h, px:px+w] = img
                img = padded

        # (H, W, C) -> (C, H, W), float32, normalise [0,1]
        img = img.astype(np.float32)
        img = np.transpose(img, (2, 0, 1))
        if img.max() > 1.0:
            img = img / 255.0

        return img

    def _load_sentinel(self, aerial_path, modality):
        """Charge un patch S1 ou S2 correspondant au patch aerial."""
        # Meme nom de fichier mais dans le dossier s1/ ou s2/
        sentinel_dir = self.data_dir / modality / self.split
        if not sentinel_dir.exists():
            sentinel_dir = self.data_dir / modality

        # Retrouver le meme fichier (meme classe, meme nom)
        class_name = aerial_path.parent.name
        sentinel_file = sentinel_dir / class_name / aerial_path.name

        if not sentinel_file.exists():
            return None

        try:
            import tifffile
            import warnings
            with warnings.catch_warnings():
                warnings.filterwarnings("ignore", message=".*GDAL_NODATA.*")
                img = tifffile.imread(str(sentinel_file)).astype(np.float32)
        except Exception:
            return None

        if img.ndim == 2:
            img = img[np.newaxis, ...]
        elif img.ndim == 3 and img.shape[-1] <= 12:
            img = np.transpose(img, (2, 0, 1))

        return img

    def __getitem__(self, idx):
        file_path, tsa_label, label = self.samples[idx]

        data = {}

        # Aerial (toujours present)
        aerial = self._load_aerial(file_path)
        data["aerial"] = torch.from_numpy(aerial)

        # S1 (optionnel)
        if "s1" in self.modalities:
            s1 = self._load_sentinel(file_path, "s1")
            if s1 is not None:
                # S1: 3 bandes (VV, VH, VV/VH) -> 2 bandes (VV, VH) pour s1_asc
                if s1.shape[0] >= 2:
                    s1 = s1[:2]  # VV, VH
                s1_t = torch.from_numpy(s1)
                # Normaliser
                s1_min = s1_t.min()
                s1_max = s1_t.max()
                if s1_max > s1_min:
                    s1_t = (s1_t - s1_min) / (s1_max - s1_min)
                data["s1_asc"] = s1_t

        # S2 (optionnel)
        if "s2" in self.modalities:
            s2 = self._load_sentinel(file_path, "s2")
            if s2 is not None:
                s2_t = torch.from_numpy(s2)
                if s2_t.max() > 1.0:
                    s2_t = s2_t / 10000.0
                data["s2"] = s2_t

        return data, label


# ============================================================================
# Fine-tuning
# ============================================================================

def finetuner(checkpoint_path, data_dir, output_path,
              epochs=30, lr=1e-3, lr_encoder=1e-5,
              batch_size=16, freeze_encoder=True,
              modalities=None, n_classes=7,
              device="cpu", patience=5,
              augment=True, weight_decay=1e-4):
    """
    Fine-tune MAESTRO sur TreeSatAI.

    Args:
        checkpoint_path: Chemin vers le checkpoint pre-entraine MAESTRO (.ckpt)
        data_dir: Chemin vers le dossier TreeSatAI
        output_path: Chemin de sortie pour le checkpoint fine-tune
        epochs: Nombre d'epoques
        lr: Learning rate pour la tete de classification
        lr_encoder: Learning rate pour les encodeurs (si non geles)
        batch_size: Taille du batch
        freeze_encoder: Geler les encodeurs (True = entrainer seulement la tete)
        modalities: Liste des modalites a utiliser
        n_classes: Nombre de classes (8 = TreeSatAI regroupe, 13 = PureForest)
        device: 'cpu' ou 'cuda'
        patience: Early stopping patience
        augment: Augmentation des donnees (flips, rotations)
        weight_decay: Weight decay pour l'optimiseur

    Returns:
        dict avec historique d'entrainement et chemin du checkpoint
    """
    device = torch.device(device)
    if modalities is None:
        modalities = ["aerial"]

    print("=" * 60)
    print(" MAESTRO Fine-tuning sur TreeSatAI")
    print(" Modalites: %s" % ", ".join(modalities))
    print(" Epochs: %d, LR head: %s, LR encoder: %s" % (
        epochs, lr, lr_encoder))
    print(" Freeze encoder: %s" % freeze_encoder)
    print("=" * 60)

    # 1. Charger le modele pre-entraine
    print("\n[1/5] Chargement du modele pre-entraine...")
    mod_config = {}
    for m in modalities:
        if m in MODALITIES:
            mod_config[m] = MODALITIES[m]
        elif m == "s1":
            mod_config["s1_asc"] = MODALITIES["s1_asc"]

    model = MAESTROClassifier(
        embed_dim=768, encoder_depth=9, inter_depth=3,
        num_heads=12, n_classes=n_classes,
        modalities=mod_config if mod_config else None
    )

    # Charger les poids pre-entraines
    checkpoint_path = Path(checkpoint_path)
    if checkpoint_path.suffix == ".safetensors":
        from safetensors.torch import load_file
        state_dict = load_file(str(checkpoint_path), device=str(device))
    else:
        _install_maestro_stubs()
        chemin_reel = _resoudre_chemin_hf(checkpoint_path)
        import io
        with open(str(chemin_reel), "rb") as f:
            buffer = io.BytesIO(f.read())
        checkpoint = torch.load(buffer, map_location=device,
                                weights_only=False)
        if isinstance(checkpoint, dict):
            if "state_dict" in checkpoint:
                state_dict = checkpoint["state_dict"]
            elif "model" in checkpoint:
                state_dict = checkpoint["model"]
            else:
                state_dict = checkpoint
        else:
            state_dict = checkpoint

    # Filtrer les cles pour les modalites selectionnees
    mod_names = list(mod_config.keys()) if mod_config else list(MODALITIES.keys())
    prefixes = ["model.encoder_inter."]
    for mod_name in mod_names:
        prefixes.append("model.patch_embed.%s." % mod_name)
        enc_name = S1_ENCODER_KEY if mod_name.startswith("s1_") else mod_name
        prefixes.append("model.encoder.%s." % enc_name)

    prefix_tuple = tuple(prefixes)
    filtered_sd = {k: v for k, v in state_dict.items()
                   if k.startswith(prefix_tuple)}

    missing, unexpected = model.load_state_dict(filtered_sd, strict=False)
    missing_head = [k for k in missing if k.startswith("head.")]
    missing_other = [k for k in missing if not k.startswith("head.")]
    print("  Poids charges: %d cles (tete: %d cles aleatoires)" % (
        len(filtered_sd), len(missing_head)))
    if missing_other:
        print("  [WARN] Cles manquantes non-head: %s" % missing_other[:5])

    model = model.to(device)

    # 2. Geler les encodeurs si demande
    if freeze_encoder:
        for name, param in model.named_parameters():
            if not name.startswith("head."):
                param.requires_grad = False
        n_trainable = sum(p.numel() for p in model.parameters() if p.requires_grad)
        n_total = sum(p.numel() for p in model.parameters())
        print("  Parametres entrainables: %s / %s (tete seulement)" % (
            format(n_trainable, ","), format(n_total, ",")))
    else:
        print("  Tous les parametres sont entrainables (fine-tune complet)")

    # 3. Charger le dataset
    print("\n[2/5] Chargement du dataset TreeSatAI...")
    dataset_mods = []
    for m in modalities:
        if m.startswith("s1"):
            dataset_mods.append("s1")
        else:
            dataset_mods.append(m)
    dataset_mods = list(set(dataset_mods))

    # Choisir le mapping selon le nombre de classes
    if n_classes == N_CLASSES_TREESATAI:
        class_mapping = "nemeton"
    elif n_classes == 13:
        class_mapping = "pureforest"
    else:
        class_mapping = None  # Classes originales TreeSatAI

    train_dataset = TreeSatAIDataset(
        data_dir, split="train", modalities=dataset_mods,
        class_mapping=class_mapping
    )
    test_dataset = TreeSatAIDataset(
        data_dir, split="test", modalities=dataset_mods,
        class_mapping=class_mapping
    )

    if len(train_dataset) == 0:
        raise RuntimeError("Aucun sample d'entrainement trouve dans: %s" % data_dir)

    # Collate function pour gerer les dicts multi-modaux
    def collate_fn(batch):
        data_list, labels = zip(*batch)
        labels = torch.tensor(labels, dtype=torch.long)

        # Regrouper par modalite
        all_mods = set()
        for d in data_list:
            all_mods.update(d.keys())

        batched = {}
        for mod in all_mods:
            tensors = [d[mod] for d in data_list if mod in d]
            if len(tensors) == len(data_list):
                batched[mod] = torch.stack(tensors)

        return batched, labels

    train_loader = DataLoader(
        train_dataset, batch_size=batch_size, shuffle=True,
        num_workers=0, collate_fn=collate_fn, drop_last=True
    )
    test_loader = DataLoader(
        test_dataset, batch_size=batch_size, shuffle=False,
        num_workers=0, collate_fn=collate_fn
    )

    # 4. Calculer les poids de classe (compensate imbalance)
    label_counts = {}
    for _, _, label in train_dataset.samples:
        label_counts[label] = label_counts.get(label, 0) + 1

    class_weights = torch.ones(n_classes, device=device)
    total = sum(label_counts.values())
    for cls, count in label_counts.items():
        if 0 <= cls < n_classes:
            class_weights[cls] = total / (n_classes * count)
    # Normaliser
    class_weights = class_weights / class_weights.sum() * n_classes

    # Noms des classes pour l'affichage
    if n_classes == N_CLASSES_TREESATAI:
        class_names = ESSENCES_TREESATAI
    else:
        class_names = ESSENCES

    print("  Classes presentes: %s" % dict(sorted(label_counts.items())))
    print("  Poids de classe: %s" % {
        class_names[k] if k < len(class_names) else k:
        "%.2f" % class_weights[k].item()
        for k in sorted(label_counts.keys()) if 0 <= k < n_classes
    })

    # 5. Entrainement
    print("\n[3/5] Entrainement...")
    criterion = nn.CrossEntropyLoss(weight=class_weights)

    # Parametres avec LR differents
    if freeze_encoder:
        optimizer = optim.AdamW(
            filter(lambda p: p.requires_grad, model.parameters()),
            lr=lr, weight_decay=weight_decay
        )
    else:
        head_params = [p for n, p in model.named_parameters()
                       if n.startswith("head.")]
        encoder_params = [p for n, p in model.named_parameters()
                          if not n.startswith("head.")]
        optimizer = optim.AdamW([
            {"params": head_params, "lr": lr},
            {"params": encoder_params, "lr": lr_encoder},
        ], weight_decay=weight_decay)

    scheduler = optim.lr_scheduler.CosineAnnealingLR(optimizer, T_max=epochs)

    history = {"train_loss": [], "train_acc": [], "val_loss": [], "val_acc": []}
    best_val_acc = 0.0
    best_epoch = 0
    patience_counter = 0

    for epoch in range(epochs):
        # --- Train ---
        model.train()
        if freeze_encoder:
            model.model.eval()  # Garder les encodeurs en mode eval (BatchNorm, Dropout)

        train_loss = 0.0
        train_correct = 0
        train_total = 0

        n_batches = len(train_loader)
        for batch_idx, (data, labels) in enumerate(train_loader):
            labels = labels.to(device)
            data = {k: v.to(device) for k, v in data.items()}

            # Data augmentation simple
            if augment and torch.rand(1).item() > 0.5:
                data = {k: torch.flip(v, dims=[-1]) for k, v in data.items()}
            if augment and torch.rand(1).item() > 0.5:
                data = {k: torch.flip(v, dims=[-2]) for k, v in data.items()}

            optimizer.zero_grad()
            logits = model(data)
            loss = criterion(logits, labels)
            loss.backward()
            optimizer.step()

            train_loss += loss.item() * labels.size(0)
            preds = logits.argmax(dim=1)
            train_correct += (preds == labels).sum().item()
            train_total += labels.size(0)

            # Progression toutes les 50 batches
            if (batch_idx + 1) % 50 == 0 or (batch_idx + 1) == n_batches:
                running_acc = train_correct / max(train_total, 1) * 100
                print("    Epoch %02d | batch %d/%d | loss=%.4f | acc=%.1f%%" % (
                    epoch + 1, batch_idx + 1, n_batches,
                    loss.item(), running_acc), flush=True)

        scheduler.step()

        train_loss /= max(train_total, 1)
        train_acc = train_correct / max(train_total, 1) * 100

        # --- Validation ---
        model.eval()
        val_loss = 0.0
        val_correct = 0
        val_total = 0

        with torch.no_grad():
            for data, labels in test_loader:
                labels = labels.to(device)
                data = {k: v.to(device) for k, v in data.items()}
                logits = model(data)
                loss = criterion(logits, labels)
                val_loss += loss.item() * labels.size(0)
                preds = logits.argmax(dim=1)
                val_correct += (preds == labels).sum().item()
                val_total += labels.size(0)

        val_loss /= max(val_total, 1)
        val_acc = val_correct / max(val_total, 1) * 100

        history["train_loss"].append(train_loss)
        history["train_acc"].append(train_acc)
        history["val_loss"].append(val_loss)
        history["val_acc"].append(val_acc)

        print("  Epoch %02d/%02d | train_loss=%.4f train_acc=%.1f%% | "
              "val_loss=%.4f val_acc=%.1f%%" % (
                  epoch + 1, epochs, train_loss, train_acc,
                  val_loss, val_acc), flush=True)

        # Early stopping / best model
        if val_acc > best_val_acc:
            best_val_acc = val_acc
            best_epoch = epoch + 1
            patience_counter = 0
            # Sauvegarder le meilleur modele
            torch.save({
                "state_dict": model.state_dict(),
                "epoch": epoch + 1,
                "val_acc": val_acc,
                "n_classes": n_classes,
                "class_mapping": class_mapping,
                "class_names": list(class_names[:n_classes]),
                "modalities": list(mod_config.keys()),
                "history": history,
            }, str(output_path))
            print("    -> Meilleur modele sauvegarde (val_acc=%.1f%%)" % val_acc)
        else:
            patience_counter += 1
            if patience_counter >= patience:
                print("  Early stopping a l'epoch %d (patience=%d)" % (
                    epoch + 1, patience))
                break

    # 6. Resume
    print("\n[4/5] Resume du fine-tuning")
    print("  Meilleur epoch: %d (val_acc=%.1f%%)" % (best_epoch, best_val_acc))
    print("  Checkpoint sauvegarde: %s" % output_path)

    # 7. Evaluation finale
    print("\n[5/5] Evaluation finale sur le test set...")
    # Recharger le meilleur modele
    best_ckpt = torch.load(str(output_path), map_location=device,
                            weights_only=False)
    model.load_state_dict(best_ckpt["state_dict"])
    model.eval()

    all_preds = []
    all_labels = []
    with torch.no_grad():
        for data, labels in test_loader:
            labels_d = labels.to(device)
            data = {k: v.to(device) for k, v in data.items()}
            logits = model(data)
            preds = logits.argmax(dim=1).cpu()
            all_preds.extend(preds.tolist())
            all_labels.extend(labels.tolist())

    # Precision par classe
    from collections import Counter
    correct_per_class = Counter()
    total_per_class = Counter()
    for p, l in zip(all_preds, all_labels):
        total_per_class[l] += 1
        if p == l:
            correct_per_class[l] += 1

    print("\n  Precision par classe:")
    for cls in sorted(total_per_class.keys()):
        if 0 <= cls < len(class_names):
            name = class_names[cls]
        else:
            name = "Classe_%d" % cls
        acc = correct_per_class[cls] / total_per_class[cls] * 100
        print("    %s: %.1f%% (%d/%d)" % (
            name, acc, correct_per_class[cls], total_per_class[cls]))

    overall_acc = sum(1 for p, l in zip(all_preds, all_labels) if p == l)
    overall_acc = overall_acc / max(len(all_labels), 1) * 100
    print("\n  Precision globale: %.1f%% (%d samples)" % (
        overall_acc, len(all_labels)))

    return {
        "best_epoch": best_epoch,
        "best_val_acc": best_val_acc,
        "history": history,
        "output_path": str(output_path),
        "overall_test_acc": overall_acc,
    }


# ============================================================================
# CLI
# ============================================================================

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Fine-tune MAESTRO sur TreeSatAI")
    parser.add_argument("--checkpoint", required=True,
                        help="Chemin vers le checkpoint pre-entraine (.ckpt)")
    parser.add_argument("--data_dir", required=True,
                        help="Dossier TreeSatAI")
    parser.add_argument("--output", default="maestro_7classes_treesatai.pt",
                        help="Chemin de sortie [default: maestro_7classes_treesatai.pt]")
    parser.add_argument("--epochs", type=int, default=30)
    parser.add_argument("--lr", type=float, default=1e-3)
    parser.add_argument("--lr_encoder", type=float, default=1e-5)
    parser.add_argument("--batch_size", type=int, default=16)
    parser.add_argument("--freeze_encoder", action="store_true", default=True,
                        help="Geler les encodeurs (entrainer seulement la tete)")
    parser.add_argument("--unfreeze_encoder", action="store_true",
                        help="Fine-tune aussi les encodeurs")
    parser.add_argument("--modalities", nargs="+", default=["aerial"],
                        help="Modalites: aerial s1 s2")
    parser.add_argument("--gpu", action="store_true",
                        help="Utiliser CUDA")
    parser.add_argument("--patience", type=int, default=5)

    args = parser.parse_args()

    freeze = not args.unfreeze_encoder
    dev = "cuda" if args.gpu and torch.cuda.is_available() else "cpu"

    finetuner(
        checkpoint_path=args.checkpoint,
        data_dir=args.data_dir,
        output_path=args.output,
        epochs=args.epochs,
        lr=args.lr,
        lr_encoder=args.lr_encoder,
        batch_size=args.batch_size,
        freeze_encoder=freeze,
        modalities=args.modalities,
        device=dev,
        patience=args.patience,
    )
