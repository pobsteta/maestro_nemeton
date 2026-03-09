#!/usr/bin/env python3
"""
train_treesatai.py
Fine-tuning de la tete de classification MAESTRO sur TreeSatAI (8 classes).

Le backbone MAESTRO pre-entraine (encodeurs par modalite + encoder_inter)
est charge depuis le checkpoint IGNF, puis une nouvelle tete de classification
est entrainee sur les donnees TreeSatAI regroupees en 8 classes d'essences.

Donnees TreeSatAI (IGNF/TreeSatAI-Time-Series sur HuggingFace) :
  - aerial : RGBI 0.2m, patches 300x300 px (redimensionnes a 256x256)
  - sentinel-1 : VV+VH, patches 6x6 px (asc + desc)
  - sentinel-2 : 10 bandes BOA, patches 6x6 px

Usage :
  python train_treesatai.py --checkpoint /chemin/pretrain-epoch=99.ckpt
  python train_treesatai.py --checkpoint /chemin/pretrain-epoch=99.ckpt --gpu
  python train_treesatai.py --checkpoint /chemin/pretrain-epoch=99.ckpt --unfreeze
  python train_treesatai.py --checkpoint /chemin/pretrain-epoch=99.ckpt --modalites aerial,s1_asc,s1_des,s2
"""

import argparse
import json
import os
import sys
import zipfile
from pathlib import Path

import h5py
import numpy as np
import torch
import torch.nn as nn
from torch.utils.data import Dataset, DataLoader

# Importer l'architecture MAESTRO depuis le module d'inference
sys.path.insert(0, str(Path(__file__).parent))
from maestro_inference import (
    MAESTROClassifier, MODALITIES, ESSENCES,
    _install_maestro_stubs, _normaliser_optique, _normaliser_mnt,
)

# =========================================================================
# Mapping TreeSatAI 15 genres -> 8 classes regroupees
# =========================================================================

CLASSES_TREESATAI = [
    "Chenes",            # 0 - Quercus
    "Hetre",             # 1 - Fagus
    "Autres feuillus",   # 2 - Acer, Alnus, Betula, Fraxinus, Populus, Prunus, Tilia
    "Pins",              # 3 - Pinus
    "Epicea/Sapin",      # 4 - Picea, Abies
    "Douglas",           # 5 - Pseudotsuga
    "Meleze",            # 6 - Larix
    "Cleared",           # 7 - Cleared
]

GENRE_TO_CLASSE = {
    "Quercus":      0,
    "Fagus":        1,
    "Acer":         2,
    "Alnus":        2,
    "Betula":       2,
    "Fraxinus":     2,
    "Populus":      2,
    "Prunus":       2,
    "Tilia":        2,
    "Pinus":        3,
    "Picea":        4,
    "Abies":        4,
    "Pseudotsuga":  5,
    "Larix":        6,
    "Cleared":      7,
}

N_CLASSES = len(CLASSES_TREESATAI)


# =========================================================================
# Dataset TreeSatAI
# =========================================================================

class TreeSatAIDataset(Dataset):
    """Dataset TreeSatAI pour fine-tuning MAESTRO.

    Charge les images aerial (RGBI) et optionnellement Sentinel-1/2
    depuis les fichiers extraits du depot HuggingFace IGNF/TreeSatAI-Time-Series.

    Labels multi-genres ponderes -> classe dominante apres regroupement.
    """

    def __init__(self, data_dir, filenames, labels_dict, modalites=None,
                 aerial_size=256):
        """
        Args:
            data_dir: Racine des donnees extraites (contient aerial/, sentinel/)
            filenames: Liste des noms de fichiers (ex: "Fagus_sylvatica_1_29861_WEFL_NLF.tif")
            labels_dict: Dict {filename: [[genre, proportion], ...]}
            modalites: Liste des modalites a charger (defaut: ["aerial"])
            aerial_size: Taille cible pour les images aerial (defaut: 256)
        """
        self.data_dir = Path(data_dir)
        self.labels_dict = labels_dict
        self.aerial_size = aerial_size
        self.modalites = modalites or ["aerial"]

        # Filtrer les fichiers qui ont un label valide
        self.filenames = []
        self.targets = []
        for fn in filenames:
            label = self._get_label(fn)
            if label is not None:
                self.filenames.append(fn)
                self.targets.append(label)

        print(f"  {len(self.filenames)} patches charges "
              f"(modalites: {', '.join(self.modalites)})")

    def _get_label(self, filename):
        """Convertit les labels multi-genres en classe dominante regroupee."""
        if filename not in self.labels_dict:
            return None

        proportions = self.labels_dict[filename]
        # Agreger les proportions par classe regroupee
        class_proportions = [0.0] * N_CLASSES
        for genre, prop in proportions:
            if genre in GENRE_TO_CLASSE:
                class_proportions[GENRE_TO_CLASSE[genre]] += prop

        # Classe dominante
        dominant = int(np.argmax(class_proportions))
        return dominant

    def __len__(self):
        return len(self.filenames)

    def __getitem__(self, idx):
        filename = self.filenames[idx]
        label = self.targets[idx]
        stem = Path(filename).stem  # sans .tif

        inputs = {}

        # --- Aerial (RGBI, 300x300 -> 256x256) ---
        if "aerial" in self.modalites:
            aerial_path = self.data_dir / "aerial" / filename
            if aerial_path.exists():
                import rasterio
                with rasterio.open(aerial_path) as src:
                    img = src.read().astype(np.float32)  # (C, H, W)
                # Resize a aerial_size x aerial_size
                img = self._resize(img, self.aerial_size)
                img = _normaliser_optique(torch.from_numpy(img))
                inputs["aerial"] = img

        # --- Sentinel-1 et Sentinel-2 (depuis HDF5 time-series) ---
        has_sentinel = any(m.startswith("s1_") or m == "s2"
                          for m in self.modalites)
        if has_sentinel:
            h5_path = self.data_dir / "sentinel-ts" / f"{stem}.h5"
            if h5_path.exists():
                with h5py.File(h5_path, "r") as h5:
                    # S1 ascending : moyenne temporelle
                    if "s1_asc" in self.modalites and "sen-1-asc-data" in h5:
                        s1a = h5["sen-1-asc-data"][:].astype(np.float32)
                        s1a = np.nanmean(s1a, axis=0)  # (2, 6, 6)
                        inputs["s1_asc"] = _normaliser_mnt(
                            torch.from_numpy(s1a))

                    # S1 descending : moyenne temporelle
                    if "s1_des" in self.modalites and "sen-1-des-data" in h5:
                        s1d = h5["sen-1-des-data"][:].astype(np.float32)
                        s1d = np.nanmean(s1d, axis=0)  # (2, 6, 6)
                        inputs["s1_des"] = _normaliser_mnt(
                            torch.from_numpy(s1d))

                    # S2 : moyenne temporelle (masquee nuages)
                    if "s2" in self.modalites and "sen-2-data" in h5:
                        s2 = h5["sen-2-data"][:].astype(np.float32)
                        if "sen-2-masks" in h5:
                            masks = h5["sen-2-masks"][:]
                            # mask[:,0] = cloud prob, mask[:,1] = snow prob
                            cloud = masks[:, 0:1, :, :]
                            valid = (cloud < 50).astype(np.float32)
                            valid = np.broadcast_to(valid,
                                                    s2.shape)
                            s2_sum = np.nansum(s2 * valid, axis=0)
                            count = np.nansum(valid, axis=0).clip(min=1)
                            s2 = s2_sum / count
                        else:
                            s2 = np.nanmean(s2, axis=0)
                        inputs["s2"] = _normaliser_optique(
                            torch.from_numpy(s2), max_val=10000.0)

        return inputs, label

    @staticmethod
    def _resize(img, size):
        """Resize (C, H, W) numpy array vers (C, size, size) par interpolation."""
        C, H, W = img.shape
        if H == size and W == size:
            return img
        t = torch.from_numpy(img).unsqueeze(0)  # (1, C, H, W)
        t = torch.nn.functional.interpolate(
            t, size=(size, size), mode="bilinear", align_corners=False)
        return t.squeeze(0).numpy()


def collate_multimodal(batch):
    """Collate function pour batches multi-modaux.

    Regroupe les inputs par modalite et empile les labels.
    Ignore les patches sans la modalite (pas de padding).
    """
    all_inputs = {}
    labels = []

    for inputs, label in batch:
        labels.append(label)
        for mod, tensor in inputs.items():
            if mod not in all_inputs:
                all_inputs[mod] = []
            all_inputs[mod].append(tensor)

    # Stack chaque modalite separement
    stacked = {}
    for mod, tensors in all_inputs.items():
        if len(tensors) == len(batch):  # tous les patches ont cette modalite
            stacked[mod] = torch.stack(tensors)

    return stacked, torch.tensor(labels, dtype=torch.long)


# =========================================================================
# Preparation des donnees
# =========================================================================

def preparer_donnees(data_dir, cache_dir=None):
    """Telecharge et extrait les donnees TreeSatAI depuis HuggingFace.

    Args:
        data_dir: Repertoire de destination pour les donnees extraites
        cache_dir: Repertoire cache HuggingFace (optionnel)

    Returns:
        (labels_dict, train_files, val_files, test_files)
    """
    from huggingface_hub import hf_hub_download

    data_dir = Path(data_dir)
    data_dir.mkdir(parents=True, exist_ok=True)

    repo_id = "IGNF/TreeSatAI-Time-Series"

    # --- Labels ---
    labels_path = data_dir / "labels.json"
    if not labels_path.exists():
        print("Telechargement des labels...")
        zip_path = hf_hub_download(repo_id, "labels.zip", repo_type="dataset",
                                   cache_dir=cache_dir)
        with zipfile.ZipFile(zip_path) as z:
            with z.open("TreeSatBA_v9_60m_multi_labels.json") as f:
                labels = json.load(f)
        with open(labels_path, "w") as f:
            json.dump(labels, f)
    else:
        with open(labels_path) as f:
            labels = json.load(f)
    print(f"Labels: {len(labels)} patches")

    # --- Splits ---
    splits = {}
    for split_name in ["train", "val", "test"]:
        split_file = data_dir / f"{split_name}_filenames.lst"
        if not split_file.exists():
            print("Telechargement des splits...")
            zip_path = hf_hub_download(repo_id, "split.zip",
                                       repo_type="dataset",
                                       cache_dir=cache_dir)
            with zipfile.ZipFile(zip_path) as z:
                for sn in ["train", "val", "test"]:
                    with z.open(f"split/{sn}_filenames.lst") as f:
                        content = f.read().decode().strip()
                    (data_dir / f"{sn}_filenames.lst").write_text(content)
            break
        splits[split_name] = split_file.read_text().strip().split("\n")

    for split_name in ["train", "val", "test"]:
        split_file = data_dir / f"{split_name}_filenames.lst"
        splits[split_name] = split_file.read_text().strip().split("\n")

    print(f"Splits: train={len(splits['train'])}, "
          f"val={len(splits['val'])}, test={len(splits['test'])}")

    # --- Aerial ---
    aerial_dir = data_dir / "aerial"
    if not aerial_dir.exists() or len(list(aerial_dir.glob("*.tif"))) == 0:
        print("Telechargement des images aerial (~16 Go)...")
        zip_path = hf_hub_download(repo_id, "aerial.zip",
                                   repo_type="dataset",
                                   cache_dir=cache_dir)
        print("Extraction des images aerial...")
        with zipfile.ZipFile(zip_path) as z:
            z.extractall(data_dir)
        print(f"  {len(list(aerial_dir.glob('*.tif')))} images extraites")
    else:
        print(f"Aerial: {len(list(aerial_dir.glob('*.tif')))} images (cache)")

    # --- Sentinel time-series (optionnel) ---
    sentinel_ts_dir = data_dir / "sentinel-ts"
    if not sentinel_ts_dir.exists():
        print("Telechargement des series temporelles Sentinel (~6 Go)...")
        zip_path = hf_hub_download(repo_id, "sentinel-ts.zip",
                                   repo_type="dataset",
                                   cache_dir=cache_dir)
        print("Extraction des series temporelles Sentinel...")
        with zipfile.ZipFile(zip_path) as z:
            z.extractall(data_dir)
        n_h5 = len(list(sentinel_ts_dir.glob("*.h5")))
        print(f"  {n_h5} fichiers HDF5 extraits")

    return labels, splits["train"], splits["val"], splits["test"]


# =========================================================================
# Entrainement
# =========================================================================

def calculer_poids_classes(targets):
    """Calcule les poids inverses des classes pour gerer le desequilibre."""
    counts = np.bincount(targets, minlength=N_CLASSES).astype(float)
    counts = np.maximum(counts, 1.0)  # eviter division par zero
    weights = 1.0 / counts
    weights = weights / weights.sum() * N_CLASSES  # normaliser
    return torch.FloatTensor(weights)


def entrainer(args):
    """Boucle d'entrainement principale."""
    device = torch.device("cuda" if args.gpu and torch.cuda.is_available()
                          else "cpu")
    print(f"\nDevice: {device}")

    # --- Donnees ---
    print("\n=== Preparation des donnees TreeSatAI ===")
    labels, train_files, val_files, test_files = preparer_donnees(
        args.data_dir)

    modalites = args.modalites.split(",")
    print(f"Modalites: {', '.join(modalites)}")

    # Verifier que rasterio est disponible pour aerial
    if "aerial" in modalites:
        try:
            import rasterio  # noqa: F401
        except ImportError:
            print("ERREUR: rasterio requis pour les images aerial.")
            print("  pip install rasterio")
            sys.exit(1)

    train_ds = TreeSatAIDataset(
        args.data_dir, train_files, labels,
        modalites=modalites, aerial_size=256)
    val_ds = TreeSatAIDataset(
        args.data_dir, val_files, labels,
        modalites=modalites, aerial_size=256)

    train_loader = DataLoader(
        train_ds, batch_size=args.batch_size, shuffle=True,
        num_workers=args.workers, collate_fn=collate_multimodal,
        pin_memory=True, drop_last=True)
    val_loader = DataLoader(
        val_ds, batch_size=args.batch_size, shuffle=False,
        num_workers=args.workers, collate_fn=collate_multimodal,
        pin_memory=True)

    # Distribution des classes
    print("\nDistribution des classes (train):")
    for i, name in enumerate(CLASSES_TREESATAI):
        count = sum(1 for t in train_ds.targets if t == i)
        print(f"  {i} - {name}: {count} ({100*count/len(train_ds.targets):.1f}%)")

    # --- Modele ---
    print("\n=== Chargement du modele MAESTRO ===")
    mod_config = {k: MODALITIES[k] for k in modalites if k in MODALITIES}

    modele = MAESTROClassifier(
        embed_dim=768, encoder_depth=9, inter_depth=3, num_heads=12,
        n_classes=N_CLASSES, modalities=mod_config,
    )

    # Charger les poids pre-entraines du backbone
    checkpoint_path = Path(args.checkpoint)
    if not checkpoint_path.exists():
        print(f"ERREUR: Checkpoint introuvable: {checkpoint_path}")
        sys.exit(1)

    _install_maestro_stubs()
    import io
    with open(str(checkpoint_path), "rb") as f:
        buffer = io.BytesIO(f.read())
    checkpoint = torch.load(buffer, map_location="cpu", weights_only=False)

    if isinstance(checkpoint, dict):
        state_dict = checkpoint.get("state_dict",
                                    checkpoint.get("model", checkpoint))
    else:
        state_dict = checkpoint

    # Filtrer les cles du backbone (pas la tete)
    prefixes = ["model.encoder_inter."]
    for mod_name in mod_config:
        prefixes.append(f"model.patch_embed.{mod_name}.")
        enc = "s1" if mod_name.startswith("s1_") else mod_name
        prefixes.append(f"model.encoder.{enc}.")

    filtered = {k: v for k, v in state_dict.items()
                if any(k.startswith(p) for p in prefixes)}
    missing, unexpected = modele.load_state_dict(filtered, strict=False)
    head_missing = [k for k in missing if k.startswith("head.")]
    other_missing = [k for k in missing if not k.startswith("head.")]
    print(f"  Backbone charge: {len(filtered)} cles")
    print(f"  Tete de classification: {len(head_missing)} cles "
          f"(initialisation aleatoire, {N_CLASSES} classes)")
    if other_missing:
        print(f"  [ATTENTION] {len(other_missing)} cles backbone manquantes")

    # Geler le backbone si demande (defaut: gele)
    if not args.unfreeze:
        for name, param in modele.named_parameters():
            if not name.startswith("head."):
                param.requires_grad = False
        n_trainable = sum(p.numel() for p in modele.parameters()
                          if p.requires_grad)
        n_total = sum(p.numel() for p in modele.parameters())
        print(f"  Backbone GELE: {n_trainable:,} params entrainables "
              f"/ {n_total:,} total")
    else:
        n_total = sum(p.numel() for p in modele.parameters())
        print(f"  Backbone DEGELE: {n_total:,} params entrainables (fine-tuning complet)")

    modele = modele.to(device)

    # --- Optimiseur ---
    class_weights = calculer_poids_classes(np.array(train_ds.targets))
    criterion = nn.CrossEntropyLoss(weight=class_weights.to(device))

    if args.unfreeze:
        # LR differentiel : backbone faible, tete forte
        backbone_params = [p for n, p in modele.named_parameters()
                           if not n.startswith("head.") and p.requires_grad]
        head_params = [p for n, p in modele.named_parameters()
                       if n.startswith("head.")]
        optimizer = torch.optim.AdamW([
            {"params": backbone_params, "lr": args.lr * 0.1},
            {"params": head_params, "lr": args.lr},
        ], weight_decay=args.weight_decay)
    else:
        optimizer = torch.optim.AdamW(
            filter(lambda p: p.requires_grad, modele.parameters()),
            lr=args.lr, weight_decay=args.weight_decay)

    scheduler = torch.optim.lr_scheduler.CosineAnnealingLR(
        optimizer, T_max=args.epochs)

    # --- Boucle d'entrainement ---
    print(f"\n=== Entrainement ({args.epochs} epochs) ===")
    best_val_acc = 0.0
    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    for epoch in range(args.epochs):
        # --- Train ---
        modele.train()
        if not args.unfreeze:
            modele.model.eval()  # garder BatchNorm/Dropout du backbone en eval

        train_loss = 0.0
        train_correct = 0
        train_total = 0

        for batch_idx, (inputs, targets) in enumerate(train_loader):
            inputs = {k: v.to(device) for k, v in inputs.items()}
            targets = targets.to(device)

            if not inputs:
                continue

            optimizer.zero_grad()
            logits = modele(inputs)
            loss = criterion(logits, targets)
            loss.backward()
            optimizer.step()

            train_loss += loss.item() * targets.size(0)
            preds = logits.argmax(dim=1)
            train_correct += (preds == targets).sum().item()
            train_total += targets.size(0)

            if (batch_idx + 1) % 50 == 0:
                print(f"  Epoch {epoch+1}/{args.epochs} - "
                      f"batch {batch_idx+1}/{len(train_loader)} - "
                      f"loss: {loss.item():.4f}")

        train_acc = train_correct / max(train_total, 1)
        train_loss = train_loss / max(train_total, 1)

        # --- Validation ---
        modele.eval()
        val_loss = 0.0
        val_correct = 0
        val_total = 0
        class_correct = [0] * N_CLASSES
        class_total = [0] * N_CLASSES

        with torch.no_grad():
            for inputs, targets in val_loader:
                inputs = {k: v.to(device) for k, v in inputs.items()}
                targets = targets.to(device)

                if not inputs:
                    continue

                logits = modele(inputs)
                loss = criterion(logits, targets)

                val_loss += loss.item() * targets.size(0)
                preds = logits.argmax(dim=1)
                val_correct += (preds == targets).sum().item()
                val_total += targets.size(0)

                for i in range(targets.size(0)):
                    t = targets[i].item()
                    class_total[t] += 1
                    if preds[i].item() == t:
                        class_correct[t] += 1

        val_acc = val_correct / max(val_total, 1)
        val_loss = val_loss / max(val_total, 1)
        scheduler.step()

        print(f"\nEpoch {epoch+1}/{args.epochs}: "
              f"train_loss={train_loss:.4f} train_acc={train_acc:.3f} | "
              f"val_loss={val_loss:.4f} val_acc={val_acc:.3f}")

        # Accuracy par classe
        for i, name in enumerate(CLASSES_TREESATAI):
            if class_total[i] > 0:
                acc = class_correct[i] / class_total[i]
                print(f"  {name}: {acc:.3f} ({class_correct[i]}/{class_total[i]})")

        # Sauvegarder le meilleur modele
        if val_acc > best_val_acc:
            best_val_acc = val_acc
            save_path = output_dir / "maestro_treesatai_best.pt"
            torch.save({
                "epoch": epoch + 1,
                "model_state_dict": modele.state_dict(),
                "val_acc": val_acc,
                "val_loss": val_loss,
                "n_classes": N_CLASSES,
                "classes": CLASSES_TREESATAI,
                "genre_to_classe": GENRE_TO_CLASSE,
                "modalites": modalites,
            }, str(save_path))
            print(f"  -> Meilleur modele sauvegarde: {save_path} "
                  f"(val_acc={val_acc:.3f})")

    # Sauvegarder le modele final
    save_path = output_dir / "maestro_treesatai_final.pt"
    torch.save({
        "epoch": args.epochs,
        "model_state_dict": modele.state_dict(),
        "val_acc": val_acc,
        "val_loss": val_loss,
        "n_classes": N_CLASSES,
        "classes": CLASSES_TREESATAI,
        "genre_to_classe": GENRE_TO_CLASSE,
        "modalites": modalites,
    }, str(save_path))
    print(f"\nModele final sauvegarde: {save_path}")
    print(f"Meilleure validation: {best_val_acc:.3f}")


# =========================================================================
# CLI
# =========================================================================

def main():
    parser = argparse.ArgumentParser(
        description="Fine-tuning MAESTRO sur TreeSatAI (8 classes)")

    parser.add_argument("--checkpoint", required=True,
                        help="Chemin vers le checkpoint MAESTRO pre-entraine")
    parser.add_argument("--data-dir", default="data/treesatai",
                        help="Repertoire des donnees TreeSatAI (defaut: data/treesatai)")
    parser.add_argument("--output-dir", default="outputs/training",
                        help="Repertoire de sortie (defaut: outputs/training)")
    parser.add_argument("--modalites", default="aerial",
                        help="Modalites separees par virgule "
                        "(defaut: aerial). Ex: aerial,s1_asc,s1_des,s2")
    parser.add_argument("--epochs", type=int, default=30,
                        help="Nombre d'epochs (defaut: 30)")
    parser.add_argument("--batch-size", type=int, default=32,
                        help="Taille du batch (defaut: 32)")
    parser.add_argument("--lr", type=float, default=1e-3,
                        help="Learning rate (defaut: 1e-3)")
    parser.add_argument("--weight-decay", type=float, default=1e-4,
                        help="Weight decay (defaut: 1e-4)")
    parser.add_argument("--workers", type=int, default=4,
                        help="Nombre de workers DataLoader (defaut: 4)")
    parser.add_argument("--gpu", action="store_true",
                        help="Utiliser CUDA si disponible")
    parser.add_argument("--unfreeze", action="store_true",
                        help="Degeler le backbone (fine-tuning complet)")

    args = parser.parse_args()
    entrainer(args)


if __name__ == "__main__":
    main()
