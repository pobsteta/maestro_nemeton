#!/usr/bin/env python3
"""
train_segmentation.py
Entrainement du decodeur de segmentation MAESTRO sur des patches
FLAIR-HUB + labels BD Foret V2 rasterises (classes NDP0).

Le backbone MAESTRO est gele. Seul le decodeur convolutionnel est entraine.

Usage:
    python train_segmentation.py \
        --checkpoint /path/to/MAESTRO_pretrain.ckpt \
        --data-dir /path/to/training_patches/ \
        --output-dir outputs/segmentation/ \
        --epochs 50 --batch-size 8 --lr 1e-3 --gpu
"""

import argparse
import sys
import time
from pathlib import Path

import numpy as np
import torch
import torch.nn as nn
import torch.nn.functional as F
from torch.utils.data import Dataset, DataLoader

# Ajouter le repertoire courant pour les imports locaux
sys.path.insert(0, str(Path(__file__).parent))

from maestro_inference import (
    charger_modele, _normaliser_optique, _normaliser_mnt, MODALITIES
)
from maestro_segmentation import (
    creer_segmenter, sauvegarder_segmenter, CLASSES_NDP0
)


# ---------------------------------------------------------------------------
# Dataset
# ---------------------------------------------------------------------------

class SegmentationDataset(Dataset):
    """Dataset de patches multimodaux + masques NDP0 pour la segmentation.

    Structure attendue :
        data_dir/
            aerial/       *.tif (4 bandes, 250x250)
            dem/          *.tif (2 bandes, 250x250) [optionnel]
            s2/           *.tif (10 bandes, 5x5)    [optionnel]
            s1_asc/       *.tif (2 bandes, 5x5)     [optionnel]
            s1_des/       *.tif (2 bandes, 5x5)     [optionnel]
            labels/       *.tif (1 bande, 250x250, codes NDP0 0-9)

    Les fichiers doivent avoir le meme nom de base dans chaque dossier.
    """

    def __init__(self, data_dir, split="train", modalities=None,
                 patch_size=250):
        self.data_dir = Path(data_dir) / split
        self.patch_size = patch_size
        self.modalities = modalities or ["aerial"]

        # Lister les patches disponibles (basé sur aerial/)
        aerial_dir = self.data_dir / "aerial"
        if not aerial_dir.exists():
            raise FileNotFoundError("Dossier aerial/ introuvable: %s" % aerial_dir)

        self.patch_ids = sorted([
            p.stem for p in aerial_dir.glob("*.tif")
        ])

        # Verifier que les labels existent
        label_dir = self.data_dir / "labels"
        if not label_dir.exists():
            raise FileNotFoundError("Dossier labels/ introuvable: %s" % label_dir)

        # Filtrer les patches qui ont un label
        self.patch_ids = [
            pid for pid in self.patch_ids
            if (label_dir / (pid + ".tif")).exists()
        ]

        print("  Dataset %s: %d patches, modalites: %s" % (
            split, len(self.patch_ids), ", ".join(self.modalities)))

    def __len__(self):
        return len(self.patch_ids)

    def __getitem__(self, idx):
        pid = self.patch_ids[idx]
        inputs = {}

        for mod in self.modalities:
            tif_path = self.data_dir / mod / (pid + ".tif")
            if not tif_path.exists():
                continue

            # Lire le .tif avec rasterio
            import rasterio
            with rasterio.open(str(tif_path)) as src:
                data = src.read().astype(np.float32)  # (C, H, W)

            inputs[mod] = data

        # Lire le label
        label_path = self.data_dir / "labels" / (pid + ".tif")
        with rasterio.open(str(label_path)) as src:
            label = src.read(1).astype(np.int64)  # (H, W)

        # Normalisation
        for mod in list(inputs.keys()):
            t = torch.from_numpy(inputs[mod])
            if mod in ("aerial", "spot"):
                t = _normaliser_optique(t)
            elif mod == "dem":
                t = _normaliser_mnt(t)
            elif mod.startswith("s1_"):
                t = _normaliser_mnt(t)
            elif mod == "s2":
                t = _normaliser_optique(t, max_val=10000.0)
            inputs[mod] = t

        label = torch.from_numpy(label)

        return inputs, label


def collate_multimodal(batch):
    """Collate function pour batchs multi-modaux."""
    inputs_list, labels_list = zip(*batch)

    # Empiler les labels
    labels = torch.stack(labels_list)

    # Empiler chaque modalite
    all_mods = set()
    for inp in inputs_list:
        all_mods.update(inp.keys())

    batched_inputs = {}
    for mod in all_mods:
        mod_tensors = [inp[mod] for inp in inputs_list if mod in inp]
        if len(mod_tensors) == len(inputs_list):
            batched_inputs[mod] = torch.stack(mod_tensors)

    return batched_inputs, labels


# ---------------------------------------------------------------------------
# Entrainement
# ---------------------------------------------------------------------------

def compute_class_weights(dataset, n_classes=10, device="cpu"):
    """Calcule les poids de classe inverses pour gerer le desequilibre."""
    print("  Calcul des poids de classe...")
    counts = torch.zeros(n_classes, dtype=torch.float32)

    # Echantillonner un sous-ensemble pour les grands datasets
    n_sample = min(len(dataset), 500)
    indices = np.random.choice(len(dataset), n_sample, replace=False)

    for i in indices:
        _, label = dataset[i]
        for c in range(n_classes):
            counts[c] += (label == c).sum().float()

    # Poids inverse de la frequence (avec lissage)
    total = counts.sum()
    weights = total / (n_classes * counts.clamp(min=1.0))
    # Limiter les poids extremes
    weights = weights.clamp(max=20.0)

    for c in range(n_classes):
        print("    %d - %s: %.1f%% (poids: %.2f)" % (
            c, CLASSES_NDP0[c] if c < len(CLASSES_NDP0) else "?",
            counts[c] / total * 100, weights[c]))

    return weights.to(device)


def train_one_epoch(segmenter, dataloader, optimizer, criterion, device,
                    epoch, n_epochs):
    """Entraine le decodeur pour une epoch."""
    segmenter.decoder.train()
    running_loss = 0.0
    n_correct = 0
    n_total = 0

    for batch_idx, (inputs, labels) in enumerate(dataloader):
        # Deplacer sur device
        inputs = {k: v.to(device) for k, v in inputs.items()}
        labels = labels.to(device)

        # Forward
        logits = segmenter(inputs)  # (B, C, H, W)

        # Ajuster la taille si necessaire
        if logits.shape[2:] != labels.shape[1:]:
            logits = F.interpolate(logits, size=labels.shape[1:],
                                   mode="bilinear", align_corners=False)

        loss = criterion(logits, labels)

        # Backward
        optimizer.zero_grad()
        loss.backward()
        optimizer.step()

        running_loss += loss.item()

        # Accuracy
        preds = logits.argmax(dim=1)
        n_correct += (preds == labels).sum().item()
        n_total += labels.numel()

        if (batch_idx + 1) % 20 == 0:
            print("    Epoch %d/%d, batch %d/%d, loss: %.4f" % (
                epoch + 1, n_epochs, batch_idx + 1, len(dataloader),
                loss.item()))

    avg_loss = running_loss / len(dataloader)
    acc = n_correct / n_total * 100
    return avg_loss, acc


@torch.no_grad()
def evaluate(segmenter, dataloader, criterion, device, n_classes=10):
    """Evalue le decodeur sur un dataset de validation."""
    segmenter.eval()
    running_loss = 0.0
    confusion = torch.zeros(n_classes, n_classes, dtype=torch.long)

    for inputs, labels in dataloader:
        inputs = {k: v.to(device) for k, v in inputs.items()}
        labels = labels.to(device)

        logits = segmenter(inputs)
        if logits.shape[2:] != labels.shape[1:]:
            logits = F.interpolate(logits, size=labels.shape[1:],
                                   mode="bilinear", align_corners=False)

        loss = criterion(logits, labels)
        running_loss += loss.item()

        preds = logits.argmax(dim=1)
        for c_true in range(n_classes):
            for c_pred in range(n_classes):
                confusion[c_true, c_pred] += (
                    (labels == c_true) & (preds == c_pred)
                ).sum().item()

    avg_loss = running_loss / max(len(dataloader), 1)

    # Metriques par classe
    iou_per_class = []
    for c in range(n_classes):
        tp = confusion[c, c].item()
        fp = confusion[:, c].sum().item() - tp
        fn = confusion[c, :].sum().item() - tp
        iou = tp / max(tp + fp + fn, 1)
        iou_per_class.append(iou)

    miou = np.mean(iou_per_class)
    acc = confusion.diag().sum().item() / max(confusion.sum().item(), 1) * 100

    return avg_loss, acc, miou, iou_per_class


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="Entrainement du decodeur de segmentation MAESTRO")
    parser.add_argument("--checkpoint", required=True,
                        help="Checkpoint MAESTRO pre-entraine (.ckpt)")
    parser.add_argument("--data-dir", required=True,
                        help="Repertoire des patches d'entrainement")
    parser.add_argument("--output-dir", default="outputs/segmentation",
                        help="Repertoire de sortie")
    parser.add_argument("--n-classes", type=int, default=10,
                        help="Nombre de classes (10 pour NDP0)")
    parser.add_argument("--modalites", default="aerial",
                        help="Modalites separees par virgule")
    parser.add_argument("--epochs", type=int, default=50)
    parser.add_argument("--batch-size", type=int, default=8)
    parser.add_argument("--lr", type=float, default=1e-3)
    parser.add_argument("--patience", type=int, default=10,
                        help="Early stopping patience")
    parser.add_argument("--workers", type=int, default=4)
    parser.add_argument("--gpu", action="store_true")
    args = parser.parse_args()

    device = torch.device("cuda" if args.gpu and torch.cuda.is_available()
                          else "cpu")
    modalities = [m.strip() for m in args.modalites.split(",")]

    print("=" * 60)
    print(" MAESTRO - Entrainement decodeur segmentation (NDP0)")
    print("=" * 60)
    print("  Checkpoint: %s" % args.checkpoint)
    print("  Data: %s" % args.data_dir)
    print("  Modalites: %s" % ", ".join(modalities))
    print("  Classes: %d" % args.n_classes)
    print("  Device: %s" % device)
    print()

    # 1. Charger le backbone MAESTRO
    print("=== Chargement backbone MAESTRO ===")
    backbone = charger_modele(
        args.checkpoint,
        n_classes=13,  # pas important, on n'utilise pas la tete
        device=str(device),
        modalites=modalities,
    )

    # 2. Creer le segmenter
    print("\n=== Creation du segmenter ===")
    segmenter = creer_segmenter(
        backbone,
        n_classes=args.n_classes,
        freeze_backbone=True,
    )
    segmenter = segmenter.to(device)

    # 3. Datasets
    print("\n=== Chargement des donnees ===")
    train_ds = SegmentationDataset(args.data_dir, split="train",
                                   modalities=modalities)
    val_ds = SegmentationDataset(args.data_dir, split="val",
                                 modalities=modalities)

    train_loader = DataLoader(train_ds, batch_size=args.batch_size,
                              shuffle=True, num_workers=args.workers,
                              collate_fn=collate_multimodal,
                              pin_memory=device.type == "cuda")
    val_loader = DataLoader(val_ds, batch_size=args.batch_size,
                            shuffle=False, num_workers=args.workers,
                            collate_fn=collate_multimodal,
                            pin_memory=device.type == "cuda")

    # 4. Poids de classe et loss
    class_weights = compute_class_weights(train_ds, args.n_classes, device)
    criterion = nn.CrossEntropyLoss(weight=class_weights, ignore_index=255)

    # 5. Optimiseur (seulement les params du decodeur)
    optimizer = torch.optim.AdamW(
        segmenter.decoder.parameters(),
        lr=args.lr,
        weight_decay=1e-4,
    )
    scheduler = torch.optim.lr_scheduler.CosineAnnealingLR(
        optimizer, T_max=args.epochs, eta_min=1e-6)

    # 6. Entrainement
    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    best_miou = 0.0
    best_epoch = 0
    patience_counter = 0

    print("\n=== Entrainement ===")
    t_start = time.time()

    for epoch in range(args.epochs):
        t_epoch = time.time()

        # Train
        train_loss, train_acc = train_one_epoch(
            segmenter, train_loader, optimizer, criterion, device,
            epoch, args.epochs)

        # Eval
        val_loss, val_acc, val_miou, iou_per_class = evaluate(
            segmenter, val_loader, criterion, device, args.n_classes)

        scheduler.step()
        lr_current = optimizer.param_groups[0]["lr"]
        dt = time.time() - t_epoch

        print("  Epoch %d/%d (%.0fs) - train_loss: %.4f, train_acc: %.1f%% | "
              "val_loss: %.4f, val_acc: %.1f%%, mIoU: %.3f, lr: %.1e" % (
                  epoch + 1, args.epochs, dt,
                  train_loss, train_acc,
                  val_loss, val_acc, val_miou, lr_current))

        # IoU par classe
        for c in range(args.n_classes):
            name = CLASSES_NDP0[c] if c < len(CLASSES_NDP0) else "?"
            print("    %d-%s: IoU=%.3f" % (c, name, iou_per_class[c]))

        # Sauvegarde du meilleur modele
        if val_miou > best_miou:
            best_miou = val_miou
            best_epoch = epoch + 1
            patience_counter = 0
            best_path = str(output_dir / "segmenter_ndp0_best.pt")
            sauvegarder_segmenter(segmenter, best_path, args.n_classes)
            print("  ** Nouveau meilleur mIoU: %.3f (epoch %d) **" % (
                best_miou, best_epoch))
        else:
            patience_counter += 1

        # Checkpoint periodique
        if (epoch + 1) % 10 == 0:
            ckpt_path = str(output_dir / ("segmenter_ndp0_epoch%d.pt" %
                                          (epoch + 1)))
            sauvegarder_segmenter(segmenter, ckpt_path, args.n_classes)

        # Early stopping
        if patience_counter >= args.patience:
            print("  Early stopping apres %d epochs sans amelioration" %
                  args.patience)
            break

    total_time = time.time() - t_start
    print("\n" + "=" * 60)
    print(" Entrainement termine ! (%.0f min)" % (total_time / 60))
    print(" Meilleur mIoU: %.3f (epoch %d)" % (best_miou, best_epoch))
    print(" Decodeur: %s" % (output_dir / "segmenter_ndp0_best.pt"))
    print("=" * 60)


if __name__ == "__main__":
    main()
