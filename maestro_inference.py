"""
maestro_inference.py
Module Python d'inference pour le modele MAESTRO de l'IGNF.
Appele depuis R via reticulate.

Le modele MAESTRO est un Masked Autoencoder (MAE) base sur un Vision Transformer (ViT)
pre-entraine sur des donnees d'observation de la Terre multimodales.

Ce module fournit les fonctions pour :
  - Charger les poids pre-entraines depuis un fichier local (telecharge via hfhub)
  - Construire une tete de classification pour les essences forestieres
  - Executer l'inference sur des patches d'images
"""

import torch
import torch.nn as nn
import numpy as np
from pathlib import Path
import json


# Classes d'essences forestieres PureForest (13 classes)
ESSENCES = [
    "Chene decidue",
    "Chene vert",
    "Hetre",
    "Chataignier",
    "Pin maritime",
    "Pin sylvestre",
    "Pin laricio/noir",
    "Pin d'Alep",
    "Epicea",
    "Sapin",
    "Douglas",
    "Meleze",
    "Peuplier",
]


class PatchEmbed(nn.Module):
    """Projection des patches d'image en embeddings."""

    def __init__(self, img_size=250, patch_size=16, in_channels=4, embed_dim=768):
        super().__init__()
        self.img_size = img_size
        self.patch_size = patch_size
        self.n_patches = (img_size // patch_size) ** 2
        self.proj = nn.Conv2d(
            in_channels, embed_dim,
            kernel_size=patch_size, stride=patch_size
        )

    def forward(self, x):
        # x: (B, C, H, W) -> (B, N, D)
        x = self.proj(x)  # (B, D, H', W')
        x = x.flatten(2).transpose(1, 2)  # (B, N, D)
        return x


class MAEEncoder(nn.Module):
    """Encodeur Vision Transformer (ViT) style MAE."""

    def __init__(self, img_size=250, patch_size=16, in_channels=4,
                 embed_dim=768, depth=12, num_heads=12, mlp_ratio=4.0):
        super().__init__()

        self.patch_embed = PatchEmbed(img_size, patch_size, in_channels, embed_dim)
        n_patches = self.patch_embed.n_patches

        self.cls_token = nn.Parameter(torch.zeros(1, 1, embed_dim))
        self.pos_embed = nn.Parameter(torch.zeros(1, n_patches + 1, embed_dim))

        encoder_layer = nn.TransformerEncoderLayer(
            d_model=embed_dim,
            nhead=num_heads,
            dim_feedforward=int(embed_dim * mlp_ratio),
            activation="gelu",
            batch_first=True,
            norm_first=True,
        )
        self.transformer = nn.TransformerEncoder(encoder_layer, num_layers=depth)
        self.norm = nn.LayerNorm(embed_dim)

    def forward(self, x):
        B = x.shape[0]

        # Patch embedding
        x = self.patch_embed(x)  # (B, N, D)

        # Ajouter le token CLS
        cls_tokens = self.cls_token.expand(B, -1, -1)
        x = torch.cat([cls_tokens, x], dim=1)  # (B, N+1, D)

        # Ajouter le positional embedding
        x = x + self.pos_embed

        # Transformer
        x = self.transformer(x)
        x = self.norm(x)

        return x


class MAESTROClassifier(nn.Module):
    """
    Classificateur d'essences forestieres base sur MAESTRO.
    Utilise l'encodeur MAE/ViT avec une tete de classification.
    """

    def __init__(self, img_size=250, patch_size=16, in_channels=4,
                 embed_dim=768, depth=12, num_heads=12, n_classes=13):
        super().__init__()

        self.encoder = MAEEncoder(
            img_size=img_size,
            patch_size=patch_size,
            in_channels=in_channels,
            embed_dim=embed_dim,
            depth=depth,
            num_heads=num_heads,
        )

        # Tete de classification sur le token CLS
        self.head = nn.Sequential(
            nn.Linear(embed_dim, 256),
            nn.GELU(),
            nn.Dropout(0.1),
            nn.Linear(256, n_classes),
        )

    def forward(self, x):
        # x: (B, C, H, W)
        features = self.encoder(x)  # (B, N+1, D)
        cls_token = features[:, 0]  # (B, D)
        logits = self.head(cls_token)  # (B, n_classes)
        return logits


def charger_modele(chemin_poids, n_classes=13, device="cpu"):
    """
    Charge le modele MAESTRO avec les poids pre-entraines.

    Args:
        chemin_poids: Chemin vers le fichier de poids (.pt, .pth, .bin, .safetensors)
        n_classes: Nombre de classes de sortie (13 pour PureForest)
        device: 'cpu' ou 'cuda'

    Returns:
        Modele PyTorch en mode evaluation
    """
    chemin = Path(chemin_poids)
    device = torch.device(device)

    # Determiner la configuration du modele
    config_path = chemin.parent / "config.json"
    if config_path.exists():
        with open(config_path) as f:
            config = json.load(f)
        embed_dim = config.get("hidden_size", config.get("embed_dim", 768))
        depth = config.get("num_hidden_layers", config.get("depth", 12))
        num_heads = config.get("num_attention_heads", config.get("num_heads", 12))
        patch_size = config.get("patch_size", 16)
        in_channels = config.get("num_channels", config.get("in_channels", 4))
        img_size = config.get("image_size", config.get("img_size", 250))
    else:
        # Configuration par defaut (ViT-Base)
        embed_dim = 768
        depth = 12
        num_heads = 12
        patch_size = 16
        in_channels = 4
        img_size = 250

    # Creer le modele
    modele = MAESTROClassifier(
        img_size=img_size,
        patch_size=patch_size,
        in_channels=in_channels,
        embed_dim=embed_dim,
        depth=depth,
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
                "Le package 'safetensors' est requis pour charger les fichiers .safetensors. "
                "Installez-le avec : pip install safetensors"
            )
    else:
        state_dict = torch.load(str(chemin), map_location=device, weights_only=False)

    # Gerer les checkpoints qui encapsulent le state_dict
    if "state_dict" in state_dict:
        state_dict = state_dict["state_dict"]
    elif "model" in state_dict:
        state_dict = state_dict["model"]
    elif "model_state_dict" in state_dict:
        state_dict = state_dict["model_state_dict"]

    # Charger les poids (mode non strict pour gerer les differences d'architecture)
    missing, unexpected = modele.load_state_dict(state_dict, strict=False)
    if missing:
        print(f"  [INFO] Cles manquantes (attendu pour fine-tuning) : {len(missing)}")
    if unexpected:
        print(f"  [INFO] Cles inattendues (ignorees) : {len(unexpected)}")

    modele = modele.to(device)
    modele.eval()
    print(f"  Modele charge sur {device} ({sum(p.numel() for p in modele.parameters()):,} parametres)")

    return modele


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

    # Preparer l'image
    if isinstance(image_np, np.ndarray):
        img = torch.from_numpy(image_np.copy()).float()
    else:
        img = image_np.float()

    # Si (H, W, C) -> (C, H, W)
    if img.dim() == 3 and img.shape[-1] <= 4:
        img = img.permute(2, 0, 1)

    # Ajouter la dimension batch
    if img.dim() == 3:
        img = img.unsqueeze(0)

    # Normaliser [0, 255] -> [0, 1]
    if img.max() > 1.0:
        img = img / 255.0

    img = img.to(device)

    # Inference
    with torch.no_grad():
        logits = modele(img)
        probs = torch.softmax(logits, dim=1)
        classe = torch.argmax(probs, dim=1).item()

    return {
        "classe": classe,
        "essence": ESSENCES[classe] if classe < len(ESSENCES) else f"Classe_{classe}",
        "probabilites": probs.cpu().numpy().flatten().tolist(),
    }


def predire_batch(modele, images_np, device="cpu"):
    """
    Predit les essences forestieres pour un batch de patches.

    Args:
        modele: Modele MAESTROClassifier
        images_np: Array numpy (B, H, W, C) ou (B, C, H, W), valeurs 0-255
        device: 'cpu' ou 'cuda'

    Returns:
        Liste de predictions (codes de classes)
    """
    device = torch.device(device)

    batch = torch.from_numpy(np.array(images_np).copy()).float()

    # Si (B, H, W, C) -> (B, C, H, W)
    if batch.dim() == 4 and batch.shape[-1] <= 4:
        batch = batch.permute(0, 3, 1, 2)

    # Normaliser
    if batch.max() > 1.0:
        batch = batch / 255.0

    batch = batch.to(device)

    with torch.no_grad():
        logits = modele(batch)
        preds = torch.argmax(logits, dim=1).cpu().numpy()

    return preds.tolist()
