"""
flair_inference.py
Module Python d'inference pour les modeles FLAIR-HUB de l'IGNF.
Appele depuis R via reticulate.

Supporte la segmentation semantique pixel-a-pixel avec les architectures :
  - ResNet34 + UNet (FLAIR-INC)
  - ConvNeXTV2 + UPerNet (FLAIR-HUB)

Les modeles produisent des cartes de classification par patch (512x512 pixels).
"""

import torch
import torch.nn as nn
import torch.nn.functional as F
import numpy as np
from pathlib import Path


# ---------------------------------------------------------------------------
# Classes d'occupation du sol CoSIA (19 classes)
# ---------------------------------------------------------------------------

COSIA_CLASSES = [
    "Batiment",              # 1
    "Zone permeable",        # 2
    "Zone impermeable",      # 3
    "Sol nu",                # 4
    "Eau",                   # 5
    "Conifere",              # 6
    "Feuillu",               # 7
    "Broussaille / lande",   # 8
    "Vigne",                 # 9
    "Pelouse / prairie",     # 10
    "Culture",               # 11
    "Terre labouree",        # 12
    "Serre / bache",         # 13
    "Piscine",               # 14
    "Neige",                 # 15
    "Coupe forestiere",      # 16
    "Mixte conifere+feuillu",# 17
    "Ligneux",               # 18
    "Verger",                # 19
]

# Statistiques de normalisation (moyennes et ecarts-types des bandes RGBI)
FLAIR_MEAN = [0.485, 0.456, 0.406, 0.5]
FLAIR_STD = [0.229, 0.224, 0.225, 0.25]


# ---------------------------------------------------------------------------
# Modele de segmentation simplifie (UNet-like)
# ---------------------------------------------------------------------------

class ConvBlock(nn.Module):
    """Double convolution avec BatchNorm et ReLU."""

    def __init__(self, in_ch, out_ch):
        super().__init__()
        self.block = nn.Sequential(
            nn.Conv2d(in_ch, out_ch, 3, padding=1, bias=False),
            nn.BatchNorm2d(out_ch),
            nn.ReLU(inplace=True),
            nn.Conv2d(out_ch, out_ch, 3, padding=1, bias=False),
            nn.BatchNorm2d(out_ch),
            nn.ReLU(inplace=True),
        )

    def forward(self, x):
        return self.block(x)


class SimpleUNet(nn.Module):
    """UNet simplifie pour la segmentation semantique.

    Utilise comme fallback quand les architectures avancees
    (segmentation_models_pytorch) ne sont pas disponibles.
    """

    def __init__(self, in_channels=4, n_classes=19):
        super().__init__()
        # Encoder
        self.enc1 = ConvBlock(in_channels, 64)
        self.enc2 = ConvBlock(64, 128)
        self.enc3 = ConvBlock(128, 256)
        self.enc4 = ConvBlock(256, 512)
        self.pool = nn.MaxPool2d(2)

        # Bottleneck
        self.bottleneck = ConvBlock(512, 1024)

        # Decoder
        self.up4 = nn.ConvTranspose2d(1024, 512, 2, stride=2)
        self.dec4 = ConvBlock(1024, 512)
        self.up3 = nn.ConvTranspose2d(512, 256, 2, stride=2)
        self.dec3 = ConvBlock(512, 256)
        self.up2 = nn.ConvTranspose2d(256, 128, 2, stride=2)
        self.dec2 = ConvBlock(256, 128)
        self.up1 = nn.ConvTranspose2d(128, 64, 2, stride=2)
        self.dec1 = ConvBlock(128, 64)

        # Classification head
        self.head = nn.Conv2d(64, n_classes, 1)

    def forward(self, x):
        e1 = self.enc1(x)
        e2 = self.enc2(self.pool(e1))
        e3 = self.enc3(self.pool(e2))
        e4 = self.enc4(self.pool(e3))

        b = self.bottleneck(self.pool(e4))

        d4 = self.dec4(torch.cat([self.up4(b), e4], dim=1))
        d3 = self.dec3(torch.cat([self.up3(d4), e3], dim=1))
        d2 = self.dec2(torch.cat([self.up2(d3), e2], dim=1))
        d1 = self.dec1(torch.cat([self.up1(d2), e1], dim=1))

        return self.head(d1)


# ---------------------------------------------------------------------------
# Chargement du modele FLAIR
# ---------------------------------------------------------------------------

def _try_load_smp_model(encoder_name, decoder_type, in_channels, n_classes):
    """Tente de charger un modele segmentation_models_pytorch."""
    try:
        import segmentation_models_pytorch as smp
    except ImportError:
        return None

    if decoder_type == "unet":
        model = smp.Unet(
            encoder_name=encoder_name,
            in_channels=in_channels,
            classes=n_classes,
            encoder_weights=None,
        )
    elif decoder_type == "upernet":
        model = smp.UPerNet(
            encoder_name=encoder_name,
            in_channels=in_channels,
            classes=n_classes,
            encoder_weights=None,
        )
    else:
        return None

    return model


def charger_modele_flair(chemin_poids, n_classes=19, in_channels=4,
                          encoder="resnet34", decoder="unet", device="cpu"):
    """
    Charge un modele FLAIR pour la segmentation.

    Args:
        chemin_poids: Chemin vers le fichier de poids
        n_classes: Nombre de classes (19 pour CoSIA, 23 pour LPIS)
        in_channels: Nombre de canaux d'entree (4 = RGBI, 5 = RGBI+DEM)
        encoder: Architecture de l'encodeur (resnet34, convnextv2_nano, etc.)
        decoder: Architecture du decodeur (unet, upernet)
        device: 'cpu' ou 'cuda'

    Returns:
        Modele PyTorch en mode evaluation
    """
    chemin = Path(chemin_poids)
    device = torch.device(device)

    print("  Chargement modele FLAIR: %s + %s" % (encoder, decoder))
    print("  Entrees: %d canaux, Sorties: %d classes" % (in_channels, n_classes))

    # Essayer d'abord avec segmentation_models_pytorch
    model = _try_load_smp_model(encoder, decoder, in_channels, n_classes)

    if model is None:
        print("  [INFO] segmentation_models_pytorch non disponible, "
              "utilisation du UNet simplifie")
        model = SimpleUNet(in_channels=in_channels, n_classes=n_classes)

    # Charger les poids
    if chemin.suffix == ".safetensors":
        from safetensors.torch import load_file
        state_dict = load_file(str(chemin), device=str(device))
    else:
        checkpoint = torch.load(str(chemin), map_location=device,
                                weights_only=False)
        if isinstance(checkpoint, dict):
            if "state_dict" in checkpoint:
                state_dict = checkpoint["state_dict"]
            elif "model_state_dict" in checkpoint:
                state_dict = checkpoint["model_state_dict"]
            elif "model" in checkpoint:
                state_dict = checkpoint["model"]
            else:
                state_dict = checkpoint
        else:
            state_dict = checkpoint

    # Nettoyer les prefixes courants
    cleaned = {}
    for k, v in state_dict.items():
        # Supprimer les prefixes PyTorch Lightning
        for prefix in ["model.", "net.", "module."]:
            if k.startswith(prefix):
                k = k[len(prefix):]
                break
        cleaned[k] = v

    missing, unexpected = model.load_state_dict(cleaned, strict=False)
    if missing:
        print("  [INFO] Cles manquantes: %d" % len(missing))
    if unexpected:
        print("  [INFO] Cles inattendues: %d" % len(unexpected))

    model = model.to(device)
    model.eval()

    n_params = sum(p.numel() for p in model.parameters())
    print("  Modele charge sur %s (%s parametres)" % (
        device, format(n_params, ",")))

    return model


# ---------------------------------------------------------------------------
# Normalisation
# ---------------------------------------------------------------------------

def _normaliser_rgbi(img, mean=None, std=None):
    """Normalise un batch RGBI avec mean/std."""
    if mean is None:
        n_ch = img.shape[1]
        mean = FLAIR_MEAN[:n_ch]
        std = FLAIR_STD[:n_ch]

    # img: (B, C, H, W)
    if img.max() > 1.0:
        img = img / 255.0

    mean_t = torch.tensor(mean, dtype=img.dtype, device=img.device)
    std_t = torch.tensor(std, dtype=img.dtype, device=img.device)
    mean_t = mean_t.view(1, -1, 1, 1)
    std_t = std_t.view(1, -1, 1, 1)

    return (img - mean_t) / (std_t + 1e-8)


def _normaliser_dem(dem):
    """Normalise le DEM en min-max par batch."""
    dmin = dem.min()
    dmax = dem.max()
    if dmax > dmin:
        return (dem - dmin) / (dmax - dmin)
    return torch.zeros_like(dem)


# ---------------------------------------------------------------------------
# Fenetre de Hann 2D pour le blending des patches
# ---------------------------------------------------------------------------

def creer_fenetre_hann(taille, device="cpu"):
    """
    Cree une fenetre de Hann 2D pour le blending sans couture des patches.

    Args:
        taille: Taille du patch (int ou tuple)
        device: 'cpu' ou 'cuda'

    Returns:
        Tensor (1, 1, H, W) avec les poids de la fenetre de Hann
    """
    if isinstance(taille, int):
        taille = (taille, taille)

    h_win = torch.hann_window(taille[0], periodic=False, device=device)
    w_win = torch.hann_window(taille[1], periodic=False, device=device)
    window = h_win.unsqueeze(1) * w_win.unsqueeze(0)  # (H, W)

    # Eviter les zeros aux bords
    window = window.clamp(min=1e-6)

    return window.unsqueeze(0).unsqueeze(0)  # (1, 1, H, W)


# ---------------------------------------------------------------------------
# Inference par patch
# ---------------------------------------------------------------------------

def predire_patch_segmentation(modele, image_np, device="cpu"):
    """
    Predit la carte de segmentation pour un patch.

    Args:
        modele: Modele de segmentation
        image_np: Array numpy (H, W, C) ou (C, H, W) ou (B, C, H, W)
        device: 'cpu' ou 'cuda'

    Returns:
        dict avec 'classes' (H, W array), 'probabilites' (C, H, W array)
    """
    device_t = torch.device(device)

    arr = np.array(image_np, dtype=np.float32)

    # Normaliser les dimensions
    if arr.ndim == 3:
        if arr.shape[-1] <= 10:  # (H, W, C) -> (C, H, W)
            arr = np.transpose(arr, (2, 0, 1))
        arr = arr[np.newaxis]  # (1, C, H, W)

    n_ch = arr.shape[1]
    t = torch.from_numpy(arr).float()

    # Normalisation
    if n_ch <= 4:
        t = _normaliser_rgbi(t)
    elif n_ch == 5:
        t_rgbi = _normaliser_rgbi(t[:, :4])
        t_dem = _normaliser_dem(t[:, 4:5])
        t = torch.cat([t_rgbi, t_dem], dim=1)

    t = t.to(device_t)

    with torch.no_grad():
        logits = modele(t)  # (B, C, H, W)
        probs = F.softmax(logits, dim=1)
        classes = torch.argmax(probs, dim=1)  # (B, H, W)

    return {
        "classes": classes.squeeze(0).cpu().numpy(),
        "probabilites": probs.squeeze(0).cpu().numpy(),
    }


def predire_batch_segmentation(modele, batch_np, device="cpu"):
    """
    Predit la segmentation pour un batch de patches.

    Args:
        modele: Modele de segmentation
        batch_np: Array numpy (B, C, H, W), valeurs 0-255
        device: 'cpu' ou 'cuda'

    Returns:
        dict avec 'classes' (B, H, W array), 'logits' (B, C, H, W array)
    """
    device_t = torch.device(device)
    arr = np.array(batch_np, dtype=np.float32)
    n_ch = arr.shape[1]

    t = torch.from_numpy(arr).float()

    if n_ch <= 4:
        t = _normaliser_rgbi(t)
    elif n_ch == 5:
        t_rgbi = _normaliser_rgbi(t[:, :4])
        t_dem = _normaliser_dem(t[:, 4:5])
        t = torch.cat([t_rgbi, t_dem], dim=1)

    t = t.to(device_t)

    with torch.no_grad():
        logits = modele(t)
        classes = torch.argmax(logits, dim=1)

    return {
        "classes": classes.cpu().numpy(),
        "logits": logits.cpu().numpy(),
    }


# ---------------------------------------------------------------------------
# Inference avec blending (fenetre de Hann)
# ---------------------------------------------------------------------------

def predire_raster_complet(modele, image_np, patch_size=512, overlap=128,
                            n_classes=19, device="cpu", batch_size=4):
    """
    Predit la carte de segmentation pour un raster complet avec blending.

    Decoupe l'image en patches avec overlap, applique une fenetre de Hann
    pour le blending, et reassemble la carte complete.

    Args:
        modele: Modele de segmentation
        image_np: Array numpy (C, H, W) ou (H, W, C)
        patch_size: Taille des patches (defaut: 512)
        overlap: Recouvrement entre patches (defaut: 128)
        n_classes: Nombre de classes (defaut: 19)
        device: 'cpu' ou 'cuda'
        batch_size: Taille des batchs (defaut: 4)

    Returns:
        Array numpy (H, W) avec les classes predites
    """
    device_t = torch.device(device)
    arr = np.array(image_np, dtype=np.float32)

    # Normaliser les dimensions -> (C, H, W)
    if arr.ndim == 3 and arr.shape[-1] <= 10:
        arr = np.transpose(arr, (2, 0, 1))

    C, H, W = arr.shape
    stride = patch_size - overlap

    # Creer les buffers de sortie
    output_sum = np.zeros((n_classes, H, W), dtype=np.float32)
    weight_sum = np.zeros((1, H, W), dtype=np.float32)

    # Fenetre de Hann
    hann = creer_fenetre_hann(patch_size, device=str(device_t))
    hann_np = hann.squeeze().cpu().numpy()

    # Generer les positions des patches
    positions = []
    for y in range(0, H - patch_size + 1, stride):
        for x in range(0, W - patch_size + 1, stride):
            positions.append((y, x))

    # Ajouter les bords si necessaire
    if H > patch_size and (H - patch_size) % stride != 0:
        for x in range(0, W - patch_size + 1, stride):
            positions.append((H - patch_size, x))
    if W > patch_size and (W - patch_size) % stride != 0:
        for y in range(0, H - patch_size + 1, stride):
            positions.append((y, W - patch_size))
    if H > patch_size and W > patch_size:
        if (H - patch_size) % stride != 0 and (W - patch_size) % stride != 0:
            positions.append((H - patch_size, W - patch_size))

    # Dedupliquer
    positions = list(set(positions))

    n_patches = len(positions)
    print("  Inference: %d patches (%dx%d, overlap %d)" % (
        n_patches, patch_size, patch_size, overlap))

    # Traiter par batch
    for i in range(0, n_patches, batch_size):
        batch_pos = positions[i:i + batch_size]
        batch = np.stack([
            arr[:, y:y + patch_size, x:x + patch_size]
            for y, x in batch_pos
        ])

        t = torch.from_numpy(batch).float()
        n_ch = t.shape[1]

        if n_ch <= 4:
            t = _normaliser_rgbi(t)
        elif n_ch == 5:
            t_rgbi = _normaliser_rgbi(t[:, :4])
            t_dem = _normaliser_dem(t[:, 4:5])
            t = torch.cat([t_rgbi, t_dem], dim=1)

        t = t.to(device_t)

        with torch.no_grad():
            logits = modele(t)
            probs = F.softmax(logits, dim=1).cpu().numpy()

        for j, (y, x) in enumerate(batch_pos):
            output_sum[:, y:y + patch_size, x:x + patch_size] += \
                probs[j] * hann_np
            weight_sum[:, y:y + patch_size, x:x + patch_size] += hann_np

        if (i // batch_size) % 10 == 0:
            print("  Batch %d / %d" % (i // batch_size + 1,
                                        (n_patches + batch_size - 1) // batch_size))

    # Normaliser par les poids
    weight_sum = np.maximum(weight_sum, 1e-8)
    output_norm = output_sum / weight_sum

    # Classe finale
    classes = np.argmax(output_norm, axis=0)  # (H, W)

    return classes


# ---------------------------------------------------------------------------
# Classification spectrale de fallback
# ---------------------------------------------------------------------------

def classification_spectrale(image_np, n_classes=19):
    """
    Classification simple basee sur les indices spectraux.
    Utilisee comme fallback quand le modele ne charge pas.

    Args:
        image_np: Array numpy (C, H, W) avec bandes RGBI
        n_classes: Nombre de classes

    Returns:
        Array numpy (H, W) avec les classes predites
    """
    arr = np.array(image_np, dtype=np.float32)
    if arr.ndim == 3 and arr.shape[-1] <= 10:
        arr = np.transpose(arr, (2, 0, 1))

    C, H, W = arr.shape

    result = np.zeros((H, W), dtype=np.int64)

    if C >= 4:
        rouge = arr[0].astype(np.float64)
        vert = arr[1].astype(np.float64)
        bleu = arr[2].astype(np.float64)
        pir = arr[3].astype(np.float64)

        ndvi = (pir - rouge) / (pir + rouge + 1e-8)
        brightness = (rouge + vert + bleu) / 3.0

        # Classification basique par seuillage
        result[ndvi > 0.6] = 6    # Feuillu (forte vegetation)
        result[ndvi > 0.3] = 9    # Pelouse/prairie
        result[ndvi > 0.1] = 10   # Culture
        result[(ndvi <= 0.1) & (brightness > 180)] = 0  # Batiment
        result[(ndvi <= 0.1) & (brightness > 100)] = 2  # Zone impermeable
        result[(ndvi <= 0.1) & (brightness <= 100)] = 3  # Sol nu
        result[(ndvi < -0.1)] = 4  # Eau
    else:
        rouge = arr[0].astype(np.float64)
        vert = arr[1].astype(np.float64)
        bleu = arr[2].astype(np.float64)
        brightness = (rouge + vert + bleu) / 3.0
        result[brightness > 180] = 0
        result[(brightness > 100) & (brightness <= 180)] = 9
        result[brightness <= 100] = 3

    return result
