"""
maestro_segmentation.py
Decodeur de segmentation dense sur les tokens MAESTRO.

Strategie : le backbone MAESTRO (gele) produit des tokens spatiaux.
On extrait les tokens aerial (15x15 = 225 tokens de dim 768) apres
l'encoder_inter (qui a fusionne l'info S1/S2/DEM via cross-attention),
puis un decodeur convolutionnel upsampling les tokens en carte de
classes a 250x250 px (0.2m de resolution).

Architecture du decodeur :
  tokens 15x15x768
  -> ConvTranspose 15->30 (768->384)
  -> ConvTranspose 30->60 (384->192)
  -> ConvTranspose 60->125 (192->96)
  -> ConvTranspose 125->250 (96->48)
  -> Conv1x1 (48->n_classes)

Les tokens DEM (7x7), S2 (2x2), S1 (2x2) sont interpoles a 15x15
et ajoutes aux tokens aerial avant le decodeur, pour enrichir la
feature map avec l'information multi-modale spatialisee.
"""

import torch
import torch.nn as nn
import torch.nn.functional as F
import numpy as np
from pathlib import Path


# Classes NDP0 (10 classes)
CLASSES_NDP0 = [
    "Chene",           # 0
    "Hetre",           # 1
    "Chataignier",     # 2
    "Pin",             # 3
    "Epicea/Sapin",    # 4
    "Douglas",         # 5
    "Meleze",          # 6
    "Peuplier",        # 7
    "Feuillus divers", # 8
    "Non-foret",       # 9
]

# Nombre de tokens spatiaux par modalite (patch 50m)
TOKENS_GRID = {
    "aerial": (15, 15),   # 250/16 = 15 (arrondi inf, reste = 10px de bord)
    "dem":    (7, 7),     # 250/32 = 7  (reste = 26px de bord)
    "s2":     (2, 2),     # 5/2 = 2     (reste = 1px de bord)
    "s1_asc": (2, 2),     # 5/2 = 2
    "s1_des": (2, 2),     # 5/2 = 2
}


class SegmentationDecoder(nn.Module):
    """Decodeur convolutionnel pour segmentation dense a partir des tokens MAESTRO.

    Prend les tokens du backbone MAESTRO (15x15x768 pour aerial) et les
    decode progressivement en carte de segmentation 250x250 pixels.
    """

    def __init__(self, embed_dim=768, n_classes=10, target_size=250):
        super().__init__()
        self.embed_dim = embed_dim
        self.n_classes = n_classes
        self.target_size = target_size

        # Projection des tokens (fusion multi-modale)
        self.token_proj = nn.Sequential(
            nn.LayerNorm(embed_dim),
            nn.Linear(embed_dim, embed_dim),
            nn.GELU(),
        )

        # Decodeur par upsampling progressif
        # 15x15 -> 30x30 -> 60x60 -> 125x125 -> 250x250
        self.decoder = nn.Sequential(
            # Stage 1: 15->30
            nn.ConvTranspose2d(embed_dim, 384, kernel_size=4, stride=2, padding=1),
            nn.BatchNorm2d(384),
            nn.GELU(),

            # Stage 2: 30->60
            nn.ConvTranspose2d(384, 192, kernel_size=4, stride=2, padding=1),
            nn.BatchNorm2d(192),
            nn.GELU(),

            # Stage 3: 60->125 (stride=2, kernel=5 pour passer de 60 a 125)
            nn.ConvTranspose2d(192, 96, kernel_size=5, stride=2, padding=1),
            nn.BatchNorm2d(96),
            nn.GELU(),

            # Stage 4: 125->250
            nn.ConvTranspose2d(96, 48, kernel_size=4, stride=2, padding=1),
            nn.BatchNorm2d(48),
            nn.GELU(),
        )

        # Tete de classification pixel
        self.head = nn.Sequential(
            nn.Conv2d(48, 48, kernel_size=3, padding=1),
            nn.BatchNorm2d(48),
            nn.GELU(),
            nn.Conv2d(48, n_classes, kernel_size=1),
        )

    def forward(self, tokens_aerial, tokens_extra=None):
        """
        Args:
            tokens_aerial: (B, 225, D) tokens aerial apres encoder_inter
            tokens_extra: dict optionnel de tokens supplementaires
                          {"dem": (B, 49, D), "s2": (B, 4, D), ...}

        Returns:
            logits: (B, n_classes, 250, 250) logits de segmentation
        """
        B, N, D = tokens_aerial.shape

        # Reshape tokens aerial en feature map 2D
        h, w = TOKENS_GRID["aerial"]
        # N peut etre > h*w si le backbone a produit plus de tokens
        feat = tokens_aerial[:, :h * w, :]  # (B, 225, D)
        feat = self.token_proj(feat)         # (B, 225, D)
        feat = feat.reshape(B, h, w, D).permute(0, 3, 1, 2)  # (B, D, 15, 15)

        # Fusionner les tokens des autres modalites par interpolation
        if tokens_extra is not None:
            for mod_name, mod_tokens in tokens_extra.items():
                if mod_tokens is None or mod_name not in TOKENS_GRID:
                    continue
                mh, mw = TOKENS_GRID[mod_name]
                n_tok = mh * mw
                mt = mod_tokens[:, :n_tok, :]  # (B, n_tok, D)
                mt = mt.reshape(B, mh, mw, D).permute(0, 3, 1, 2)  # (B, D, mh, mw)
                # Interpoler a la taille de la feature map aerial (15x15)
                mt = F.interpolate(mt, size=(h, w), mode="bilinear",
                                   align_corners=False)
                feat = feat + mt  # Addition residuelle

        # Decoder upsampling
        x = self.decoder(feat)

        # Ajuster a la taille cible exacte (gestion des arrondis ConvTranspose)
        if x.shape[2] != self.target_size or x.shape[3] != self.target_size:
            x = F.interpolate(x, size=(self.target_size, self.target_size),
                              mode="bilinear", align_corners=False)

        # Classification pixel
        logits = self.head(x)  # (B, n_classes, 250, 250)
        return logits


class MAESTROSegmenter(nn.Module):
    """Modele complet : backbone MAESTRO (gele) + decodeur de segmentation.

    Le backbone produit les tokens multi-modaux. Le decodeur les transforme
    en carte de segmentation a 0.2m.
    """

    def __init__(self, backbone, decoder, freeze_backbone=True):
        super().__init__()
        self.backbone = backbone
        self.decoder = decoder

        if freeze_backbone:
            for param in self.backbone.parameters():
                param.requires_grad = False
            self.backbone.eval()

    def forward(self, inputs):
        """
        Args:
            inputs: dict[str, Tensor] des modalites
                    {"aerial": (B,4,250,250), "dem": (B,2,250,250), ...}

        Returns:
            logits: (B, n_classes, 250, 250)
        """
        # Passer dans le backbone (gele)
        with torch.no_grad():
            # Recuperer les tokens par modalite AVANT encoder_inter
            all_tokens = {}
            for mod_name, x in inputs.items():
                if mod_name not in self.backbone.model.patch_embed:
                    continue
                tokens = self.backbone.model.patch_embed[mod_name](x)
                enc_name = self.backbone.model._get_encoder_name(mod_name)
                tokens = self.backbone.model.encoder[enc_name](tokens)
                all_tokens[mod_name] = tokens

            # Concatener et passer dans encoder_inter
            concat_tokens = torch.cat(list(all_tokens.values()), dim=1)
            fused_tokens = self.backbone.model.encoder_inter(concat_tokens)

        # Separer les tokens fusionne par modalite
        # Les tokens sont concatenes dans l'ordre de all_tokens
        idx = 0
        fused_by_mod = {}
        for mod_name, tokens in all_tokens.items():
            n_tok = tokens.shape[1]
            fused_by_mod[mod_name] = fused_tokens[:, idx:idx + n_tok, :]
            idx += n_tok

        # Decoder : tokens aerial + tokens extra
        tokens_aerial = fused_by_mod.get("aerial")
        if tokens_aerial is None:
            raise ValueError("Modalite 'aerial' requise pour la segmentation")

        tokens_extra = {k: v for k, v in fused_by_mod.items() if k != "aerial"}

        logits = self.decoder(tokens_aerial, tokens_extra)
        return logits

    def train(self, mode=True):
        """Override: le backbone reste toujours en eval."""
        super().train(mode)
        self.backbone.eval()
        return self


# ---------------------------------------------------------------------------
# Fonctions utilitaires
# ---------------------------------------------------------------------------

def creer_segmenter(backbone, n_classes=10, embed_dim=768, target_size=250,
                    freeze_backbone=True):
    """Cree un MAESTROSegmenter a partir d'un backbone charge.

    Args:
        backbone: MAESTROClassifier charge (depuis charger_modele())
        n_classes: Nombre de classes (10 pour NDP0)
        embed_dim: Dimension des tokens (768 pour MAESTRO medium)
        target_size: Taille de sortie en pixels (250)
        freeze_backbone: Geler le backbone (defaut: True)

    Returns:
        MAESTROSegmenter
    """
    decoder = SegmentationDecoder(
        embed_dim=embed_dim,
        n_classes=n_classes,
        target_size=target_size,
    )
    segmenter = MAESTROSegmenter(backbone, decoder, freeze_backbone)

    n_params_decoder = sum(p.numel() for p in decoder.parameters())
    n_params_backbone = sum(p.numel() for p in backbone.parameters())
    print("  Segmenter cree:")
    print("    Backbone: %s parametres (gele=%s)" % (
        format(n_params_backbone, ","), freeze_backbone))
    print("    Decodeur: %s parametres (entrainable)" % format(n_params_decoder, ","))
    print("    Classes: %d (%s)" % (n_classes, ", ".join(CLASSES_NDP0[:n_classes])))
    print("    Sortie: %dx%d pixels" % (target_size, target_size))

    return segmenter


def predire_segmentation(segmenter, inputs, device="cpu"):
    """Predit la carte de segmentation pour un batch.

    Args:
        segmenter: MAESTROSegmenter
        inputs: dict[str, Tensor] ou dict[str, ndarray]
        device: 'cpu' ou 'cuda'

    Returns:
        dict avec 'classes' (ndarray H,W int), 'probas' (ndarray C,H,W float)
    """
    from maestro_inference import _preparer_inputs

    device = torch.device(device)
    inputs = _preparer_inputs(inputs, device)

    segmenter.eval()
    with torch.no_grad():
        logits = segmenter(inputs)                          # (B, C, H, W)
        probas = torch.softmax(logits, dim=1)               # (B, C, H, W)
        classes = torch.argmax(probas, dim=1)                # (B, H, W)

    return {
        "classes": classes.cpu().numpy(),
        "probas": probas.cpu().numpy(),
    }


def sauvegarder_segmenter(segmenter, path, n_classes=10, classes=None):
    """Sauvegarde le decodeur de segmentation (pas le backbone).

    Args:
        segmenter: MAESTROSegmenter
        path: Chemin de sortie (.pt)
        n_classes: Nombre de classes
        classes: Liste des noms de classes
    """
    if classes is None:
        classes = CLASSES_NDP0[:n_classes]

    checkpoint = {
        "decoder_state_dict": segmenter.decoder.state_dict(),
        "n_classes": n_classes,
        "classes": classes,
        "embed_dim": segmenter.decoder.embed_dim,
        "target_size": segmenter.decoder.target_size,
    }
    torch.save(checkpoint, path)
    print("  Decodeur sauvegarde: %s" % path)


def charger_segmenter(backbone, decoder_path, device="cpu",
                      freeze_backbone=True):
    """Charge un decodeur de segmentation sauvegarde.

    Args:
        backbone: MAESTROClassifier charge
        decoder_path: Chemin vers le .pt du decodeur
        device: 'cpu' ou 'cuda'
        freeze_backbone: Geler le backbone

    Returns:
        MAESTROSegmenter
    """
    device = torch.device(device)
    checkpoint = torch.load(decoder_path, map_location=device,
                            weights_only=False)

    n_classes = checkpoint["n_classes"]
    embed_dim = checkpoint.get("embed_dim", 768)
    target_size = checkpoint.get("target_size", 250)
    classes = checkpoint.get("classes", CLASSES_NDP0[:n_classes])

    print("  Chargement decodeur: %s" % Path(decoder_path).name)
    print("  Classes: %d (%s)" % (n_classes, ", ".join(classes)))

    decoder = SegmentationDecoder(
        embed_dim=embed_dim,
        n_classes=n_classes,
        target_size=target_size,
    )
    decoder.load_state_dict(checkpoint["decoder_state_dict"])

    segmenter = MAESTROSegmenter(backbone, decoder, freeze_backbone)
    segmenter = segmenter.to(device)
    segmenter.eval()

    return segmenter
