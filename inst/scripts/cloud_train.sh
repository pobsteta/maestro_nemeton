#!/bin/bash
# =============================================================================
# cloud_train.sh
# Script de setup + entrainement MAESTRO sur une instance GPU Scaleway.
#
# Ce script est destine a etre execute SUR l'instance GPU apres connexion SSH.
# Il installe les deps, telecharge les donnees, lance l'entrainement,
# puis notifie que le modele est pret a etre recupere.
#
# Usage (sur l'instance GPU) :
#   curl -sL https://raw.githubusercontent.com/pobsteta/maestro_nemeton/cleanup/code-review/inst/scripts/cloud_train.sh | bash
#
# Ou manuellement :
#   git clone https://github.com/pobsteta/maestro_nemeton.git
#   cd maestro_nemeton
#   bash inst/scripts/cloud_train.sh
# =============================================================================

set -euo pipefail

echo "========================================================"
echo " MAESTRO - Entrainement GPU sur Scaleway"
echo " TreeSatAI -> 8 classes regroupees"
echo "========================================================"
echo ""

# --- Config ---
REPO_URL="https://github.com/pobsteta/maestro_nemeton.git"
BRANCH="cleanup/code-review"
WORK_DIR="$HOME/maestro_nemeton"
DATA_DIR="${DATA_DIR:-$WORK_DIR/data/treesatai}"
OUTPUT_DIR="${OUTPUT_DIR:-$WORK_DIR/outputs/training}"
EPOCHS=30
BATCH_SIZE=64
LR=1e-3

# --- Cloner le depot si necessaire ---
if [ ! -d "$WORK_DIR" ]; then
    echo "=== Clonage du depot ==="
    git clone -b "$BRANCH" "$REPO_URL" "$WORK_DIR"
fi
cd "$WORK_DIR"

# --- Environnement Python (venv) ---
echo ""
echo "=== Installation des dependances Python ==="

VENV_DIR="${VENV_DIR:-$HOME/venv_maestro}"
if [ ! -d "$VENV_DIR" ]; then
    python3 -m venv "$VENV_DIR"
fi
source "$VENV_DIR/bin/activate"
PYTHON="$VENV_DIR/bin/python"

$PYTHON -m pip install --quiet --upgrade pip
$PYTHON -m pip install --quiet \
    torch numpy safetensors \
    rasterio h5py huggingface_hub

# Verifier GPU
echo ""
echo "=== Verification GPU ==="
$PYTHON -c "
import torch
print(f'PyTorch {torch.__version__}')
print(f'CUDA disponible: {torch.cuda.is_available()}')
if torch.cuda.is_available():
    print(f'GPU: {torch.cuda.get_device_name(0)}')
    print(f'VRAM: {torch.cuda.get_device_properties(0).total_memory / 1e9:.1f} Go')
else:
    print('ATTENTION: Pas de GPU detecte, entrainement sur CPU')
"

# --- Telecharger le checkpoint MAESTRO ---
echo ""
echo "=== Telechargement du checkpoint MAESTRO ==="
CHECKPOINT=$($PYTHON -c "
from huggingface_hub import hf_hub_download
path = hf_hub_download('IGNF/MAESTRO_FLAIR-HUB_base',
    'MAESTRO_FLAIR-HUB_base/checkpoints/pretrain-epoch=99.ckpt')
print(path)
")
echo "Checkpoint: $CHECKPOINT"

# --- Lancer l'entrainement ---
echo ""
echo "=== Lancement de l'entrainement ==="
echo "  Epochs: $EPOCHS"
echo "  Batch size: $BATCH_SIZE"
echo "  Learning rate: $LR"
echo "  Modalite: aerial"
echo ""

$PYTHON inst/python/train_treesatai.py \
    --checkpoint "$CHECKPOINT" \
    --data-dir "$DATA_DIR" \
    --output-dir "$OUTPUT_DIR" \
    --modalites aerial \
    --epochs "$EPOCHS" \
    --batch-size "$BATCH_SIZE" \
    --lr "$LR" \
    --gpu \
    --workers 4

# --- Resultats ---
echo ""
echo "========================================================"
echo " Entrainement termine !"
echo "========================================================"
echo ""
echo "Modeles sauvegardes :"
ls -lh "$OUTPUT_DIR"/*.pt 2>/dev/null || echo "  (aucun modele trouve)"
echo ""
echo "Pour recuperer le modele sur ton PC :"
echo "  scp root@<IP_INSTANCE>:$OUTPUT_DIR/maestro_treesatai_best.pt ."
echo ""
echo "IMPORTANT: Pense a supprimer l'instance Scaleway !"
echo "  scw instance server terminate <SERVER_ID>"
