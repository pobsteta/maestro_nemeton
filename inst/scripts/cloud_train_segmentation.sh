#!/bin/bash
# =============================================================================
# cloud_train_segmentation.sh
# Script de setup + entrainement du decodeur de segmentation MAESTRO
# sur une instance GPU Scaleway.
#
# Ce script est destine a etre execute SUR l'instance GPU apres connexion SSH.
# Il installe les deps, telecharge les donnees (ortho, DEM, BD Foret V2),
# prepare les patches d'entrainement, puis lance l'entrainement du decodeur.
#
# Le backbone MAESTRO est gele. Seul le decodeur convolutionnel (~3M params)
# est entraine, ce qui est rapide meme sur un GPU modeste.
#
# Usage (sur l'instance GPU) :
#   # Mode 1 : FLAIR-HUB (telecharge, labellise, entraine - RECOMMANDE)
#   FLAIR_NIVEAU=complet bash cloud_train_segmentation.sh
#
#   # Mode 2 : avec un AOI (prepare les patches sur l'instance)
#   AOI_PATH=/data/aoi.gpkg bash cloud_train_segmentation.sh
#
#   # Mode 3 : patches deja prepares (upload via scp)
#   DATA_DIR=/data/segmentation bash cloud_train_segmentation.sh
#
# Variables d'environnement (toutes optionnelles) :
#   FLAIR_NIVEAU=complet     Niveau FLAIR-HUB: minimal (5 dom), standard (10),
#                            complet (20). Si defini, telecharge+labellise+entraine
#   AOI_PATH=...             GeoPackage de l'AOI (mode 2, ignore si FLAIR_NIVEAU)
#   DATA_DIR=...             Repertoire des patches pre-prepares (mode 3)
#   EPOCHS=50                Nombre d'epochs
#   BATCH_SIZE=8             Taille batch (8-16 selon VRAM)
#   LR=1e-3                  Learning rate
#   MODALITES=aerial,dem     Modalites (aerial, aerial,dem, aerial,dem,s2, ...)
#   BRANCH=main              Branche git a utiliser
#   OUTPUT_DIR=...           Repertoire de sortie
#   NOTIFY_EMAIL=...         Email de notification
#   NOTIFY_WEBHOOK=...       URL webhook (ntfy.sh, Slack)
#   MIN_FOREST_PCT=10        % minimum de foret pour garder un patch
#   VAL_RATIO=0.15           Proportion de patches en validation
# =============================================================================

set -euo pipefail

# --- Notifications ---
NOTIFY_EMAIL="${NOTIFY_EMAIL:-}"
NOTIFY_WEBHOOK="${NOTIFY_WEBHOOK:-}"

send_notification() {
    local SUBJECT="$1"
    local SHORT_MSG="$2"
    local BODY="${3:-$SHORT_MSG}"

    if [ -n "$NOTIFY_EMAIL" ]; then
        echo "  -> Envoi email a $NOTIFY_EMAIL"
        MAIL_SUBJECT="$SUBJECT" MAIL_BODY="$BODY" MAIL_TO="$NOTIFY_EMAIL" \
        python3 -c "
import os, smtplib, socket
from email.mime.text import MIMEText

hostname = socket.gethostname()
body = os.environ['MAIL_BODY']
subject = os.environ['MAIL_SUBJECT']
recipient = os.environ['MAIL_TO']

msg = MIMEText(body)
msg['Subject'] = subject
msg['From'] = 'maestro@' + hostname
msg['To'] = recipient

try:
    with smtplib.SMTP('localhost', 25, timeout=10) as s:
        s.sendmail(msg['From'], [recipient], msg.as_string())
    print('    Email envoye')
except Exception as e:
    print(f'    SMTP local echoue: {e}')
    import subprocess
    try:
        p = subprocess.Popen(['/usr/sbin/sendmail', '-t'], stdin=subprocess.PIPE)
        p.communicate(msg.as_string().encode())
        print('    Email envoye via sendmail')
    except Exception as e2:
        print(f'    Echec sendmail: {e2}')
" 2>&1 || echo "  (notification email echouee, non bloquant)"
    fi

    if [ -n "$NOTIFY_WEBHOOK" ]; then
        echo "  -> Envoi webhook"
        case "$NOTIFY_WEBHOOK" in
            *ntfy.sh*|*ntfy/*)
                curl -s -H "Title: $SUBJECT" -d "$SHORT_MSG" "$NOTIFY_WEBHOOK" 2>&1 || true
                ;;
            *hooks.slack.com*)
                curl -s -X POST -H 'Content-type: application/json' \
                    --data "{\"text\":\"*$SUBJECT*\n$SHORT_MSG\"}" "$NOTIFY_WEBHOOK" 2>&1 || true
                ;;
            *)
                curl -s -X POST -H 'Content-type: application/json' \
                    --data "{\"subject\":\"$SUBJECT\",\"text\":\"$SHORT_MSG\"}" \
                    "$NOTIFY_WEBHOOK" 2>&1 || true
                ;;
        esac
        echo "    Webhook envoye"
    fi
}

echo "========================================================"
echo " MAESTRO - Entrainement decodeur segmentation (NDP0)"
echo " Backbone gele, decodeur entrainable (~3M params)"
echo "========================================================"
echo ""

# --- Config ---
REPO_URL="https://github.com/pobsteta/maestro_nemeton.git"
BRANCH="${BRANCH:-main}"
WORK_DIR="$HOME/maestro_nemeton"

# Volume /data si disponible
if [ -d /data ] && mountpoint -q /data 2>/dev/null; then
    DEFAULT_DATA_DIR="/data/segmentation"
    OUTPUT_DIR="${OUTPUT_DIR:-/data/outputs/segmentation}"
    export HF_HOME="/data/.cache/huggingface"
    mkdir -p "$HF_HOME"
else
    DEFAULT_DATA_DIR="$WORK_DIR/data/segmentation"
    OUTPUT_DIR="${OUTPUT_DIR:-$WORK_DIR/outputs/segmentation}"
fi

FLAIR_NIVEAU="${FLAIR_NIVEAU:-}"
AOI_PATH="${AOI_PATH:-}"
DATA_DIR="${DATA_DIR:-$DEFAULT_DATA_DIR}"
FLAIR_DIR="${FLAIR_DIR:-${DEFAULT_DATA_DIR%/*}/flair_hub}"
EPOCHS="${EPOCHS:-50}"
BATCH_SIZE="${BATCH_SIZE:-8}"
LR="${LR:-1e-3}"
MODALITES="${MODALITES:-aerial,dem}"
PATIENCE="${PATIENCE:-10}"
MIN_FOREST_PCT="${MIN_FOREST_PCT:-10}"
VAL_RATIO="${VAL_RATIO:-0.15}"

# --- Cloner le depot ---
if [ ! -d "$WORK_DIR" ]; then
    echo "=== Clonage du depot ==="
    git clone -b "$BRANCH" "$REPO_URL" "$WORK_DIR"
fi
cd "$WORK_DIR"

# --- Environnement Python ---
echo ""
echo "=== Installation des dependances Python ==="

VENV_TEST="/tmp/_venv_test_$$"
if ! python3 -m venv "$VENV_TEST" 2>/dev/null; then
    echo "Installation de python3-venv..."
    apt-get update -qq
    PY_VERSION=$(python3 -c "import sys; print(f'{sys.version_info.major}.{sys.version_info.minor}')")
    apt-get install -y -qq "python${PY_VERSION}-venv" python3-venv 2>/dev/null || \
        apt-get install -y -qq python3-venv
fi
rm -rf "$VENV_TEST"

if [ -d /data ] && mountpoint -q /data 2>/dev/null; then
    DEFAULT_VENV="/data/venv_maestro"
else
    DEFAULT_VENV="$HOME/venv_maestro"
fi
VENV_DIR="${VENV_DIR:-$DEFAULT_VENV}"
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
    print('ATTENTION: Pas de GPU detecte, entrainement sur CPU (lent)')
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

# --- Installer R et les dependances systeme (pour FLAIR-HUB ou AOI) ---
install_r_deps() {
    echo "  Verification des dependances systeme..."
    apt-get update -qq

    # Toujours installer les libs systeme geospatiales (necessaires pour sf/terra)
    # meme si R est deja present sur l'image GPU
    echo "  Installation des bibliotheques systeme geospatiales..."
    apt-get install -y -qq \
        r-base \
        libgdal-dev libproj-dev libgeos-dev libudunits2-dev \
        libsqlite3-dev libcurl4-openssl-dev libssl-dev \
        libfontconfig1-dev libharfbuzz-dev libfribidi-dev \
        libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev

    Rscript -e "
    pkgs <- c('sf', 'terra', 'curl', 'jsonlite', 'fs')
    missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
    if (length(missing) > 0) {
        cat('  Installation de', length(missing), 'packages R:', paste(missing, collapse=', '), '\n')
        install.packages(missing, repos = 'https://cloud.r-project.org', Ncpus = parallel::detectCores())
    } else {
        cat('  Tous les packages R sont deja installes\n')
    }

    # Verification finale : sf est critique pour la labellisation
    failed <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
    if (length(failed) > 0) {
        stop('ERREUR: packages R non installes: ', paste(failed, collapse=', '),
             '. Verifiez les libs systeme (libgdal-dev, libgeos-dev, libproj-dev, libudunits2-dev)')
    }
    cat('  Tous les packages R sont prets\n')
    "
}

# --- Mode 1 : FLAIR-HUB (telecharger, labelliser, organiser) ---
if [ -n "$FLAIR_NIVEAU" ] && [ ! -d "$DATA_DIR/train/aerial" ]; then
    echo ""
    echo "========================================================"
    echo " Mode FLAIR-HUB : niveau '$FLAIR_NIVEAU'"
    echo "========================================================"

    install_r_deps

    # Convertir les modalites (aerial,dem -> c("aerial","dem"))
    R_MODALITES=$(echo "$MODALITES" | sed 's/,/","/g')
    R_MODALITES="c(\"$R_MODALITES\")"

    Rscript -e "
    # Charger le package en mode dev
    for (f in list.files('R', full.names = TRUE, pattern = '\\\\.R\$')) source(f)

    # Etape 1 : Telecharger les patches FLAIR-HUB
    message('=== Etape 1/3 : Telechargement FLAIR-HUB ===')
    download_flair_segmentation(
        niveau = '$FLAIR_NIVEAU',
        modalites = $R_MODALITES,
        data_dir = '$FLAIR_DIR'
    )

    # Etape 2 : Labelliser avec BD Foret V2
    message('=== Etape 2/3 : Labellisation BD Foret V2 ===')
    labelliser_flair_bdforet('$FLAIR_DIR')

    # Etape 3 : Organiser en train/val
    message('=== Etape 3/3 : Organisation train/val ===')
    preparer_flair_segmentation(
        flair_dir = '$FLAIR_DIR',
        output_dir = '$DATA_DIR',
        modalites = $R_MODALITES,
        val_ratio = $VAL_RATIO,
        min_forest_pct = $MIN_FOREST_PCT
    )
    "
fi

# --- Mode 2 : AOI (telecharger ortho+DEM, labelliser, decouper) ---
if [ -n "$AOI_PATH" ] && [ ! -d "$DATA_DIR/train/aerial" ]; then
    echo ""
    echo "========================================================"
    echo " Mode AOI : $AOI_PATH"
    echo "========================================================"

    install_r_deps

    Rscript -e "
    for (f in list.files('R', full.names = TRUE, pattern = '\\\\.R\$')) source(f)

    preparer_donnees_segmentation(
        aoi_path = '$AOI_PATH',
        output_dir = '$DATA_DIR',
        val_ratio = $VAL_RATIO,
        min_forest_pct = $MIN_FOREST_PCT
    )
    "
fi

# --- Verifier que les patches existent ---
if [ ! -d "$DATA_DIR/train/aerial" ]; then
    echo ""
    echo "ERREUR: Pas de patches d'entrainement trouves dans $DATA_DIR/train/aerial"
    echo ""
    echo "Trois options :"
    echo "  1. FLAIR-HUB (recommande) : FLAIR_NIVEAU=complet bash $0"
    echo "  2. AOI personnalisee      : AOI_PATH=/data/aoi.gpkg bash $0"
    echo "  3. Patches pre-prepares   : scp -r data/segmentation/ root@<IP>:$DATA_DIR/"
    exit 1
fi

# Compter les patches
N_TRAIN=$(ls "$DATA_DIR/train/aerial/"*.tif 2>/dev/null | wc -l)
N_VAL=$(ls "$DATA_DIR/val/aerial/"*.tif 2>/dev/null | wc -l)
echo ""
echo "  Patches: $N_TRAIN train, $N_VAL val"

# --- Lancer l'entrainement ---
echo ""
echo "=== Lancement de l'entrainement ==="
echo "  Epochs: $EPOCHS"
echo "  Batch size: $BATCH_SIZE"
echo "  Learning rate: $LR"
echo "  Modalites: $MODALITES"
echo "  Patience: $PATIENCE"
echo "  Sortie: $OUTPUT_DIR"
echo ""

# Notification de debut
GPU_NAME=$($PYTHON -c "import torch; print(torch.cuda.get_device_name(0) if torch.cuda.is_available() else 'CPU')" 2>/dev/null || echo "inconnu")
PUBLIC_IP=$(curl -s http://169.254.42.42/conf?format=json 2>/dev/null | python3 -c "import sys,json; print(json.load(sys.stdin).get('public_ip',{}).get('address','<IP>'))" 2>/dev/null || echo "<IP>")
HOSTNAME=$(hostname)
START_TIME=$(date '+%Y-%m-%d %H:%M:%S')

send_notification \
    "[MAESTRO] Segmentation - entrainement demarre" \
    "Entrainement segmentation lance sur $HOSTNAME ($PUBLIC_IP) - GPU: $GPU_NAME - $EPOCHS epochs, batch $BATCH_SIZE, $N_TRAIN patches" \
    "Bonjour,

L'entrainement du decodeur de segmentation MAESTRO vient de demarrer.

  - Instance : $HOSTNAME ($PUBLIC_IP)
  - GPU      : $GPU_NAME
  - Debut    : $START_TIME
  - Epochs   : $EPOCHS
  - Batch    : $BATCH_SIZE
  - LR       : $LR
  - Modalites: $MODALITES
  - Patches  : $N_TRAIN train, $N_VAL val

Pour suivre en temps reel :
  ssh -t root@${PUBLIC_IP} 'tmux attach -t maestro'

Vous recevrez une notification quand ce sera termine."

# Workers
N_WORKERS=$(nproc --ignore=2 2>/dev/null || echo 4)
N_WORKERS=$((N_WORKERS > 8 ? 8 : N_WORKERS))

mkdir -p "$OUTPUT_DIR"

$PYTHON inst/python/train_segmentation.py \
    --checkpoint "$CHECKPOINT" \
    --data-dir "$DATA_DIR" \
    --output-dir "$OUTPUT_DIR" \
    --modalites "$MODALITES" \
    --epochs "$EPOCHS" \
    --batch-size "$BATCH_SIZE" \
    --lr "$LR" \
    --patience "$PATIENCE" \
    --gpu \
    --workers "$N_WORKERS"

# --- Resultats ---
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
echo ""
echo "========================================================"
echo " Entrainement termine ! ($TIMESTAMP)"
echo "========================================================"
echo ""
echo "Modeles sauvegardes :"
ls -lh "$OUTPUT_DIR"/*.pt 2>/dev/null || echo "  (aucun modele trouve)"
echo ""
echo "Pour recuperer le decodeur sur votre PC :"
echo "  scp root@${PUBLIC_IP}:$OUTPUT_DIR/segmenter_ndp0_best.pt ."
echo ""
echo "Puis predire sur votre AOI :"
echo "  library(maestro)"
echo "  maestro_segmentation_pipeline("
echo "    aoi_path = 'data/aoi.gpkg',"
echo "    backbone_path = 'MAESTRO_pretrain.ckpt',"
echo "    decoder_path = 'segmenter_ndp0_best.pt'"
echo "  )"
echo ""
echo "IMPORTANT: Pensez a supprimer l'instance Scaleway !"
echo "  scw instance server terminate <SERVER_ID>"

touch ~/TRAINING_DONE

# Notification de fin
END_TIME=$(date '+%Y-%m-%d %H:%M:%S')
BEST_MODEL=$(ls -1 "$OUTPUT_DIR"/segmenter_ndp0_best.pt 2>/dev/null && echo "oui" || echo "non")
DUREE=$((SECONDS / 60))

send_notification \
    "[MAESTRO] Segmentation - entrainement termine !" \
    "Entrainement segmentation termine sur $HOSTNAME ($PUBLIC_IP) - Duree: ${DUREE}min - Modele: $BEST_MODEL" \
    "Bonjour,

L'entrainement du decodeur de segmentation est termine !

  - Instance : $HOSTNAME ($PUBLIC_IP)
  - Debut    : $START_TIME
  - Fin      : $END_TIME
  - Duree    : ${DUREE} minutes
  - Modele   : $BEST_MODEL
  - Sortie   : $OUTPUT_DIR

Pour recuperer le decodeur :
  scp root@${PUBLIC_IP}:$OUTPUT_DIR/segmenter_ndp0_best.pt .

IMPORTANT : Pensez a supprimer l'instance Scaleway !
  scw instance server terminate <SERVER_ID>"
