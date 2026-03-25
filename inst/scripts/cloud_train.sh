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
#   curl -sL https://raw.githubusercontent.com/pobsteta/maestro_nemeton/main/inst/scripts/cloud_train.sh | bash
#
# Ou manuellement :
#   git clone https://github.com/pobsteta/maestro_nemeton.git
#   cd maestro_nemeton
#   bash inst/scripts/cloud_train.sh
#
# Variables d'environnement (toutes optionnelles) :
#   EPOCHS=30            Nombre d'epochs
#   BATCH_SIZE=64        Taille batch
#   LR=1e-3              Learning rate
#   MODALITES=aerial     Modalites (aerial, aerial,s2, aerial,s1_asc,s1_des,s2)
#   UNFREEZE=1           Degeler le backbone (fine-tuning complet)
#   BRANCH=main          Branche git a utiliser
#   DATA_DIR=...         Repertoire des donnees
#   OUTPUT_DIR=...       Repertoire de sortie
#   NOTIFY_EMAIL=...     Adresse email pour notification en fin d'entrainement
#   NOTIFY_WEBHOOK=...   URL webhook (ntfy.sh, Slack, etc.) pour notification
# =============================================================================

set -euo pipefail

# --- Notifications (email + webhook) ---
NOTIFY_EMAIL="${NOTIFY_EMAIL:-}"
NOTIFY_WEBHOOK="${NOTIFY_WEBHOOK:-}"

# Fonction generique d'envoi de notification
# Usage : send_notification "sujet" "message court" "corps email detaille"
send_notification() {
    local SUBJECT="$1"
    local SHORT_MSG="$2"
    local BODY="${3:-$SHORT_MSG}"

    # --- Email ---
    if [ -n "$NOTIFY_EMAIL" ]; then
        echo "  -> Envoi email a $NOTIFY_EMAIL"
        # Utilise des variables d'environnement au lieu de l'interpolation bash
        # dans le code Python pour eviter les injections de code
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

    # --- Webhook ---
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

# Trap pour notifier en cas d'echec (set -e arrete le script avant la
# notification de fin si une commande echoue)
_on_error() {
    local exit_code=$?
    local line_no=$1
    echo ""
    echo "!!! ERREUR ligne $line_no (code $exit_code) !!!"
    DUREE=$((SECONDS / 60))
    send_notification \
        "[MAESTRO] ECHEC entrainement" \
        "Erreur ligne $line_no (code $exit_code) sur ${HOSTNAME:-instance} - Duree: ${DUREE}min" \
        "L'entrainement MAESTRO a echoue.

  - Instance : ${HOSTNAME:-instance} (${PUBLIC_IP:-<IP>})
  - Erreur   : ligne $line_no, code de sortie $exit_code
  - Duree    : ${DUREE} minutes

Connectez-vous pour diagnostiquer :
  ssh root@${PUBLIC_IP:-<IP>}
  tmux attach -t maestro"
}
trap '_on_error $LINENO' ERR

echo "========================================================"
echo " MAESTRO - Entrainement GPU sur Scaleway"
echo " TreeSatAI -> 8 classes regroupees"
echo "========================================================"
echo ""

# --- Config (surchargeables par variables d'environnement) ---
REPO_URL="https://github.com/pobsteta/maestro_nemeton.git"
BRANCH="${BRANCH:-main}"
WORK_DIR="$HOME/maestro_nemeton"
# Utiliser /data si le volume data est monte pour eviter de remplir le disque root
if [ -d /data ] && mountpoint -q /data 2>/dev/null; then
    DATA_DIR="${DATA_DIR:-/data/treesatai}"
    OUTPUT_DIR="${OUTPUT_DIR:-/data/outputs/training}"
    # Rediriger le cache HuggingFace vers /data pour eviter de remplir le disque root
    export HF_HOME="/data/.cache/huggingface"
    mkdir -p "$HF_HOME"
else
    DATA_DIR="${DATA_DIR:-$WORK_DIR/data/treesatai}"
    OUTPUT_DIR="${OUTPUT_DIR:-$WORK_DIR/outputs/training}"
fi
EPOCHS="${EPOCHS:-30}"
BATCH_SIZE="${BATCH_SIZE:-64}"
LR="${LR:-1e-3}"
MODALITES="${MODALITES:-aerial}"
UNFREEZE="${UNFREEZE:-}"

# --- Cloner le depot si necessaire ---
if [ ! -d "$WORK_DIR" ]; then
    echo "=== Clonage du depot ==="
    git clone -b "$BRANCH" "$REPO_URL" "$WORK_DIR"
fi
cd "$WORK_DIR"

# --- Environnement Python (venv) ---
echo ""
echo "=== Installation des dependances Python ==="

# Installer python3-venv si necessaire (absent sur les images GPU Scaleway)
# Note : "python3 -m venv --help" peut reussir meme sans ensurepip,
# donc on tente de creer un venv temporaire pour verifier
VENV_TEST="/tmp/_venv_test_$$"
if ! python3 -m venv "$VENV_TEST" 2>/dev/null; then
    echo "Installation de python3-venv et ensurepip..."
    apt-get update -qq
    # Installer le paquet versionne (ex: python3.10-venv) et le generique
    PY_VERSION=$(python3 -c "import sys; print(f'{sys.version_info.major}.{sys.version_info.minor}')")
    apt-get install -y -qq "python${PY_VERSION}-venv" python3-venv 2>/dev/null || \
        apt-get install -y -qq python3-venv
fi
rm -rf "$VENV_TEST"

# Utiliser /data si le volume data est monte, sinon $HOME
if [ -d /data ] && mountpoint -q /data 2>/dev/null; then
    DEFAULT_VENV="/data/venv_maestro"
    echo "Volume data detecte sur /data - utilisation pour le venv et les donnees"
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
echo "  Modalites: $MODALITES"
echo "  Unfreeze: ${UNFREEZE:-non}"
echo ""

UNFREEZE_FLAG=""
if [ -n "$UNFREEZE" ]; then
    UNFREEZE_FLAG="--unfreeze"
fi

# --- Notification de debut ---
GPU_NAME=$($PYTHON -c "import torch; print(torch.cuda.get_device_name(0) if torch.cuda.is_available() else 'CPU')" 2>/dev/null || echo "inconnu")
PUBLIC_IP=$(curl -s http://169.254.42.42/conf?format=json 2>/dev/null | python3 -c "import sys,json; print(json.load(sys.stdin).get('public_ip',{}).get('address','<IP_INSTANCE>'))" 2>/dev/null || echo "<IP_INSTANCE>")
HOSTNAME=$(hostname)
START_TIME=$(date '+%Y-%m-%d %H:%M:%S')

send_notification \
    "[MAESTRO] Entrainement demarre" \
    "Entrainement lance sur $HOSTNAME ($PUBLIC_IP) - GPU: $GPU_NAME - $EPOCHS epochs, batch $BATCH_SIZE, lr $LR, modalites $MODALITES" \
    "Bonjour,

L'entrainement MAESTRO vient de demarrer.

  - Instance : $HOSTNAME ($PUBLIC_IP)
  - GPU      : $GPU_NAME
  - Debut    : $START_TIME
  - Epochs   : $EPOCHS
  - Batch    : $BATCH_SIZE
  - LR       : $LR
  - Modalites: $MODALITES

Pour suivre en temps reel :
  ssh -t root@${PUBLIC_IP} 'tmux attach -t maestro'

Vous recevrez une notification quand ce sera termine."

# Adapter le nombre de workers au nombre de CPUs
N_WORKERS=$(nproc --ignore=2 2>/dev/null || echo 4)
N_WORKERS=$((N_WORKERS > 8 ? 8 : N_WORKERS))

$PYTHON inst/python/train_treesatai.py \
    --checkpoint "$CHECKPOINT" \
    --data-dir "$DATA_DIR" \
    --output-dir "$OUTPUT_DIR" \
    --modalites "$MODALITES" \
    --epochs "$EPOCHS" \
    --batch-size "$BATCH_SIZE" \
    --lr "$LR" \
    --gpu \
    --workers "$N_WORKERS" \
    $UNFREEZE_FLAG

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
echo "Pour recuperer le modele sur ton PC :"
echo "  scp root@${PUBLIC_IP}:$OUTPUT_DIR/maestro_treesatai_best.pt ."
echo ""
echo "Puis predire sur votre AOI :"
echo "  Rscript inst/scripts/predict_from_checkpoint.R \\"
echo "      --aoi data/aoi.gpkg \\"
echo "      --checkpoint maestro_treesatai_best.pt"
echo ""
echo "IMPORTANT: Pense a supprimer l'instance Scaleway !"
echo "  scw instance server terminate <SERVER_ID>"

# Creer un fichier flag pour indiquer la fin de l'entrainement
touch ~/TRAINING_DONE

# --- Notification de fin ---
END_TIME=$(date '+%Y-%m-%d %H:%M:%S')
BEST_MODEL=$(ls -1 "$OUTPUT_DIR"/maestro_treesatai_best.pt 2>/dev/null && echo "oui" || echo "non")
DUREE=$((SECONDS / 60))

send_notification \
    "[MAESTRO] Entrainement termine !" \
    "Entrainement termine sur $HOSTNAME ($PUBLIC_IP) - Duree: ${DUREE}min - $EPOCHS epochs - Modele: $BEST_MODEL" \
    "Bonjour,

L'entrainement MAESTRO est termine !

  - Instance : $HOSTNAME ($PUBLIC_IP)
  - Debut    : $START_TIME
  - Fin      : $END_TIME
  - Duree    : ${DUREE} minutes
  - Epochs   : $EPOCHS
  - Modele   : $BEST_MODEL
  - Sortie   : $OUTPUT_DIR

Pour recuperer le modele :
  scp root@${PUBLIC_IP}:$OUTPUT_DIR/maestro_treesatai_best.pt .

IMPORTANT : Pensez a supprimer l'instance Scaleway !
  scw instance server terminate <SERVER_ID>"
