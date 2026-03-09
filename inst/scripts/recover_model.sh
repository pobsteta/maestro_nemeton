#!/bin/bash
# =============================================================================
# recover_model.sh
# Recupere le modele entraine depuis une instance Scaleway et nettoie.
#
# Usage :
#   bash inst/scripts/recover_model.sh [IP_INSTANCE]
#
# Si pas d'IP fournie, lit le fichier .scaleway_instance genere par deploy_scaleway.sh.
#
# Actions :
#   1. Telecharge maestro_treesatai_best.pt depuis l'instance
#   2. Telecharge les logs d'entrainement
#   3. Propose de supprimer l'instance Scaleway
# =============================================================================

set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info()  { echo -e "${BLUE}[INFO]${NC} $*"; }
log_ok()    { echo -e "${GREEN}[OK]${NC} $*"; }
log_warn()  { echo -e "${YELLOW}[WARN]${NC} $*"; }
log_error() { echo -e "${RED}[ERREUR]${NC} $*"; }

# --- Detecter l'IP de l'instance ---
if [ $# -ge 1 ]; then
    PUBLIC_IP="$1"
    SERVER_ID=""
    ZONE=""
else
    SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
    REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
    INFO_FILE="$REPO_ROOT/.scaleway_instance"

    if [ ! -f "$INFO_FILE" ]; then
        log_error "Fichier .scaleway_instance introuvable."
        echo "Usage : $0 <IP_INSTANCE>"
        echo "  ou lancez d'abord deploy_scaleway.sh"
        exit 1
    fi

    source "$INFO_FILE"
    log_info "Instance trouvee : $INSTANCE_NAME ($PUBLIC_IP)"
fi

# --- Verifier que l'instance repond ---
log_info "Verification de la connexion SSH..."
if ! ssh -o ConnectTimeout=10 -o StrictHostKeyChecking=no "root@$PUBLIC_IP" "echo ok" &>/dev/null; then
    log_error "Impossible de se connecter a root@$PUBLIC_IP"
    exit 1
fi
log_ok "Connexion SSH OK"

# --- Verifier si l'entrainement est termine ---
log_info "Verification de l'etat de l'entrainement..."

TRAINING_STATUS=$(ssh -o StrictHostKeyChecking=no "root@$PUBLIC_IP" bash -c "'
    if tmux has-session -t maestro 2>/dev/null; then
        echo \"running\"
    elif [ -f ~/maestro_nemeton/outputs/training/maestro_treesatai_best.pt ]; then
        echo \"done\"
    else
        echo \"unknown\"
    fi
'")

case "$TRAINING_STATUS" in
    running)
        log_warn "L'entrainement est encore en cours !"
        echo ""
        echo "  Pour suivre la progression :"
        echo "    ssh root@$PUBLIC_IP 'tmux attach -t maestro'"
        echo ""
        echo "  Pour voir les derniers logs :"
        ssh -o StrictHostKeyChecking=no "root@$PUBLIC_IP" "tail -5 ~/train.log 2>/dev/null || echo '(pas de logs)'"
        echo ""
        read -p "Recuperer le modele quand meme (meilleur partiel) ? [o/N] " -n 1 -r
        echo ""
        if [[ ! $REPLY =~ ^[Oo]$ ]]; then
            exit 0
        fi
        ;;
    done)
        log_ok "Entrainement termine"
        ;;
    *)
        log_warn "Etat inconnu. Tentative de recuperation..."
        ;;
esac

# --- Creer le repertoire local ---
LOCAL_DIR="outputs/training"
mkdir -p "$LOCAL_DIR"

# --- Telecharger le modele ---
echo ""
log_info "=== Recuperation du modele ==="

REMOTE_DIR="~/maestro_nemeton/outputs/training"

# Lister les fichiers disponibles
log_info "Fichiers disponibles sur l'instance :"
ssh -o StrictHostKeyChecking=no "root@$PUBLIC_IP" "ls -lh $REMOTE_DIR/*.pt 2>/dev/null || echo '  (aucun fichier .pt)'"

# Telecharger le meilleur modele
if ssh -o StrictHostKeyChecking=no "root@$PUBLIC_IP" "test -f $REMOTE_DIR/maestro_treesatai_best.pt"; then
    log_info "Telechargement de maestro_treesatai_best.pt..."
    scp -o StrictHostKeyChecking=no "root@$PUBLIC_IP:$REMOTE_DIR/maestro_treesatai_best.pt" "$LOCAL_DIR/"
    log_ok "Modele recupere : $LOCAL_DIR/maestro_treesatai_best.pt"
    ls -lh "$LOCAL_DIR/maestro_treesatai_best.pt"
else
    log_warn "maestro_treesatai_best.pt introuvable"
fi

# Telecharger le modele final aussi
if ssh -o StrictHostKeyChecking=no "root@$PUBLIC_IP" "test -f $REMOTE_DIR/maestro_treesatai_final.pt"; then
    log_info "Telechargement de maestro_treesatai_final.pt..."
    scp -o StrictHostKeyChecking=no "root@$PUBLIC_IP:$REMOTE_DIR/maestro_treesatai_final.pt" "$LOCAL_DIR/"
    log_ok "Modele final recupere : $LOCAL_DIR/maestro_treesatai_final.pt"
fi

# Telecharger les logs
if ssh -o StrictHostKeyChecking=no "root@$PUBLIC_IP" "test -f ~/train.log"; then
    log_info "Telechargement des logs..."
    scp -o StrictHostKeyChecking=no "root@$PUBLIC_IP:~/train.log" "$LOCAL_DIR/train.log"
    log_ok "Logs recuperes : $LOCAL_DIR/train.log"
fi

# --- Proposition de nettoyage ---
echo ""
echo "========================================================"
echo -e " ${GREEN}Recuperation terminee !${NC}"
echo "========================================================"
echo ""
echo "  Modele : $LOCAL_DIR/maestro_treesatai_best.pt"
echo ""
echo "  Pour predire sur votre AOI :"
echo "    Rscript inst/scripts/predict_from_checkpoint.R \\"
echo "        --aoi data/aoi.gpkg \\"
echo "        --checkpoint $LOCAL_DIR/maestro_treesatai_best.pt"
echo ""

if [ -n "${SERVER_ID:-}" ] && [ -n "${ZONE:-}" ]; then
    echo ""
    read -p "Supprimer l'instance Scaleway $INSTANCE_NAME ? [o/N] " -n 1 -r
    echo ""
    if [[ $REPLY =~ ^[Oo]$ ]]; then
        log_info "Suppression de l'instance..."
        scw instance server terminate "$SERVER_ID" zone="$ZONE" with-ip=true
        log_ok "Instance supprimee"
        rm -f "$REPO_ROOT/.scaleway_instance"
    else
        log_warn "Instance toujours active : $PUBLIC_IP"
        echo "  Pour la supprimer manuellement :"
        echo "  scw instance server terminate $SERVER_ID zone=$ZONE with-ip=true"
    fi
fi
