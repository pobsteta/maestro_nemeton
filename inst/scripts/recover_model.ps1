# =============================================================================
# recover_model.ps1
# Recupere le modele entraine depuis une instance Scaleway et nettoie (Windows).
#
# Usage :
#   .\inst\scripts\recover_model.ps1
#   .\inst\scripts\recover_model.ps1 -IP 51.15.x.x
#   .\inst\scripts\recover_model.ps1 -IP 51.15.x.x -Terminate
#
# Actions :
#   1. Telecharge maestro_treesatai_best.pt depuis l'instance
#   2. Telecharge les logs d'entrainement
#   3. Propose de supprimer l'instance Scaleway
# =============================================================================

[CmdletBinding()]
param(
    [string]$IP,
    [switch]$Terminate
)

$ErrorActionPreference = "Stop"

function Log-Info  { param($msg) Write-Host "[INFO] $msg" -ForegroundColor Blue }
function Log-Ok    { param($msg) Write-Host "[OK] $msg" -ForegroundColor Green }
function Log-Warn  { param($msg) Write-Host "[WARN] $msg" -ForegroundColor Yellow }
function Log-Error { param($msg) Write-Host "[ERREUR] $msg" -ForegroundColor Red }

# --- Detecter l'IP de l'instance ---
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$RepoRoot = Split-Path -Parent (Split-Path -Parent $ScriptDir)
$ServerId = ""
$InstanceZone = ""
$InstanceName = ""

if ($IP) {
    $PublicIP = $IP
} else {
    $InfoFile = Join-Path $RepoRoot ".scaleway_instance"
    if (-not (Test-Path $InfoFile)) {
        Log-Error "Fichier .scaleway_instance introuvable."
        Write-Host "Usage : .\recover_model.ps1 -IP <IP_INSTANCE>"
        Write-Host "  ou lancez d'abord deploy_scaleway.ps1"
        exit 1
    }

    # Lire le fichier .scaleway_instance
    Get-Content $InfoFile | ForEach-Object {
        if ($_ -match '^(\w+)=(.+)$') {
            switch ($Matches[1]) {
                "SERVER_ID"     { $ServerId = $Matches[2] }
                "PUBLIC_IP"     { $PublicIP = $Matches[2] }
                "ZONE"          { $InstanceZone = $Matches[2] }
                "INSTANCE_NAME" { $InstanceName = $Matches[2] }
            }
        }
    }
    Log-Info "Instance trouvee : $InstanceName ($PublicIP)"
}

# --- Verifier que l'instance repond ---
Log-Info "Verification de la connexion SSH..."
try {
    $result = ssh -o ConnectTimeout=10 -o StrictHostKeyChecking=no "root@$PublicIP" "echo ok" 2>$null
    if ($result -ne "ok") { throw "SSH failed" }
    Log-Ok "Connexion SSH OK"
} catch {
    Log-Error "Impossible de se connecter a root@$PublicIP"
    exit 1
}

# --- Verifier si l'entrainement est termine ---
Log-Info "Verification de l'etat de l'entrainement..."

$TrainingStatus = ssh -o StrictHostKeyChecking=no "root@$PublicIP" @"
if tmux has-session -t maestro 2>/dev/null; then
    echo running
elif [ -f ~/maestro_nemeton/outputs/training/maestro_treesatai_best.pt ]; then
    echo done
else
    echo unknown
fi
"@

$TrainingStatus = $TrainingStatus.Trim()

switch ($TrainingStatus) {
    "running" {
        Log-Warn "L'entrainement est encore en cours !"
        Write-Host ""
        Write-Host "  Pour suivre la progression :"
        Write-Host "    ssh root@$PublicIP 'tmux attach -t maestro'"
        Write-Host ""
        Write-Host "  Derniers logs :"
        ssh -o StrictHostKeyChecking=no "root@$PublicIP" "tail -5 ~/train.log 2>/dev/null || echo '(pas de logs)'"
        Write-Host ""
        $reply = Read-Host "Recuperer le modele quand meme (meilleur partiel) ? [o/N]"
        if ($reply -ne "o" -and $reply -ne "O") {
            exit 0
        }
    }
    "done" {
        Log-Ok "Entrainement termine"
    }
    default {
        Log-Warn "Etat inconnu. Tentative de recuperation..."
    }
}

# --- Creer le repertoire local ---
$LocalDir = Join-Path $RepoRoot "outputs\training"
if (-not (Test-Path $LocalDir)) {
    New-Item -ItemType Directory -Path $LocalDir -Force | Out-Null
}

# --- Telecharger le modele ---
Write-Host ""
Log-Info "=== Recuperation du modele ==="

# Chercher le modele dans /data (volume) ou dans ~/maestro_nemeton (ancien chemin)
$RemoteDir = ssh -o StrictHostKeyChecking=no "root@$PublicIP" "if [ -d /data/outputs/training ]; then echo /data/outputs/training; else echo ~/maestro_nemeton/outputs/training; fi"
$RemoteDir = $RemoteDir.Trim()
Log-Info "Repertoire distant : $RemoteDir"

# Lister les fichiers disponibles
Log-Info "Fichiers disponibles sur l'instance :"
ssh -o StrictHostKeyChecking=no "root@$PublicIP" "ls -lh $RemoteDir/*.pt 2>/dev/null || echo '  (aucun fichier .pt)'"

# Telecharger le meilleur modele
$hasBest = ssh -o StrictHostKeyChecking=no "root@$PublicIP" "test -f $RemoteDir/maestro_treesatai_best.pt && echo yes || echo no"
if ($hasBest.Trim() -eq "yes") {
    Log-Info "Telechargement de maestro_treesatai_best.pt..."
    scp -o StrictHostKeyChecking=no "root@${PublicIP}:$RemoteDir/maestro_treesatai_best.pt" "$LocalDir\"
    Log-Ok "Modele recupere : $LocalDir\maestro_treesatai_best.pt"
    Get-Item "$LocalDir\maestro_treesatai_best.pt" | Select-Object Name, Length, LastWriteTime
} else {
    Log-Warn "maestro_treesatai_best.pt introuvable"
}

# Telecharger le modele final
$hasFinal = ssh -o StrictHostKeyChecking=no "root@$PublicIP" "test -f $RemoteDir/maestro_treesatai_final.pt && echo yes || echo no"
if ($hasFinal.Trim() -eq "yes") {
    Log-Info "Telechargement de maestro_treesatai_final.pt..."
    scp -o StrictHostKeyChecking=no "root@${PublicIP}:$RemoteDir/maestro_treesatai_final.pt" "$LocalDir\"
    Log-Ok "Modele final recupere : $LocalDir\maestro_treesatai_final.pt"
}

# Telecharger les logs
$hasLog = ssh -o StrictHostKeyChecking=no "root@$PublicIP" "test -f ~/train.log && echo yes || echo no"
if ($hasLog.Trim() -eq "yes") {
    Log-Info "Telechargement des logs..."
    scp -o StrictHostKeyChecking=no "root@${PublicIP}:~/train.log" "$LocalDir\"
    Log-Ok "Logs recuperes : $LocalDir\train.log"
}

# --- Resultats ---
Write-Host ""
Write-Host "========================================================"
Write-Host " Recuperation terminee !" -ForegroundColor Green
Write-Host "========================================================"
Write-Host ""
Write-Host "  Modele : $LocalDir\maestro_treesatai_best.pt"
Write-Host ""
Write-Host "  Pour predire sur votre AOI :"
Write-Host "    Rscript inst\scripts\predict_from_checkpoint.R ``"
Write-Host "        --aoi data\aoi.gpkg ``"
Write-Host "        --checkpoint $LocalDir\maestro_treesatai_best.pt"
Write-Host ""

# --- Nettoyage de l'instance ---
if ($ServerId -and $InstanceZone) {
    if ($Terminate) {
        Log-Info "Suppression de l'instance..."
        scw instance server terminate $ServerId zone=$InstanceZone with-ip=true
        Log-Ok "Instance supprimee"
        Remove-Item -Path (Join-Path $RepoRoot ".scaleway_instance") -Force -ErrorAction SilentlyContinue
    } else {
        Write-Host ""
        $reply = Read-Host "Supprimer l'instance Scaleway $InstanceName ? [o/N]"
        if ($reply -eq "o" -or $reply -eq "O") {
            Log-Info "Suppression de l'instance..."
            scw instance server terminate $ServerId zone=$InstanceZone with-ip=true
            Log-Ok "Instance supprimee"
            Remove-Item -Path (Join-Path $RepoRoot ".scaleway_instance") -Force -ErrorAction SilentlyContinue
        } else {
            Log-Warn "Instance toujours active : $PublicIP"
            Write-Host "  Pour la supprimer manuellement :"
            Write-Host "  scw instance server terminate $ServerId zone=$InstanceZone with-ip=true"
        }
    }
}
