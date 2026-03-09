# =============================================================================
# deploy_scaleway.ps1
# Deploiement de l'entrainement MAESTRO sur une instance GPU Scaleway (Windows).
#
# Ce script est execute DEPUIS VOTRE PC WINDOWS. Il :
#   1. Cree une instance GPU Scaleway (via CLI scw)
#   2. Envoie le script d'entrainement sur l'instance
#   3. Lance l'entrainement en arriere-plan (tmux)
#   4. Donne les commandes pour suivre et recuperer le modele
#
# Pre-requis :
#   - CLI Scaleway installee (scw init)
#   - Client SSH (OpenSSH integre a Windows 10+)
#   - SCP disponible (integre avec OpenSSH)
#
# Usage :
#   .\inst\scripts\deploy_scaleway.ps1
#   .\inst\scripts\deploy_scaleway.ps1 -InstanceType L4-1-24G -Epochs 50
#   .\inst\scripts\deploy_scaleway.ps1 -DryRun
#
# Instances GPU Scaleway recommandees (verifiez la disponibilite avec scw instance server-type list) :
#   RENDER-S : GPU (petit)  - entree de gamme
#   L4-1-24G : NVIDIA L4 (24 Go VRAM) - bon rapport qualite/prix
#   H100-1-80G : H100 (80 Go VRAM) - entrainement rapide
#
# Listez les types disponibles dans votre zone :
#   scw instance server-type list zone=fr-par-2 | findstr -i gpu
# =============================================================================

[CmdletBinding()]
param(
    [string]$InstanceType = "L4-1-24G",
    [string]$Image = "ubuntu_jammy_gpu_os_12",
    [string]$Zone = "fr-par-2",
    [string]$InstanceName = "maestro-train",
    [int]$Epochs = 30,
    [int]$BatchSize = 64,
    [string]$LR = "1e-3",
    [string]$Modalites = "aerial",
    [switch]$Unfreeze,
    [switch]$DryRun
)

$ErrorActionPreference = "Stop"

# --- Couleurs ---
function Log-Info  { param($msg) Write-Host "[INFO] $msg" -ForegroundColor Blue }
function Log-Ok    { param($msg) Write-Host "[OK] $msg" -ForegroundColor Green }
function Log-Warn  { param($msg) Write-Host "[WARN] $msg" -ForegroundColor Yellow }
function Log-Error { param($msg) Write-Host "[ERREUR] $msg" -ForegroundColor Red }

Write-Host ""
Write-Host "========================================================"
Write-Host " MAESTRO - Deploiement GPU sur Scaleway (Windows)"
Write-Host "========================================================"
Write-Host ""
Log-Info "Configuration :"
Write-Host "  Instance type : $InstanceType"
Write-Host "  Image         : $Image"
Write-Host "  Zone          : $Zone"
Write-Host "  Nom           : $InstanceName"
Write-Host "  Epochs        : $Epochs"
Write-Host "  Batch size    : $BatchSize"
Write-Host "  Learning rate : $LR"
Write-Host "  Modalites     : $Modalites"
Write-Host "  Unfreeze      : $(if ($Unfreeze) { 'oui' } else { 'non' })"
Write-Host ""

# --- Verifier les pre-requis ---
Log-Info "Verification des pre-requis..."

# Verifier scw
if (-not (Get-Command scw -ErrorAction SilentlyContinue)) {
    Log-Error "CLI Scaleway (scw) non trouvee."
    Write-Host ""
    Write-Host "Installation :"
    Write-Host "  # Via winget"
    Write-Host "  winget install scaleway-cli"
    Write-Host ""
    Write-Host "  # Ou via scoop"
    Write-Host "  scoop bucket add scaleway https://github.com/scaleway/scoop-scaleway"
    Write-Host "  scoop install scaleway-cli"
    Write-Host ""
    Write-Host "  # Puis configurer :"
    Write-Host "  scw init"
    exit 1
}

# Verifier SSH
if (-not (Get-Command ssh -ErrorAction SilentlyContinue)) {
    Log-Error "Client SSH non trouve."
    Write-Host ""
    Write-Host "OpenSSH est integre a Windows 10+."
    Write-Host "Activez-le dans : Parametres > Applications > Fonctionnalites facultatives > Client OpenSSH"
    exit 1
}

# Verifier que scw est configure
$scwOutput = scw info 2>&1
if ($LASTEXITCODE -ne 0) {
    Log-Error "CLI Scaleway non configuree. Executez : scw init"
    exit 1
}
Log-Ok "CLI Scaleway configuree"

# --- Mode dry-run ---
if ($DryRun) {
    Write-Host ""
    Log-Warn "[DRY-RUN] Commandes qui seraient executees :"
    Write-Host ""
    Write-Host "# 1. Creer l'instance"
    Write-Host "scw instance server create type=$InstanceType image=$Image zone=$Zone name=$InstanceName ip=new -o json"
    Write-Host ""
    Write-Host "# 2. Attendre que l'instance soit prete"
    Write-Host "scw instance server wait <SERVER_ID> zone=$Zone"
    Write-Host ""
    Write-Host "# 3. Recuperer l'IP"
    Write-Host "scw instance server get <SERVER_ID> zone=$Zone -o json"
    Write-Host ""
    Write-Host "# 4. Copier et lancer le script d'entrainement"
    Write-Host "scp inst\scripts\cloud_train.sh root@<IP>:~/"
    Write-Host "ssh root@<IP> 'EPOCHS=$Epochs BATCH_SIZE=$BatchSize bash ~/cloud_train.sh'"
    Write-Host ""
    Write-Host "# 5. Recuperer le modele"
    Write-Host "scp root@<IP>:~/maestro_nemeton/outputs/training/maestro_treesatai_best.pt ."
    Write-Host ""
    Write-Host "# 6. Supprimer l'instance"
    Write-Host "scw instance server terminate <SERVER_ID> zone=$Zone with-ip=true"
    exit 0
}

# --- Etape 1 : Verifier les instances existantes et creer ---
Write-Host ""
Log-Info "=== Etape 1 : Creation de l'instance GPU ==="

# Verifier si une instance avec le meme nom existe deja
Log-Info "Verification des instances existantes..."
$existingRaw = scw instance server list zone=$Zone name=$InstanceName -o json 2>&1
$existingServers = $null
try {
    $existingServers = $existingRaw | ConvertFrom-Json
} catch { }

if ($existingServers -and $existingServers.Count -gt 0) {
    $existing = $existingServers[0]
    Log-Warn "Instance '$InstanceName' deja existante : $($existing.id) (etat: $($existing.state))"
    Log-Info "Reutilisation de l'instance existante."
    $ServerId = $existing.id

    # Demarrer si arretee
    if ($existing.state -eq "stopped") {
        Log-Info "Demarrage de l'instance arretee..."
        scw instance server action run $ServerId zone=$Zone 2>&1 | Out-Null
    }
} else {
    Log-Info "Creation de l'instance $InstanceType..."
    $rawJson = scw instance server create `
        type=$InstanceType `
        image=$Image `
        zone=$Zone `
        name=$InstanceName `
        ip=new `
        -o json 2>&1

    # Verifier si la creation a echoue
    $ServerJson = $null
    try {
        $ServerJson = $rawJson | ConvertFrom-Json
    } catch {
        Log-Error "Echec de la creation de l'instance :"
        Write-Host $rawJson
        Write-Host ""
        Log-Info "Verifiez vos quotas : une instance du meme type existe peut-etre deja."
        Log-Info "  scw instance server list zone=$Zone"
        Log-Info "Listez les types GPU disponibles avec :"
        Write-Host "  scw instance server-type list zone=$Zone"
        exit 1
    }

    if ($ServerJson.error -or -not $ServerJson.id) {
        Log-Error "Echec de la creation de l'instance :"
        Write-Host $rawJson
        Write-Host ""
        Log-Info "Verifiez vos quotas : une instance du meme type existe peut-etre deja."
        Log-Info "  scw instance server list zone=$Zone"
        Log-Info "Listez les types GPU disponibles avec :"
        Write-Host "  scw instance server-type list zone=$Zone"
        exit 1
    }

    $ServerId = $ServerJson.id
    Log-Ok "Instance creee: $ServerId"
}

# --- Etape 2 : Attendre que l'instance soit prete ---
Write-Host ""
Log-Info "=== Etape 2 : Demarrage de l'instance ==="
Log-Info "Attente du demarrage (peut prendre 2-5 minutes)..."

scw instance server wait $ServerId zone=$Zone timeout=600s

# Recuperer l'IP publique
$ServerInfo = scw instance server get $ServerId zone=$Zone -o json | ConvertFrom-Json

# Extraire l'IP publique (gerer les differents formats de reponse)
$PublicIP = $null
if ($ServerInfo.public_ip -and $ServerInfo.public_ip.address) {
    $PublicIP = $ServerInfo.public_ip.address
} elseif ($ServerInfo.public_ips -and $ServerInfo.public_ips.Count -gt 0) {
    $PublicIP = $ServerInfo.public_ips[0].address
}

if (-not $PublicIP) {
    Log-Error "Impossible de recuperer l'IP publique de l'instance."
    Log-Info "Verifiez manuellement :"
    Write-Host "  scw instance server get $ServerId zone=$Zone"
    Log-Info "Pour supprimer : scw instance server terminate $ServerId zone=$Zone with-ip=true"
    exit 1
}
Log-Ok "Instance prete: $PublicIP"

# --- Etape 3 : Attendre que SSH soit disponible ---
Write-Host ""
Log-Info "=== Etape 3 : Connexion SSH ==="

# Supprimer l'ancienne cle SSH pour cette IP (evite l'erreur "host key changed")
Log-Info "Nettoyage des anciennes cles SSH pour $PublicIP..."
try {
    $sshKeygenOutput = ssh-keygen -R $PublicIP 2>&1
} catch {
    # Ignorer : l'hote n'est peut-etre pas dans known_hosts
}

Log-Info "Attente du serveur SSH..."

$sshReady = $false
$sshTimeout = 600      # 10 minutes
$sshInterval = 15      # secondes entre chaque tentative
$sshElapsed = 0

Log-Info "Attente du serveur SSH (timeout: ${sshTimeout}s)..."

while ($sshElapsed -lt $sshTimeout) {
    $result = $null
    try {
        $result = ssh -o ConnectTimeout=10 -o StrictHostKeyChecking=no -o "UserKnownHostsFile=NUL" -o BatchMode=yes "root@$PublicIP" "echo ok" 2>&1
    } catch { }
    if ($result -match "ok") {
        Log-Ok "SSH disponible apres ${sshElapsed}s"
        $sshReady = $true
        break
    }
    $sshElapsed += $sshInterval
    if ($sshElapsed -lt $sshTimeout) {
        Log-Info "  Toujours en attente du SSH... (${sshElapsed}s/${sshTimeout}s)"
    }
    Start-Sleep -Seconds $sshInterval
}

if (-not $sshReady) {
    Log-Error "Timeout SSH apres $($maxAttempts * 5)s"
    Log-Info "L'instance est peut-etre encore en cours de demarrage."
    Log-Info "Essayez manuellement : ssh root@$PublicIP"
    Log-Info "Pour supprimer : scw instance server terminate $ServerId zone=$Zone with-ip=true"
    exit 1
}

# --- Etape 4 : Deployer et lancer l'entrainement ---
Write-Host ""
Log-Info "=== Etape 4 : Deploiement de l'entrainement ==="

# Detecter le repertoire du script
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$RepoRoot = Split-Path -Parent (Split-Path -Parent $ScriptDir)

# Copier le script d'entrainement
Log-Info "Envoi du script d'entrainement..."
scp -o StrictHostKeyChecking=no -o "UserKnownHostsFile=NUL" "$RepoRoot\inst\scripts\cloud_train.sh" "root@${PublicIP}:~/"

# Preparer le flag unfreeze
$UnfreezeVal = $(if ($Unfreeze) { "1" } else { "" })

# Lancer l'entrainement dans tmux
Log-Info "Lancement de l'entrainement dans tmux..."
ssh -o StrictHostKeyChecking=no -o "UserKnownHostsFile=NUL" "root@$PublicIP" @"
apt-get update -qq && apt-get install -y -qq tmux > /dev/null 2>&1
tmux new-session -d -s maestro "export EPOCHS=$Epochs; export BATCH_SIZE=$BatchSize; export LR=$LR; export MODALITES=$Modalites; export UNFREEZE=$UnfreezeVal; bash ~/cloud_train.sh 2>&1 | tee ~/train.log"
"@

Log-Ok "Entrainement lance en arriere-plan (tmux session: maestro)"

# --- Etape 5 : Instructions de suivi ---
Write-Host ""
Write-Host "========================================================"
Write-Host " Deploiement reussi !" -ForegroundColor Green
Write-Host "========================================================"
Write-Host ""
Write-Host "  Instance  : $InstanceName ($InstanceType)"
Write-Host "  Server ID : $ServerId"
Write-Host "  IP        : $PublicIP"
Write-Host "  Zone      : $Zone"
Write-Host ""
Write-Host "--- Commandes utiles ---"
Write-Host ""
Write-Host "  # Suivre l'entrainement en temps reel :"
Write-Host "  ssh root@$PublicIP 'tmux attach -t maestro'"
Write-Host ""
Write-Host "  # Voir les logs :"
Write-Host "  ssh root@$PublicIP 'tail -f ~/train.log'"
Write-Host ""
Write-Host "  # Verifier la GPU :"
Write-Host "  ssh root@$PublicIP 'nvidia-smi'"
Write-Host ""
Write-Host "  # Recuperer le modele entraine :"
Write-Host "  scp root@${PublicIP}:~/maestro_nemeton/outputs/training/maestro_treesatai_best.pt ."
Write-Host ""
Write-Host "  # Ou utiliser le script de recuperation :"
Write-Host "  .\inst\scripts\recover_model.ps1"
Write-Host ""
Write-Host "  # IMPORTANT : Supprimer l'instance apres recuperation du modele !"
Write-Host "  scw instance server terminate $ServerId zone=$Zone with-ip=true"
Write-Host ""

# Sauvegarder les infos de l'instance
$InfoFile = Join-Path $RepoRoot ".scaleway_instance"
@"
# Instance Scaleway creee le $(Get-Date -Format "yyyy-MM-ddTHH:mm:ss")
SERVER_ID=$ServerId
PUBLIC_IP=$PublicIP
ZONE=$Zone
INSTANCE_NAME=$InstanceName
INSTANCE_TYPE=$InstanceType
"@ | Out-File -FilePath $InfoFile -Encoding UTF8

Log-Info "Infos instance sauvegardees dans .scaleway_instance"
