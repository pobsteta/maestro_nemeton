<#
.SYNOPSIS
    Deploie l'entrainement MAESTRO sur une instance GPU Scaleway.

.DESCRIPTION
    Ce script est execute DEPUIS VOTRE PC WINDOWS (PowerShell). Il :
      1. Cree une instance GPU Scaleway (via CLI scw)
      2. Envoie le script d'entrainement sur l'instance
      3. Lance l'entrainement en arriere-plan (tmux)
      4. Donne les commandes pour suivre et recuperer le modele

    Pre-requis :
      - CLI Scaleway installee et configuree (scw init)
      - Client SSH (OpenSSH integre a Windows 10+)
      - Cle SSH configuree dans Scaleway

    Instances GPU Scaleway recommandees :
      GPU-3070-S   : RTX 3070 (8 Go VRAM)  - ~0.76 EUR/h - suffisant pour aerial seul
      L4-1-24G     : NVIDIA L4 (24 Go VRAM) - ~0.92 EUR/h - recommande multi-modal
      H100-1-80G   : H100 (80 Go VRAM)     - ~3.50 EUR/h - entrainement rapide

    Cout estime pour 30 epochs (aerial, batch_size=64) :
      GPU-3070-S : ~2-3h -> ~2 EUR
      L4-1-24G   : ~1-2h -> ~1.50 EUR

.PARAMETER InstanceType
    Type d'instance GPU (defaut: GPU-3070-S)

.PARAMETER Image
    Image OS (defaut: ubuntu_jammy_gpu_os_12)

.PARAMETER Zone
    Zone Scaleway (defaut: fr-par-2)

.PARAMETER Epochs
    Nombre d'epochs (defaut: 30)

.PARAMETER BatchSize
    Taille batch (defaut: 64)

.PARAMETER LR
    Learning rate (defaut: 1e-3)

.PARAMETER Modalites
    Modalites (defaut: aerial)

.PARAMETER Unfreeze
    Degeler le backbone (fine-tuning complet)

.PARAMETER Segmentation
    Mode segmentation (decodeur NDP0 au lieu de TreeSatAI)

.PARAMETER Flair
    Niveau FLAIR-HUB : minimal, standard, complet (active automatiquement -Segmentation)

.PARAMETER Aoi
    Chemin local vers l'AOI GeoPackage (mode segmentation)

.PARAMETER InstanceName
    Nom de l'instance (defaut: maestro-train)

.PARAMETER DataVolumeGB
    Taille du volume data en Go (defaut: 100)

.PARAMETER NotifyEmail
    Email pour notifications debut/fin d'entrainement

.PARAMETER NotifyWebhook
    URL webhook (ntfy.sh, Slack) pour notifications

.PARAMETER DryRun
    Afficher les commandes sans executer

.EXAMPLE
    # Mode FLAIR-HUB (recommande pour segmentation) :
    .\deploy_scaleway.ps1 -Flair complet -InstanceType L4-1-24G

.EXAMPLE
    # Mode segmentation avec AOI locale :
    .\deploy_scaleway.ps1 -Segmentation -Aoi data\aoi.gpkg

.EXAMPLE
    # Classification TreeSatAI :
    .\deploy_scaleway.ps1 -Epochs 50 -BatchSize 32

.EXAMPLE
    # Dry-run :
    .\deploy_scaleway.ps1 -Flair complet -DryRun
#>

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
    [int]$DataVolumeGB = 100,
    [switch]$Unfreeze,
    [switch]$Segmentation,
    [string]$Flair = "",
    [string]$Aoi = "",
    [string]$Branch = "main",
    [string]$NotifyEmail = "",
    [string]$NotifyWebhook = "",
    [switch]$DryRun
)

# Note : "Continue" (pas "Stop") car les CLI externes (scw, ssh) ecrivent souvent
# des warnings sur stderr, que PowerShell traiterait comme des erreurs fatales.
# On utilise $LASTEXITCODE pour detecter les vrais echecs.
$ErrorActionPreference = "Continue"

# --- Couleurs ---
function Log-Info  { param($msg) Write-Host "[INFO] $msg" -ForegroundColor Blue }
function Log-Ok    { param($msg) Write-Host "[OK] $msg" -ForegroundColor Green }
function Log-Warn  { param($msg) Write-Host "[WARN] $msg" -ForegroundColor Yellow }
function Log-Error { param($msg) Write-Host "[ERREUR] $msg" -ForegroundColor Red }

# --- Si -Flair est specifie, activer segmentation ---
if ($Flair) {
    $Segmentation = [switch]::new($true)
}

# --- Scripts et chemins ---
if ($Segmentation) {
    $TrainScript = "cloud_train_segmentation.sh"
    $ResultFile = "segmenter_ndp0_best.pt"
    $ResultDir = "outputs/segmentation"
} else {
    $TrainScript = "cloud_train.sh"
    $ResultFile = "maestro_treesatai_best.pt"
    $ResultDir = "outputs/training"
}

# --- Affichage configuration ---
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
Write-Host "  Volume data   : ${DataVolumeGB} Go"
Write-Host "  Unfreeze      : $(if ($Unfreeze) { 'oui' } else { 'non' })"
Write-Host "  Branche       : $Branch"
Write-Host "  Mode          : $(if ($Segmentation) { 'segmentation NDP0' } else { 'classification TreeSatAI' })"
if ($Flair) {
    Write-Host "  FLAIR niveau  : $Flair"
}
if ($Aoi) {
    Write-Host "  AOI locale    : $Aoi"
}
Write-Host ""

# --- Verifier les pre-requis ---
Log-Info "Verification des pre-requis..."

# CLI Scaleway
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

# SSH
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

# --- Detecter le repertoire du projet ---
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$RepoRoot = Split-Path -Parent (Split-Path -Parent $ScriptDir)
$TrainScriptLocal = Join-Path $RepoRoot "inst\scripts\$TrainScript"

# --- Mode Dry-Run ---
if ($DryRun) {
    Write-Host ""
    Log-Warn "[DRY-RUN] Commandes qui seraient executees :"
    Write-Host ""
    Write-Host "# 1. Creer l'instance"
    Write-Host "scw instance server create type=$InstanceType image=$Image zone=$Zone name=$InstanceName ip=new -o json"
    Write-Host ""
    Write-Host "# 2. Attendre que l'instance soit prete"
    Write-Host 'scw instance server wait <SERVER_ID> zone=$Zone'
    Write-Host ""
    Write-Host "# 3. Recuperer l'IP"
    Write-Host '$ServerInfo = scw instance server get <SERVER_ID> zone='$Zone' -o json | ConvertFrom-Json'
    Write-Host '$IP = $ServerInfo.public_ip.address'
    Write-Host ""
    Write-Host "# 4. Copier et lancer le script d'entrainement"

    if ($Segmentation) {
        Write-Host "scp $TrainScriptLocal root@<IP>:~/"
        if ($Flair) {
            Write-Host "ssh root@<IP> 'FLAIR_NIVEAU=$Flair EPOCHS=$Epochs BATCH_SIZE=$BatchSize LR=$LR MODALITES=$Modalites bash ~/cloud_train_segmentation.sh'"
        } elseif ($Aoi) {
            Write-Host "scp $Aoi root@<IP>:/data/aoi.gpkg"
            Write-Host "ssh root@<IP> 'AOI_PATH=/data/aoi.gpkg EPOCHS=$Epochs BATCH_SIZE=$BatchSize LR=$LR MODALITES=$Modalites bash ~/cloud_train_segmentation.sh'"
        } else {
            Write-Host "# Upload des patches prepares :"
            Write-Host "scp -r data\segmentation\ root@<IP>:/data/segmentation/"
            Write-Host "ssh root@<IP> 'EPOCHS=$Epochs BATCH_SIZE=$BatchSize LR=$LR MODALITES=$Modalites bash ~/cloud_train_segmentation.sh'"
        }
        Write-Host ""
        Write-Host "# 5. Recuperer le decodeur"
        Write-Host "scp root@<IP>:~/maestro_nemeton/$ResultDir/$ResultFile ."
    } else {
        Write-Host "scp $TrainScriptLocal root@<IP>:~/"
        Write-Host "ssh root@<IP> 'EPOCHS=$Epochs BATCH_SIZE=$BatchSize bash ~/cloud_train.sh'"
        Write-Host ""
        Write-Host "# 5. Recuperer le modele"
        Write-Host "scp root@<IP>:~/maestro_nemeton/$ResultDir/$ResultFile ."
    }
    Write-Host ""
    Write-Host "# 6. Supprimer l'instance"
    Write-Host "scw instance server terminate <SERVER_ID> zone=$Zone with-ip=true"
    exit 0
}

# =============================================================================
# --- Etape 1 : Creer l'instance GPU ---
# =============================================================================
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

    if ($existing.state -match "stopping|stopped_in_place|locked") {
        Log-Info "Instance en cours de suppression, attente..."
        $waitMax = 120
        $waitElapsed = 0
        while ($waitElapsed -lt $waitMax) {
            Start-Sleep -Seconds 10
            $waitElapsed += 10
            $checkRaw = scw instance server list zone=$Zone name=$InstanceName -o json 2>&1
            $checkServers = $null
            try { $checkServers = $checkRaw | ConvertFrom-Json } catch { }
            if (-not $checkServers -or $checkServers.Count -eq 0) {
                Log-Ok "Instance supprimee apres ${waitElapsed}s"
                break
            }
            Log-Info "  Attente... (${waitElapsed}s / ${waitMax}s, etat: $($checkServers[0].state))"
        }
        $existingRaw = scw instance server list zone=$Zone name=$InstanceName -o json 2>&1
        $existingServers = $null
        try { $existingServers = $existingRaw | ConvertFrom-Json } catch { }
    }
}

if ($existingServers -and $existingServers.Count -gt 0) {
    $existing = $existingServers[0]

    if ($existing.state -eq "running") {
        Log-Warn "Reutilisation de l'instance en cours d'execution."
        $ServerId = $existing.id
    } elseif ($existing.state -eq "stopped") {
        Log-Info "Demarrage de l'instance arretee..."
        $ServerId = $existing.id
        scw instance server action run $ServerId zone=$Zone 2>&1 | Out-Null
    } else {
        Log-Error "Instance dans un etat inattendu : $($existing.state)"
        Log-Info "Supprimez-la manuellement : scw instance server terminate $($existing.id) zone=$Zone with-ip=true"
        exit 1
    }
} else {
    Log-Info "Creation de l'instance $InstanceType avec volume data ${DataVolumeGB}Go..."
    $rawJson = scw instance server create `
        type=$InstanceType `
        image=$Image `
        zone=$Zone `
        name=$InstanceName `
        ip=new `
        additional-volumes.0=block:${DataVolumeGB}GB `
        -o json 2>&1

    $ServerJson = $null
    try {
        $ServerJson = $rawJson | ConvertFrom-Json
    } catch {
        Log-Error "Echec de la creation de l'instance :"
        Write-Host $rawJson
        Write-Host ""
        Log-Info "Verifiez vos quotas et les types disponibles :"
        Write-Host "  scw instance server-type list zone=$Zone"
        exit 1
    }

    if ($ServerJson.error -or -not $ServerJson.id) {
        Log-Error "Echec de la creation de l'instance :"
        Write-Host $rawJson
        exit 1
    }

    $ServerId = $ServerJson.id
    Log-Ok "Instance creee: $ServerId"
}

# =============================================================================
# --- Etape 2 : Attendre que l'instance soit prete ---
# =============================================================================
Write-Host ""
Log-Info "=== Etape 2 : Demarrage de l'instance ==="
Log-Info "Attente du demarrage (peut prendre 2-5 minutes)..."

scw instance server wait $ServerId zone=$Zone timeout=600s

# Recuperer l'IP publique
$ServerInfo = scw instance server get $ServerId zone=$Zone -o json | ConvertFrom-Json

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

# =============================================================================
# --- Etape 3 : Attendre que SSH soit disponible ---
# =============================================================================
Write-Host ""
Log-Info "=== Etape 3 : Connexion SSH ==="

# Supprimer l'ancienne cle SSH
Log-Info "Nettoyage des anciennes cles SSH pour $PublicIP..."
try {
    $sshKeygenOutput = ssh-keygen -R $PublicIP 2>&1
} catch { }

# Etape 3a : Attendre que le port 22 soit ouvert (TCP)
$sshReady = $false
$sshTimeout = 600
$sshInterval = 10
$sshElapsed = 0

Log-Info "Attente du port SSH 22 sur $PublicIP (timeout: ${sshTimeout}s)..."

while ($sshElapsed -lt $sshTimeout) {
    try {
        $tcp = New-Object System.Net.Sockets.TcpClient
        $connect = $tcp.BeginConnect($PublicIP, 22, $null, $null)
        $wait = $connect.AsyncWaitHandle.WaitOne(5000, $false)
        if ($wait -and $tcp.Connected) {
            $tcp.Close()
            Log-Ok "Port SSH 22 ouvert apres ${sshElapsed}s"
            $sshReady = $true
            break
        }
        $tcp.Close()
    } catch { }
    $sshElapsed += $sshInterval
    if ($sshElapsed -lt $sshTimeout) {
        Log-Info "  Toujours en attente du SSH... (${sshElapsed}s/${sshTimeout}s)"
    }
    Start-Sleep -Seconds $sshInterval
}

if (-not $sshReady) {
    Log-Error "Timeout SSH apres ${sshTimeout}s - le port 22 n'a jamais repondu."
    Log-Info "Essayez manuellement : ssh root@$PublicIP"
    Log-Info "Pour supprimer : scw instance server terminate $ServerId zone=$Zone with-ip=true"
    exit 1
}

# Pause pour laisser sshd finir son initialisation
Log-Info "Attente de 10s supplementaires pour l'initialisation de sshd..."
Start-Sleep -Seconds 10

# Verifier la connexion SSH reelle
Log-Info "Test de connexion SSH..."
$sshTestOk = $false
for ($i = 1; $i -le 5; $i++) {
    try {
        $result = (ssh -o ConnectTimeout=10 -o StrictHostKeyChecking=accept-new "root@$PublicIP" "echo maestro_ready" 2>&1) | Out-String
        Log-Info "  SSH tentative $i - reponse: $($result.Trim())"
        if ($result -match "maestro_ready") {
            Log-Ok "Connexion SSH verifiee"
            $sshTestOk = $true
            break
        }
    } catch {
        Log-Info "  SSH tentative $i - exception: $_"
    }
    Log-Info "  Tentative SSH $i/5 echouee, nouvel essai dans 10s..."
    Start-Sleep -Seconds 10
}

if (-not $sshTestOk) {
    Log-Warn "Le test SSH a echoue mais le port est ouvert."
    Log-Warn "Le script continue - si l'etape 4 echoue, connectez-vous manuellement :"
    Log-Warn "  ssh root@$PublicIP"
}

# --- Etape 3b : Monter le volume data ---
$MountScript = Join-Path $RepoRoot "inst\scripts\mount_data.sh"
if (Test-Path $MountScript) {
    Write-Host ""
    Log-Info "=== Etape 3b : Montage du volume data ==="
    Log-Info "Formatage et montage du volume data sur /data..."
    scp -o StrictHostKeyChecking=accept-new $MountScript "root@${PublicIP}:~/"
    ssh -o StrictHostKeyChecking=accept-new "root@$PublicIP" "sed -i 's/\r$//' ~/mount_data.sh && bash ~/mount_data.sh"
}

# =============================================================================
# --- Etape 4 : Deployer et lancer l'entrainement ---
# =============================================================================
Write-Host ""
Log-Info "=== Etape 4 : Deploiement de l'entrainement ==="

# Copier le script d'entrainement
Log-Info "Envoi du script d'entrainement ($TrainScript)..."
scp -o StrictHostKeyChecking=accept-new $TrainScriptLocal "root@${PublicIP}:~/"

# Convertir les fins de ligne CRLF -> LF
Log-Info "Conversion des fins de ligne CRLF -> LF..."
ssh -o StrictHostKeyChecking=accept-new "root@$PublicIP" "sed -i 's/\r$//' ~/$TrainScript"

# Envoyer l'AOI si fourni (mode segmentation)
if ($Segmentation -and $Aoi) {
    Log-Info "Envoi de l'AOI: $Aoi"
    ssh -o StrictHostKeyChecking=accept-new "root@$PublicIP" "mkdir -p /data"
    scp -o StrictHostKeyChecking=accept-new $Aoi "root@${PublicIP}:/data/aoi.gpkg"
}

# Construire les exports d'environnement
$EnvExports = @()
if ($Flair) {
    $EnvExports += "export FLAIR_NIVEAU=$Flair;"
} elseif ($Segmentation -and $Aoi) {
    $EnvExports += "export AOI_PATH=/data/aoi.gpkg;"
}
$EnvExports += "export EPOCHS=$Epochs;"
$EnvExports += "export BATCH_SIZE=$BatchSize;"
$EnvExports += "export LR=$LR;"
$EnvExports += "export MODALITES=$Modalites;"

$EnvExports += "export BRANCH=$Branch;"

$UnfreezeVal = $(if ($Unfreeze) { "1" } else { "" })
if ($UnfreezeVal) {
    $EnvExports += "export UNFREEZE=$UnfreezeVal;"
}

# Notifications
if ($NotifyEmail) {
    $EnvExports += "export NOTIFY_EMAIL=$NotifyEmail;"
}
if ($NotifyWebhook) {
    $EnvExports += "export NOTIFY_WEBHOOK=$NotifyWebhook;"
}

$EnvString = $EnvExports -join " "

# Installer tmux
Log-Info "Installation de tmux sur l'instance..."
$PkgList = "tmux"
if ($NotifyEmail) {
    $PkgList = "tmux msmtp msmtp-mta"
    Log-Info "Installation de msmtp pour notification email vers $NotifyEmail"
}
ssh -o StrictHostKeyChecking=accept-new "root@$PublicIP" "apt-get update -qq && apt-get install -y -qq $PkgList > /dev/null 2>&1"

# Configurer tmux pour garder la fenetre ouverte meme si le process termine
ssh -o StrictHostKeyChecking=accept-new "root@$PublicIP" "echo 'set -g remain-on-exit on' > ~/.tmux.conf"

# Lancer l'entrainement dans tmux
Log-Info "Lancement de l'entrainement dans tmux..."

# Creer un script wrapper sur le serveur pour eviter les problemes de quotes
$WrapperContent = @"
#!/bin/bash
$EnvString
bash ~/cloud_train_segmentation.sh 2>&1 | tee ~/train.log
EXIT_CODE=\$?
echo ""
echo "=== Entrainement termine (code: \$EXIT_CODE) ==="
echo "Appuyez sur Entree pour fermer ou Ctrl+B D pour detacher"
read
"@
$WrapperContent | ssh -o StrictHostKeyChecking=accept-new "root@$PublicIP" "cat > ~/run_train.sh && chmod +x ~/run_train.sh"

ssh -o StrictHostKeyChecking=accept-new "root@$PublicIP" "tmux new-session -d -s maestro ~/run_train.sh"

# Verifier que la session tmux existe
Start-Sleep -Seconds 2
$tmuxCheck = ssh -o StrictHostKeyChecking=accept-new "root@$PublicIP" "tmux has-session -t maestro 2>&1 && echo tmux_ok || echo tmux_fail"
if ($tmuxCheck -notmatch "tmux_ok") {
    Log-Error "La session tmux n'a pas demarre. Diagnostic :"
    ssh -o StrictHostKeyChecking=accept-new "root@$PublicIP" "which tmux; tmux list-sessions 2>&1; cat ~/run_train.sh; cat ~/train.log 2>/dev/null | head -20"
    Log-Warn "Tentative de lancement direct (sans tmux)..."
    ssh -o StrictHostKeyChecking=accept-new "root@$PublicIP" "nohup ~/run_train.sh > ~/train.log 2>&1 &"
}

Log-Ok "Entrainement lance en arriere-plan (tmux session: maestro)"

# =============================================================================
# --- Etape 5 : Instructions de suivi ---
# =============================================================================
Write-Host ""
Write-Host "========================================================"
Write-Host " Deploiement reussi !" -ForegroundColor Green
Write-Host "========================================================"
Write-Host ""
Write-Host "  Instance  : $InstanceName ($InstanceType)"
Write-Host "  Server ID : $ServerId"
Write-Host "  IP        : $PublicIP"
Write-Host "  Zone      : $Zone"
Write-Host "  Mode      : $(if ($Segmentation) { 'segmentation NDP0' } else { 'classification TreeSatAI' })"
if ($Flair) {
    Write-Host "  FLAIR     : $Flair" -ForegroundColor Cyan
}
if ($NotifyEmail) {
    Write-Host "  Email     : $NotifyEmail" -ForegroundColor Cyan
}
if ($NotifyWebhook) {
    Write-Host "  Webhook   : $NotifyWebhook" -ForegroundColor Cyan
}
Write-Host ""
Write-Host "--- Commandes utiles ---" -ForegroundColor Cyan
Write-Host ""
Write-Host "  # Verifier si l'entrainement est termine :"
Write-Host "  ssh root@$PublicIP 'grep -q ""Entrainement termine"" ~/train.log && echo TERMINE || echo EN COURS'"
Write-Host ""
Write-Host "  # Suivre l'entrainement en temps reel :"
Write-Host "  ssh -t root@$PublicIP 'tmux attach -t maestro'"
Write-Host ""
Write-Host "  # Voir les logs :"
Write-Host "  ssh root@$PublicIP 'tail -f ~/train.log'"
Write-Host ""
Write-Host "  # Voir les dernieres lignes du log :"
Write-Host "  ssh root@$PublicIP 'tail -20 ~/train.log'"
Write-Host ""
Write-Host "  # Verifier la GPU :"
Write-Host "  ssh root@$PublicIP 'nvidia-smi'"
Write-Host ""
Write-Host "  # Verifier l'espace disque :"
Write-Host "  ssh root@$PublicIP 'df -h /data'"
Write-Host ""
Write-Host "  # Recuperer le modele entraine :"
Write-Host "  scp root@${PublicIP}:~/maestro_nemeton/$ResultDir/$ResultFile ."
Write-Host ""

if ($Segmentation) {
    Write-Host "  # Predire sur votre AOI (segmentation 0.2m) :"
    Write-Host "  library(maestro)"
    Write-Host "  maestro_segmentation_pipeline("
    Write-Host "    aoi_path = 'data/aoi.gpkg',"
    Write-Host "    backbone_path = 'MAESTRO_pretrain.ckpt',"
    Write-Host "    decoder_path = '$ResultFile'"
    Write-Host "  )"
} else {
    Write-Host "  # Predire sur votre AOI (apres recuperation du modele) :"
    Write-Host "  Rscript inst\scripts\predict_from_checkpoint.R ``"
    Write-Host "      --aoi data\aoi.gpkg ``"
    Write-Host "      --checkpoint $ResultFile ``"
    Write-Host "      --output outputs\"
}
Write-Host ""
Write-Host "  # IMPORTANT : Supprimer l'instance apres recuperation du modele !" -ForegroundColor Yellow
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
MODE=$(if ($Segmentation) { 'segmentation' } else { 'classification' })
FLAIR_NIVEAU=$Flair
RESULT_FILE=$ResultFile
RESULT_DIR=$ResultDir
"@ | Out-File -FilePath $InfoFile -Encoding UTF8

Log-Info "Infos instance sauvegardees dans .scaleway_instance"

# =============================================================================
# --- Etape 6 : Attacher a la session tmux pour suivre en direct ---
# =============================================================================
Write-Host ""
Log-Info "=== Connexion a la session tmux (Ctrl+B puis D pour detacher) ==="
Write-Host ""
ssh -t -o StrictHostKeyChecking=accept-new "root@$PublicIP" "tmux attach -t maestro"
