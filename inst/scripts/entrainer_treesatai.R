#!/usr/bin/env Rscript
# =============================================================================
# entrainer_treesatai.R
# Fine-tuning de la tete MAESTRO sur TreeSatAI (8 classes regroupees)
#
# Utilisation :
#   Rscript inst/scripts/entrainer_treesatai.R
#   Rscript inst/scripts/entrainer_treesatai.R --gpu --unfreeze
#   Rscript inst/scripts/entrainer_treesatai.R --modalites aerial,s1_asc,s1_des,s2
#
# Pre-requis :
#   conda activate maestro
#   pip install rasterio h5py huggingface_hub
# =============================================================================

# --- Packages ---
if (!requireNamespace("reticulate", quietly = TRUE))
  stop("Package 'reticulate' requis: install.packages('reticulate')")

# --- Arguments CLI ---
args <- commandArgs(trailingOnly = TRUE)

# Defaults
gpu       <- "--gpu" %in% args
unfreeze  <- "--unfreeze" %in% args
epochs    <- 30L
batch_size <- 32L
lr        <- 1e-3
modalites <- "aerial"
data_dir  <- "data/treesatai"
output_dir <- "outputs/training"

# Parse named args
for (i in seq_along(args)) {
  if (args[i] == "--epochs" && i < length(args))
    epochs <- as.integer(args[i + 1])
  if (args[i] == "--batch-size" && i < length(args))
    batch_size <- as.integer(args[i + 1])
  if (args[i] == "--lr" && i < length(args))
    lr <- as.numeric(args[i + 1])
  if (args[i] == "--modalites" && i < length(args))
    modalites <- args[i + 1]
  if (args[i] == "--data-dir" && i < length(args))
    data_dir <- args[i + 1]
  if (args[i] == "--output-dir" && i < length(args))
    output_dir <- args[i + 1]
}

cat("========================================================\n")
cat(" MAESTRO - Fine-tuning sur TreeSatAI (8 classes)\n")
cat("========================================================\n\n")

# --- Configurer Python ---
# Utiliser le .Rprofile si disponible, sinon configurer manuellement
if (nchar(Sys.getenv("RETICULATE_PYTHON")) == 0) {
  envname <- "maestro"
  home <- Sys.getenv("HOME")
  conda_dirs <- c(
    file.path(home, "miniforge3"),
    file.path(home, "mambaforge"),
    file.path(home, "miniconda3"),
    file.path(home, "anaconda3")
  )
  for (d in conda_dirs) {
    py <- file.path(d, "envs", envname, "bin", "python")
    if (file.exists(py)) {
      Sys.setenv(RETICULATE_PYTHON = py)
      break
    }
  }
}

library(reticulate)
cat("Python:", reticulate::py_config()$python, "\n")

# --- Telecharger le modele MAESTRO via hfhub ---
if (!requireNamespace("hfhub", quietly = TRUE))
  stop("Package 'hfhub' requis: install.packages('hfhub')")

model_id <- "IGNF/MAESTRO_FLAIR-HUB_base"
cat("\nTelechargement du checkpoint MAESTRO depuis", model_id, "...\n")

repo_files <- hfhub::hub_list_files(model_id)
ckpt_file <- grep("pretrain.*\\.ckpt$", repo_files, value = TRUE)
if (length(ckpt_file) == 0) stop("Pas de checkpoint .ckpt trouve dans ", model_id)
ckpt_file <- ckpt_file[1]
cat("  Checkpoint:", ckpt_file, "\n")

checkpoint_path <- hfhub::hub_download(model_id, ckpt_file)
cat("  Cache local:", checkpoint_path, "\n")

# --- Lancer l'entrainement Python ---
train_script <- system.file("python", "train_treesatai.py", package = "maestro")
if (train_script == "")
  train_script <- file.path("inst", "python", "train_treesatai.py")

cmd_args <- c(
  train_script,
  "--checkpoint", checkpoint_path,
  "--data-dir", data_dir,
  "--output-dir", output_dir,
  "--modalites", modalites,
  "--epochs", epochs,
  "--batch-size", batch_size,
  "--lr", lr
)
if (gpu) cmd_args <- c(cmd_args, "--gpu")
if (unfreeze) cmd_args <- c(cmd_args, "--unfreeze")

python_bin <- reticulate::py_config()$python
cat("\nLancement de l'entrainement:\n")
cat(" ", python_bin, paste(cmd_args, collapse = " "), "\n\n")

exit_code <- system2(python_bin, cmd_args)
if (exit_code != 0) {
  stop("Entrainement echoue (code ", exit_code, ")")
}

cat("\n========================================================\n")
cat(" Entrainement termine !\n")
cat(" Modeles sauvegardes dans:", output_dir, "\n")
cat("========================================================\n")
