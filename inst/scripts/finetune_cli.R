#!/usr/bin/env Rscript
# =============================================================================
# finetune_cli.R
# Fine-tuning de MAESTRO sur TreeSatAI en ligne de commande
#
# Utilisation :
#   # 1. Telecharger TreeSatAI
#   Rscript finetune_cli.R --download --data_dir data/TreeSatAI
#
#   # 2. Fine-tuner (tete seulement)
#   Rscript finetune_cli.R --data_dir data/TreeSatAI
#
#   # 3. Fine-tuner avec encodeurs
#   Rscript finetune_cli.R --data_dir data/TreeSatAI --unfreeze --gpu
#
#   # 4. Utiliser le modele fine-tune dans le pipeline
#   Rscript maestro_cli.R --aoi data/aoi.gpkg --model outputs/maestro_8classes_treesatai.pt
# =============================================================================

if (!requireNamespace("optparse", quietly = TRUE)) {
  install.packages("optparse", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("maestro", quietly = TRUE)) {
  stop("Le package 'maestro' n'est pas installe.\n",
       "Installez-le avec : devtools::install() ou R CMD INSTALL .")
}

library(optparse)
library(maestro)

option_list <- list(
  make_option(c("--download"), action = "store_true", default = FALSE,
              help = "Telecharger le dataset TreeSatAI depuis Zenodo"),
  make_option(c("--download_hf"), action = "store_true", default = FALSE,
              help = "Telecharger TreeSatAI Time-Series depuis Hugging Face"),
  make_option(c("--data_dir"), type = "character", default = "data/TreeSatAI",
              help = "Dossier du dataset TreeSatAI [default: %default]"),
  make_option(c("--checkpoint"), type = "character", default = NULL,
              help = "Checkpoint pre-entraine (NULL = telecharger depuis HF)"),
  make_option(c("-o", "--output"), type = "character",
              default = "outputs/maestro_8classes_treesatai.pt",
              help = "Chemin de sortie du modele fine-tune [default: %default]"),
  make_option(c("--epochs"), type = "integer", default = 30L,
              help = "Nombre d'epoques [default: %default]"),
  make_option(c("--lr"), type = "double", default = 1e-3,
              help = "Learning rate (tete) [default: %default]"),
  make_option(c("--lr_encoder"), type = "double", default = 1e-5,
              help = "Learning rate (encodeurs) [default: %default]"),
  make_option(c("--batch_size"), type = "integer", default = 16L,
              help = "Taille du batch [default: %default]"),
  make_option(c("--unfreeze"), action = "store_true", default = FALSE,
              help = "Fine-tuner aussi les encodeurs (pas seulement la tete)"),
  make_option(c("--modalities"), type = "character", default = "aerial",
              help = "Modalites separees par virgule: aerial,s1,s2 [default: %default]"),
  make_option(c("--gpu"), action = "store_true", default = FALSE,
              help = "Utiliser le GPU (CUDA)"),
  make_option(c("--patience"), type = "integer", default = 5L,
              help = "Early stopping patience [default: %default]"),
  make_option(c("--token"), type = "character", default = NULL,
              help = "Token Hugging Face")
)

opt <- parse_args(OptionParser(option_list = option_list))

# 1. Telecharger si demande
if (opt$download) {
  mods <- strsplit(opt$modalities, ",")[[1]]
  download_treesatai(opt$data_dir, modalities = mods)
  if (!opt$download_hf && is.null(opt$checkpoint)) {
    message("\nTelechargement termine. Relancez sans --download pour fine-tuner.")
    quit(save = "no")
  }
}

if (opt$download_hf) {
  download_treesatai_hf(opt$data_dir)
  quit(save = "no")
}

# 2. Obtenir le checkpoint pre-entraine
checkpoint_path <- opt$checkpoint
if (is.null(checkpoint_path)) {
  message("Telechargement du checkpoint pre-entraine MAESTRO...")
  fichiers_modele <- telecharger_modele(token = opt$token)
  checkpoint_path <- fichiers_modele$weights
}

# 3. Fine-tuner
mods <- strsplit(opt$modalities, ",")[[1]]
finetune_maestro(
  checkpoint_path = checkpoint_path,
  data_dir = opt$data_dir,
  output_path = opt$output,
  epochs = opt$epochs,
  lr = opt$lr,
  lr_encoder = opt$lr_encoder,
  batch_size = opt$batch_size,
  freeze_encoder = !opt$unfreeze,
  modalities = mods,
  gpu = opt$gpu,
  patience = opt$patience
)
