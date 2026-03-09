#!/usr/bin/env Rscript
# =============================================================================
# predict_from_checkpoint.R
# Prediction des essences forestieres sur une AOI a partir d'un checkpoint
# MAESTRO fine-tune (recupere depuis Scaleway ou autre GPU).
#
# Usage :
#   Rscript inst/scripts/predict_from_checkpoint.R \
#       --aoi data/aoi.gpkg \
#       --checkpoint maestro_treesatai_best.pt
#
#   Rscript inst/scripts/predict_from_checkpoint.R \
#       --aoi data/aoi.gpkg \
#       --checkpoint maestro_treesatai_best.pt \
#       --output resultats/ \
#       --gpu \
#       --s2 --s1 \
#       --annees 2022:2024
#
# Entrees :
#   --aoi         Fichier GeoPackage (.gpkg) de la zone d'interet
#   --checkpoint  Fichier .pt du modele fine-tune (backbone + tete entrainee)
#
# Sorties (dans --output) :
#   essences_forestieres.gpkg  : Grille de patches avec les essences predites
#   essences_forestieres.tif   : Carte raster des essences
#   statistiques_essences.csv  : Statistiques par essence
# =============================================================================

suppressPackageStartupMessages({
  library(optparse)
})

# --- Options CLI ---
option_list <- list(
  make_option("--aoi", type = "character", default = NULL,
              help = "Chemin vers le fichier GeoPackage (.gpkg) de l'AOI [REQUIS]"),
  make_option("--checkpoint", type = "character", default = NULL,
              help = "Chemin vers le checkpoint fine-tune (.pt) [REQUIS]"),
  make_option("--output", type = "character", default = "outputs",
              help = "Repertoire de sortie [defaut: outputs]"),
  make_option("--gpu", action = "store_true", default = FALSE,
              help = "Utiliser le GPU CUDA"),
  make_option("--s2", action = "store_true", default = FALSE,
              help = "Inclure Sentinel-2 (10 bandes, 10m)"),
  make_option("--s1", action = "store_true", default = FALSE,
              help = "Inclure Sentinel-1 (VV+VH, 10m)"),
  make_option("--annees", type = "character", default = NULL,
              help = "Annees pour composite multitemporel (ex: 2022:2024)"),
  make_option("--saison", type = "character", default = "ete",
              help = "Saison pour composite : ete, printemps, automne, annee [defaut: ete]"),
  make_option("--patch-size", type = "integer", default = 250L,
              help = "Taille des patches en pixels [defaut: 250]"),
  make_option("--resolution", type = "double", default = 0.2,
              help = "Resolution spatiale en metres [defaut: 0.2]"),
  make_option("--batch-size", type = "integer", default = 16L,
              help = "Taille des batchs d'inference [defaut: 16]")
)

parser <- OptionParser(
  usage = "Usage: %prog --aoi AOI.gpkg --checkpoint MODEL.pt [OPTIONS]",
  option_list = option_list,
  description = paste(
    "",
    "Prediction des essences forestieres avec un modele MAESTRO fine-tune.",
    "Le modele est charge depuis un checkpoint .pt (recupere depuis Scaleway",
    "ou autre instance GPU apres entrainement sur TreeSatAI).",
    "",
    sep = "\n"
  )
)

args <- parse_args(parser)

# --- Validation ---
if (is.null(args$aoi)) {
  print_help(parser)
  stop("--aoi est requis (chemin vers le fichier GeoPackage)")
}
if (is.null(args$checkpoint)) {
  print_help(parser)
  stop("--checkpoint est requis (chemin vers le fichier .pt)")
}
if (!file.exists(args$aoi)) {
  stop(sprintf("Fichier AOI introuvable : %s", args$aoi))
}
if (!file.exists(args$checkpoint)) {
  stop(sprintf("Checkpoint introuvable : %s", args$checkpoint))
}

# --- Charger le package maestro ---
# Si execute depuis le repo, charger via devtools
if (file.exists("DESCRIPTION")) {
  message("Chargement du package maestro depuis le depot local...")
  suppressPackageStartupMessages(devtools::load_all(quiet = TRUE))
} else {
  suppressPackageStartupMessages(library(maestro))
}

# --- Parser les annees si fournies ---
annees_sentinel <- NULL
if (!is.null(args$annees)) {
  # Support format "2022:2024" ou "2022,2023,2024"
  if (grepl(":", args$annees)) {
    parts <- as.integer(strsplit(args$annees, ":")[[1]])
    annees_sentinel <- seq(parts[1], parts[2])
  } else {
    annees_sentinel <- as.integer(strsplit(args$annees, ",")[[1]])
  }
  message(sprintf("Mode multitemporel : %d annees (%s), saison = %s",
                   length(annees_sentinel),
                   paste(range(annees_sentinel), collapse = "-"),
                   args$saison))
}

# --- Lancer le pipeline ---
message("")
message("========================================================")
message(" MAESTRO - Prediction des essences forestieres")
message(sprintf(" AOI        : %s", args$aoi))
message(sprintf(" Checkpoint : %s", args$checkpoint))
message(sprintf(" Sortie     : %s/", args$output))
message(sprintf(" GPU        : %s", ifelse(args$gpu, "oui", "non")))
message("========================================================")
message("")

result <- maestro_pipeline(
  aoi_path           = args$aoi,
  output_dir         = args$output,
  checkpoint         = args$checkpoint,
  patch_size         = args$`patch-size`,
  resolution         = args$resolution,
  use_s2             = args$s2,
  use_s1             = args$s1,
  annees_sentinel    = annees_sentinel,
  saison             = args$saison,
  gpu                = args$gpu
)

message("")
message("========================================================")
message(" Prediction terminee !")
message(sprintf(" Resultats dans : %s/", args$output))
message("")
message(" Fichiers generes :")
output_files <- list.files(args$output, full.names = TRUE)
for (f in output_files) {
  size <- file.info(f)$size
  size_str <- if (size > 1e6) {
    sprintf("%.1f Mo", size / 1e6)
  } else {
    sprintf("%.0f Ko", size / 1e3)
  }
  message(sprintf("   %s (%s)", basename(f), size_str))
}
message("========================================================")
