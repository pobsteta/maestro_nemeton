#!/usr/bin/env Rscript
# =============================================================================
# maestro_cli.R
# Interface en ligne de commande pour le package maestro
#
# Utilisation :
#   Rscript maestro_cli.R --aoi data/aoi.gpkg
#   Rscript maestro_cli.R --aoi data/aoi.gpkg --millesime_ortho 2023
#   Rscript maestro_cli.R --aoi data/aoi.gpkg --s2 --s1 --gpu
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
  make_option(c("-a", "--aoi"), type = "character", default = "data/aoi.gpkg",
              help = "Chemin vers le fichier GeoPackage de la zone d'interet [default: %default]"),
  make_option(c("-o", "--output"), type = "character", default = "outputs",
              help = "Repertoire de sortie [default: %default]"),
  make_option(c("-m", "--model"), type = "character",
              default = "IGNF/MAESTRO_FLAIR-HUB_base",
              help = "Identifiant du modele Hugging Face [default: %default]"),
  make_option(c("--millesime_ortho"), type = "integer", default = NULL,
              help = "Millesime de l'ortho RVB (NULL = plus recent)"),
  make_option(c("--millesime_irc"), type = "integer", default = NULL,
              help = "Millesime de l'ortho IRC (NULL = plus recent)"),
  make_option(c("-s", "--patch_size"), type = "integer", default = 250L,
              help = "Taille des patches en pixels [default: %default]"),
  make_option(c("--resolution"), type = "double", default = 0.2,
              help = "Resolution spatiale en metres [default: %default]"),
  make_option(c("--s2"), action = "store_true", default = FALSE,
              help = "Inclure Sentinel-2 (10 bandes spectrales, 10m)"),
  make_option(c("--s1"), action = "store_true", default = FALSE,
              help = "Inclure Sentinel-1 (VV+VH radar, 10m)"),
  make_option(c("--date_sentinel"), type = "character", default = NULL,
              help = "Date cible pour Sentinel (YYYY-MM-DD, NULL = ete en cours). Ignore si --annees est fourni."),
  make_option(c("--annees"), type = "character", default = NULL,
              help = "Annees pour composite multi-annuel (ex: '2021:2024' ou '2022,2023,2024'). Active le mode multitemporel."),
  make_option(c("--saison"), type = "character", default = "ete",
              help = "Saison pour le composite: ete, printemps, automne, annee [default: %default]"),
  make_option(c("--max_scenes"), type = "integer", default = 3L,
              help = "Nombre max de scenes par annee pour le composite [default: %default]"),
  make_option(c("--gpu"), action = "store_true", default = FALSE,
              help = "Utiliser le GPU (CUDA) si disponible"),
  make_option(c("--token"), type = "character", default = NULL,
              help = "Token Hugging Face (ou variable HUGGING_FACE_HUB_TOKEN)")
)

opt <- parse_args(OptionParser(option_list = option_list))

# Parser le parametre --annees (supporte "2021:2024" ou "2022,2023,2024")
annees_sentinel <- NULL
if (!is.null(opt$annees)) {
  annees_str <- opt$annees
  if (grepl(":", annees_str)) {
    parts <- as.integer(strsplit(annees_str, ":")[[1]])
    annees_sentinel <- seq(parts[1], parts[2])
  } else {
    annees_sentinel <- as.integer(strsplit(annees_str, ",")[[1]])
  }
  message(sprintf("Mode multitemporel: annees %s",
                   paste(annees_sentinel, collapse = ", ")))
}

maestro_pipeline(
  aoi_path             = opt$aoi,
  output_dir           = opt$output,
  model_id             = opt$model,
  millesime_ortho      = opt$millesime_ortho,
  millesime_irc        = opt$millesime_irc,
  patch_size           = opt$patch_size,
  resolution           = opt$resolution,
  use_s2               = opt$s2,
  use_s1               = opt$s1,
  date_sentinel        = opt$date_sentinel,
  annees_sentinel      = annees_sentinel,
  saison               = opt$saison,
  max_scenes_par_annee = opt$max_scenes,
  gpu                  = opt$gpu,
  token                = opt$token
)
