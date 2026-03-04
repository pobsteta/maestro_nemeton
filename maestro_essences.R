#!/usr/bin/env Rscript
# =============================================================================
# maestro_essences.R
# Reconnaissance des essences forestieres a partir d'une zone d'interet (AOI)
# en utilisant le modele MAESTRO de l'IGNF sur Hugging Face
# =============================================================================
#
# Utilisation :
#   Rscript maestro_essences.R --aoi data/aoi.gpkg
#   Rscript maestro_essences.R --aoi data/aoi.gpkg --millesime_ortho 2023 --millesime_irc 2023
#
# Les donnees (ortho RVB, ortho IRC, MNT 1m) sont telechargees automatiquement
# depuis la Geoplateforme IGN via WMS-R.
#
# Pre-requis :
#   - R >= 4.1
#   - Packages R : hfhub, sf, terra, curl, fs, reticulate, jsonlite, optparse
#   - Python >= 3.11 avec : torch, numpy, rasterio
# =============================================================================

# --- Installation des packages si necessaire ---
installer_si_absent <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(sprintf("Installation du package '%s'...", pkg))
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

paquets_requis <- c("hfhub", "sf", "terra", "curl", "fs",
                     "reticulate", "jsonlite", "optparse")
invisible(lapply(paquets_requis, installer_si_absent))

library(hfhub)
library(sf)
library(terra)
library(curl)
library(fs)
library(reticulate)
library(jsonlite)
library(optparse)

# =============================================================================
# 1. CONFIGURATION
# =============================================================================

# --- IGN Geoplateforme WMS-R ---
IGN_WMS_URL      <- "https://data.geopf.fr/wms-r"
IGN_LAYER_ORTHO  <- "ORTHOIMAGERY.ORTHOPHOTOS"
IGN_LAYER_IRC    <- "ORTHOIMAGERY.ORTHOPHOTOS.IRC"
IGN_LAYER_MNT    <- "ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES"

# --- Limites WMS ---
WMS_MAX_PX <- 4096

# --- Resolution ---
RES_IGN <- 0.2  # BD ORTHO IGN (0.2 m)

# --- Patches ---
PATCH_SIZE <- 250  # 250 px a 0.2 m = 50 m

# Classes d'essences forestieres PureForest (13 classes, 18 especes)
# Source : BD Foret V2 / IGN
ESSENCES <- data.frame(
  code = 0:12,
  classe = c(
    "Chene decidue",        # Quercus petraea, Q. robur, Q. pubescens
    "Chene vert",           # Quercus ilex
    "Hetre",                # Fagus sylvatica
    "Chataignier",          # Castanea sativa
    "Pin maritime",         # Pinus pinaster
    "Pin sylvestre",        # Pinus sylvestris
    "Pin laricio/noir",     # Pinus nigra (laricio, noir)
    "Pin d'Alep",           # Pinus halepensis
    "Epicea",               # Picea abies
    "Sapin",                # Abies alba
    "Douglas",              # Pseudotsuga menziesii
    "Meleze",               # Larix decidua, L. kaempferi
    "Peuplier"              # Populus spp.
  ),
  nom_latin = c(
    "Quercus spp. (deciduous)",
    "Quercus ilex",
    "Fagus sylvatica",
    "Castanea sativa",
    "Pinus pinaster",
    "Pinus sylvestris",
    "Pinus nigra",
    "Pinus halepensis",
    "Picea abies",
    "Abies alba",
    "Pseudotsuga menziesii",
    "Larix spp.",
    "Populus spp."
  ),
  type = c(
    "feuillu", "feuillu", "feuillu", "feuillu",
    "resineux", "resineux", "resineux", "resineux",
    "resineux", "resineux", "resineux", "resineux",
    "feuillu"
  ),
  stringsAsFactors = FALSE
)

# --- Arguments en ligne de commande ---
option_list <- list(
  make_option(c("-a", "--aoi"), type = "character", default = "data/aoi.gpkg",
              help = "Chemin vers le fichier GeoPackage de la zone d'interet [default: %default]"),
  make_option(c("-o", "--output"), type = "character", default = "outputs",
              help = "Repertoire de sortie [default: %default]"),
  make_option(c("-m", "--model"), type = "character",
              default = "IGNF/MAESTRO_FLAIR-HUB_base",
              help = "Identifiant du modele Hugging Face [default: %default]"),
  make_option(c("--millesime_ortho"), type = "integer", default = NULL,
              help = "Millesime de l'ortho RVB (NULL = mosaique la plus recente)"),
  make_option(c("--millesime_irc"), type = "integer", default = NULL,
              help = "Millesime de l'ortho IRC (NULL = mosaique la plus recente)"),
  make_option(c("-s", "--patch_size"), type = "integer", default = 250L,
              help = "Taille des patches en pixels [default: %default]"),
  make_option(c("--resolution"), type = "double", default = 0.2,
              help = "Resolution spatiale en metres [default: %default]"),
  make_option(c("--gpu"), action = "store_true", default = FALSE,
              help = "Utiliser le GPU (CUDA) si disponible"),
  make_option(c("--token"), type = "character", default = NULL,
              help = "Token Hugging Face (ou definir HUGGING_FACE_HUB_TOKEN)")
)

opt <- parse_args(OptionParser(option_list = option_list))

# =============================================================================
# 2. CHARGEMENT DE L'AOI
# =============================================================================

#' Charger l'AOI depuis un fichier GeoPackage
#'
#' @param gpkg_path Chemin vers le fichier .gpkg
#' @return sf object en Lambert-93 (EPSG:2154)
load_aoi <- function(gpkg_path) {
  if (!file.exists(gpkg_path)) {
    stop("Fichier AOI introuvable: ", gpkg_path)
  }

  layers <- st_layers(gpkg_path)
  message("Couches dans ", basename(gpkg_path), ": ",
          paste(layers$name, collapse = ", "))

  aoi <- st_read(gpkg_path, layer = layers$name[1], quiet = TRUE)
  message(sprintf("AOI chargee: %d entite(s), CRS: %s",
                   nrow(aoi), st_crs(aoi)$Name))

  # Reprojection en Lambert-93 si necessaire
  if (is.na(st_crs(aoi)$epsg) || st_crs(aoi)$epsg != 2154) {
    message("Reprojection vers Lambert-93 (EPSG:2154)...")
    aoi <- st_transform(aoi, 2154)
  }

  aoi_union <- st_union(aoi)
  bbox <- st_bbox(aoi_union)
  message(sprintf("Emprise Lambert-93: [%.0f, %.0f] - [%.0f, %.0f]",
                   bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"]))
  message(sprintf("Surface: %.2f ha",
                   as.numeric(st_area(aoi_union)) / 10000))

  return(aoi)
}

# =============================================================================
# 3. TELECHARGEMENT DES DONNEES IGN VIA WMS-R
#    (adapte de pobsteta/flair_hub_nemeton)
# =============================================================================

#' Construire le nom de couche WMS IGN selon le millesime
#'
#' @param type "ortho" ou "irc"
#' @param millesime NULL (mosaique la plus recente) ou entier (ex: 2023)
#' @return Nom de couche WMS
ign_layer_name <- function(type = c("ortho", "irc"), millesime = NULL) {
  type <- match.arg(type)
  if (is.null(millesime)) {
    if (type == "ortho") return(IGN_LAYER_ORTHO)
    else                 return(IGN_LAYER_IRC)
  }
  millesime <- as.character(millesime)
  if (type == "ortho") {
    return(paste0("ORTHOIMAGERY.ORTHOPHOTOS", millesime))
  } else {
    return(paste0("ORTHOIMAGERY.ORTHOPHOTOS.IRC.", millesime))
  }
}

#' Telecharger une tuile WMS IGN
#'
#' @param bbox c(xmin, ymin, xmax, ymax) en Lambert-93
#' @param layer Couche WMS
#' @param res_m Resolution en metres
#' @param dest_file Fichier de sortie
#' @param styles Style WMS ("" = defaut, "normal" = valeurs brutes pour elevation)
#' @return SpatRaster ou NULL si echec
download_wms_tile <- function(bbox, layer, res_m = RES_IGN, dest_file,
                               styles = "", max_retries = 3) {
  xmin <- bbox[1]; ymin <- bbox[2]; xmax <- bbox[3]; ymax <- bbox[4]

  width  <- round((xmax - xmin) / res_m)
  height <- round((ymax - ymin) / res_m)

  # WMS 1.3.0 avec CRS EPSG:2154 : BBOX = xmin,ymin,xmax,ymax
  wms_url <- paste0(
    IGN_WMS_URL, "?",
    "SERVICE=WMS&VERSION=1.3.0&REQUEST=GetMap",
    "&LAYERS=", layer,
    "&CRS=EPSG:2154",
    "&BBOX=", paste(xmin, ymin, xmax, ymax, sep = ","),
    "&WIDTH=", width,
    "&HEIGHT=", height,
    "&FORMAT=image/geotiff",
    "&STYLES=", styles
  )

  # Handle curl avec HTTP/1.1 force (evite les erreurs HTTP/2 du serveur IGN)
  h <- curl::new_handle()
  curl::handle_setopt(h, http_version = 2L)  # CURL_HTTP_VERSION_1_1

  tmp_file <- tempfile(fileext = ".tif")

  for (attempt in seq_len(max_retries)) {
    result <- tryCatch({
      curl::curl_download(url = wms_url, destfile = tmp_file,
                          quiet = TRUE, handle = h)

      r <- rast(tmp_file)

      # Assigner le CRS et l'emprise si necessaire
      if (is.na(crs(r)) || crs(r) == "") {
        crs(r) <- "EPSG:2154"
      }
      ext(r) <- ext(xmin, xmax, ymin, ymax)

      # Ecrire le fichier final et re-lire
      writeRaster(r, dest_file, overwrite = TRUE)
      r <- rast(dest_file)
      unlink(tmp_file)

      return(r)
    }, error = function(e) {
      e
    })

    if (!inherits(result, "error")) return(result)

    # Retry avec backoff exponentiel
    if (attempt < max_retries) {
      wait_s <- 2^attempt
      message(sprintf("  Retry %d/%d dans %ds (%s)",
                       attempt, max_retries, wait_s, result$message))
      Sys.sleep(wait_s)
    } else {
      unlink(tmp_file)
      warning("Echec WMS apres ", max_retries, " tentatives: ", result$message)
      return(NULL)
    }
  }
}

#' Telecharger une couche WMS IGN complete pour une emprise (tuilage automatique)
#'
#' Subdivise les grandes emprises en tuiles respectant la limite WMS de 4096 px.
#'
#' @param bbox c(xmin, ymin, xmax, ymax) en Lambert-93
#' @param layer Couche WMS
#' @param res_m Resolution en metres
#' @param output_dir Repertoire de sortie
#' @param prefix Prefixe pour les fichiers
#' @param styles Style WMS
#' @return SpatRaster mosaique
download_ign_tiled <- function(bbox, layer, res_m = RES_IGN,
                                output_dir, prefix = "ortho",
                                styles = "") {
  xmin <- bbox[1]; ymin <- bbox[2]; xmax <- bbox[3]; ymax <- bbox[4]
  tile_size_m <- WMS_MAX_PX * res_m

  x_starts <- seq(xmin, xmax, by = tile_size_m)
  y_starts <- seq(ymin, ymax, by = tile_size_m)

  n_tiles <- length(x_starts) * length(y_starts)
  message(sprintf("Telechargement %s: %d tuile(s) WMS...", prefix, n_tiles))

  tile_rasters <- list()
  idx <- 1

  for (x0 in x_starts) {
    for (y0 in y_starts) {
      x1 <- min(x0 + tile_size_m, xmax)
      y1 <- min(y0 + tile_size_m, ymax)

      if ((x1 - x0) < res_m * 2 || (y1 - y0) < res_m * 2) next

      tile_bbox <- c(x0, y0, x1, y1)
      tile_file <- file.path(output_dir,
                              sprintf("%s_tile_%03d.tif", prefix, idx))

      message(sprintf("  Tuile %d/%d [%.0f,%.0f - %.0f,%.0f]...",
                       idx, n_tiles, x0, y0, x1, y1))

      r <- download_wms_tile(tile_bbox, layer, res_m, tile_file,
                              styles = styles)
      if (!is.null(r)) {
        tile_rasters[[idx]] <- r
      }
      idx <- idx + 1
    }
  }

  if (length(tile_rasters) == 0) {
    stop("Aucune tuile WMS telechargee avec succes.")
  }

  if (length(tile_rasters) == 1) {
    mosaic <- tile_rasters[[1]]
  } else {
    message("Mosaiquage de ", length(tile_rasters), " tuiles...")
    mosaic <- do.call(merge, tile_rasters)
  }

  return(mosaic)
}

#' Verifier qu'un raster WMS contient des donnees reelles
#'
#' @param r SpatRaster a valider
#' @param min_pct Pourcentage minimum de pixels non-vides requis
#' @return TRUE si le raster contient suffisamment de donnees
validate_wms_data <- function(r, min_pct = 5) {
  if (is.null(r)) return(FALSE)
  vals <- values(r[[1]])
  n_valid <- sum(!is.na(vals) & vals > 0)
  pct <- n_valid / length(vals) * 100
  if (pct < min_pct) {
    message(sprintf("  Donnees insuffisantes : %.1f%% de pixels valides (%d/%d)",
                    pct, n_valid, length(vals)))
  }
  return(pct >= min_pct)
}

#' Telecharger les ortho RVB et IRC pour une AOI
#'
#' Gestion du cache : si ortho_rvb.tif et ortho_irc.tif existent deja,
#' ils sont reutilises sans re-telechargement.
#'
#' @param aoi sf object (AOI en Lambert-93)
#' @param output_dir Repertoire de sortie
#' @param millesime_ortho NULL ou entier (annee de l'ortho RVB)
#' @param millesime_irc NULL ou entier (annee de l'ortho IRC)
#' @return Liste avec rvb, irc (SpatRaster), chemins et info millesime
download_ortho_for_aoi <- function(aoi, output_dir,
                                    millesime_ortho = NULL,
                                    millesime_irc = NULL) {
  dir_create(output_dir)

  layer_ortho <- ign_layer_name("ortho", millesime_ortho)
  layer_irc   <- ign_layer_name("irc",   millesime_irc)
  label_ortho <- if (is.null(millesime_ortho)) "plus recent" else millesime_ortho
  label_irc   <- if (is.null(millesime_irc))   "plus recent" else millesime_irc

  rvb_path <- file.path(output_dir, "ortho_rvb.tif")
  irc_path <- file.path(output_dir, "ortho_irc.tif")

  # Cache : reutiliser les fichiers existants
  if (file.exists(rvb_path) && file.exists(irc_path)) {
    message("\n=== Ortho IGN deja presentes (cache) ===")
    message(sprintf("  RVB: %s", rvb_path))
    message(sprintf("  IRC: %s", irc_path))

    rvb <- rast(rvb_path)
    irc <- rast(irc_path)
    names(rvb)[1:min(3, nlyr(rvb))] <- c("Rouge", "Vert", "Bleu")[1:min(3, nlyr(rvb))]
    names(irc)[1:min(3, nlyr(irc))] <- c("PIR", "Rouge", "Vert")[1:min(3, nlyr(irc))]

    return(list(rvb = rvb, irc = irc,
                rvb_path = rvb_path, irc_path = irc_path,
                millesime_ortho = millesime_ortho,
                millesime_irc = millesime_irc))
  }

  bbox <- as.numeric(st_bbox(st_union(aoi)))

  message(sprintf("\n=== Telechargement ortho IGN pour l'AOI ==="))
  message(sprintf("Emprise: %.0f, %.0f - %.0f, %.0f (Lambert-93)",
                   bbox[1], bbox[2], bbox[3], bbox[4]))
  message(sprintf("Taille: %.0f x %.0f m (%.2f ha)",
                   bbox[3] - bbox[1], bbox[4] - bbox[2],
                   (bbox[3] - bbox[1]) * (bbox[4] - bbox[2]) / 10000))
  message(sprintf("Millesime RVB: %s (couche: %s)", label_ortho, layer_ortho))
  message(sprintf("Millesime IRC: %s (couche: %s)", label_irc, layer_irc))

  # --- RVB (avec fallback si millesime indisponible) ---
  message("\n--- Ortho RVB ---")
  rvb <- tryCatch(
    download_ign_tiled(bbox, layer = layer_ortho, res_m = RES_IGN,
                       output_dir = output_dir, prefix = "rvb"),
    error = function(e) {
      message("  Erreur telechargement RVB: ", e$message)
      NULL
    }
  )

  # Fallback si millesime indisponible
  if (!is.null(millesime_ortho) &&
      (is.null(rvb) || !validate_wms_data(rvb))) {
    message(sprintf("  Millesime %s indisponible, fallback sur %s",
                    millesime_ortho, IGN_LAYER_ORTHO))
    layer_ortho <- IGN_LAYER_ORTHO
    tile_files <- dir_ls(output_dir, glob = "rvb_tile_*.tif")
    if (length(tile_files) > 0) file_delete(tile_files)
    rvb <- download_ign_tiled(bbox, layer = IGN_LAYER_ORTHO, res_m = RES_IGN,
                               output_dir = output_dir, prefix = "rvb")
  }
  if (is.null(rvb)) stop("Impossible de telecharger l'ortho RVB")
  names(rvb)[1:min(3, nlyr(rvb))] <- c("Rouge", "Vert", "Bleu")[1:min(3, nlyr(rvb))]

  # --- IRC (avec fallback si millesime indisponible) ---
  message("\n--- Ortho IRC ---")
  irc <- tryCatch(
    download_ign_tiled(bbox, layer = layer_irc, res_m = RES_IGN,
                       output_dir = output_dir, prefix = "irc"),
    error = function(e) {
      message("  Erreur telechargement IRC: ", e$message)
      NULL
    }
  )

  if (!is.null(millesime_irc) &&
      (is.null(irc) || !validate_wms_data(irc))) {
    message(sprintf("  Millesime %s indisponible, fallback sur %s",
                    millesime_irc, IGN_LAYER_IRC))
    layer_irc <- IGN_LAYER_IRC
    tile_files <- dir_ls(output_dir, glob = "irc_tile_*.tif")
    if (length(tile_files) > 0) file_delete(tile_files)
    irc <- download_ign_tiled(bbox, layer = IGN_LAYER_IRC, res_m = RES_IGN,
                               output_dir = output_dir, prefix = "irc")
  }
  if (is.null(irc)) stop("Impossible de telecharger l'ortho IRC")
  names(irc)[1:min(3, nlyr(irc))] <- c("PIR", "Rouge", "Vert")[1:min(3, nlyr(irc))]

  # Decouper aux limites exactes de l'AOI
  aoi_vect <- vect(st_union(aoi))
  rvb <- crop(rvb, aoi_vect)
  irc <- crop(irc, aoi_vect)

  # Sauvegarder les mosaiques finales
  writeRaster(rvb, rvb_path, overwrite = TRUE)
  writeRaster(irc, irc_path, overwrite = TRUE)

  # Re-lire depuis les fichiers sauvegardes (terra est file-backed)
  rvb <- rast(rvb_path)
  irc <- rast(irc_path)
  names(rvb)[1:min(3, nlyr(rvb))] <- c("Rouge", "Vert", "Bleu")[1:min(3, nlyr(rvb))]
  names(irc)[1:min(3, nlyr(irc))] <- c("PIR", "Rouge", "Vert")[1:min(3, nlyr(irc))]

  message(sprintf("\nRVB sauvegarde: %s (%d x %d px)", rvb_path, ncol(rvb), nrow(rvb)))
  message(sprintf("IRC sauvegarde: %s (%d x %d px)", irc_path, ncol(irc), nrow(irc)))

  # Nettoyer les tuiles temporaires
  tile_files <- dir_ls(output_dir, glob = "*_tile_*.tif")
  if (length(tile_files) > 0) file_delete(tile_files)

  return(list(rvb = rvb, irc = irc,
              rvb_path = rvb_path, irc_path = irc_path,
              millesime_ortho = millesime_ortho,
              millesime_irc = millesime_irc))
}

#' Telecharger le MNT (RGE ALTI 1m) pour une AOI
#'
#' @param aoi sf object en Lambert-93
#' @param output_dir Repertoire de sortie
#' @param rgbi SpatRaster de reference pour le reechantillonnage a 0.2m
#' @return Liste avec mnt (SpatRaster) et mnt_path
download_mnt_for_aoi <- function(aoi, output_dir, rgbi = NULL) {
  dir_create(output_dir)

  mnt_path <- file.path(output_dir, "mnt_1m.tif")

  # Cache : reutiliser le fichier existant
  if (file.exists(mnt_path)) {
    message("\n=== MNT deja telecharge (cache) ===")
    mnt <- rast(mnt_path)
    names(mnt) <- "MNT"
    message(sprintf("MNT: %s (%d x %d px)", mnt_path, ncol(mnt), nrow(mnt)))
    return(list(mnt = mnt, mnt_path = mnt_path))
  }

  bbox <- as.numeric(st_bbox(st_union(aoi)))
  message(sprintf("\n=== Telechargement MNT IGN (RGE ALTI 1m via WMS-R) ==="))

  # MNT (DTM - terrain nu) a 1m de resolution native
  mnt <- tryCatch(
    download_ign_tiled(bbox, layer = IGN_LAYER_MNT, res_m = 1,
                        output_dir = output_dir, prefix = "mnt",
                        styles = "normal"),
    error = function(e) {
      message("  MNT non telecharge: ", e$message)
      NULL
    }
  )

  if (is.null(mnt)) {
    warning("Aucune donnee MNT telechargee.")
    return(NULL)
  }

  # Decouper aux limites de l'AOI
  aoi_vect <- vect(st_union(aoi))
  mnt <- crop(mnt, aoi_vect)
  names(mnt) <- "MNT"

  # Reechantillonner vers la grille aerienne (0.2m) si fournie
  if (!is.null(rgbi)) {
    message("Reechantillonnage MNT de 1m vers 0.2m...")
    mnt <- resample(mnt, rgbi, method = "bilinear")
  }

  # Sauvegarder
  writeRaster(mnt, mnt_path, overwrite = TRUE, gdal = c("COMPRESS=LZW"))

  # Statistiques
  mnt_vals <- values(mnt, na.rm = TRUE)
  if (length(mnt_vals) > 0 && any(is.finite(mnt_vals))) {
    message(sprintf("  Altitude MNT: %.0f - %.0f m",
                     min(mnt_vals, na.rm = TRUE),
                     max(mnt_vals, na.rm = TRUE)))
  }

  message(sprintf("MNT sauvegarde: %s (%d x %d px)", mnt_path, ncol(mnt), nrow(mnt)))

  # Nettoyer les tuiles temporaires
  tile_files <- dir_ls(output_dir, glob = "mnt_tile_*.tif")
  if (length(tile_files) > 0) file_delete(tile_files)

  # Re-lire depuis le fichier sauvegarde
  mnt <- rast(mnt_path)
  names(mnt) <- "MNT"

  return(list(mnt = mnt, mnt_path = mnt_path))
}

# =============================================================================
# 4. COMBINAISON RVB + IRC + MNT
# =============================================================================

#' Combiner les ortho RVB et IRC en image 4 bandes RGBI
#'
#' @param rvb SpatRaster ortho RVB (3 bandes : Rouge, Vert, Bleu)
#' @param irc SpatRaster ortho IRC (3 bandes : PIR, Rouge, Vert)
#' @return SpatRaster 4 bandes (Rouge, Vert, Bleu, PIR)
combine_rvb_irc <- function(rvb, irc) {
  message("Combinaison RVB + PIR en image 4 bandes RGBI...")

  if (!compareGeom(rvb, irc, stopOnError = FALSE)) {
    message("  Reechantillonnage IRC sur la grille RVB...")
    irc <- resample(irc, rvb, method = "bilinear")
  }

  pir <- irc[[1]]
  names(pir) <- "PIR"

  rgbi <- c(rvb[[1]], rvb[[2]], rvb[[3]], pir)
  names(rgbi) <- c("Rouge", "Vert", "Bleu", "PIR")

  message(sprintf("  Image RGBI: %d x %d px, %d bandes",
                   ncol(rgbi), nrow(rgbi), nlyr(rgbi)))
  return(rgbi)
}

#' Combiner RGBI + MNT en image 5 bandes
#'
#' @param rgbi SpatRaster 4 bandes (R, G, B, PIR)
#' @param mnt SpatRaster 1 bande (MNT)
#' @return SpatRaster 5 bandes (Rouge, Vert, Bleu, PIR, MNT)
combine_rgbi_mnt <- function(rgbi, mnt) {
  message("Ajout du MNT comme 5eme bande...")

  if (!compareGeom(rgbi, mnt, stopOnError = FALSE)) {
    message("  Reechantillonnage MNT sur la grille RGBI...")
    mnt <- resample(mnt, rgbi, method = "bilinear")
  }

  rgbi_mnt <- c(rgbi, mnt)
  names(rgbi_mnt) <- c("Rouge", "Vert", "Bleu", "PIR", "MNT")

  message(sprintf("  Image RGBI+MNT: %d x %d px, %d bandes",
                   ncol(rgbi_mnt), nrow(rgbi_mnt), nlyr(rgbi_mnt)))
  return(rgbi_mnt)
}

# =============================================================================
# 5. TELECHARGEMENT DU MODELE MAESTRO VIA HFHUB
# =============================================================================

#' Trouver le nom du fichier checkpoint dans un depot HF via l'API
#'
#' @param hf_repo Identifiant du depot HF
#' @return Nom du fichier checkpoint
find_checkpoint_name <- function(hf_repo) {
  url <- paste0("https://huggingface.co/api/models/", hf_repo)
  resp <- tryCatch({
    tmp <- tempfile()
    h <- curl::new_handle()
    curl::curl_download(url, tmp, handle = h)
    info <- jsonlite::fromJSON(tmp)
    unlink(tmp)
    info
  }, error = function(e) NULL)
  if (is.null(resp)) return(NULL)

  files <- resp$siblings$rfilename
  ckpt_files <- files[grepl("\\.(ckpt|pth|pt|bin|safetensors)$", files)]
  if (length(ckpt_files) == 0) return(NULL)

  # Prioriser : .ckpt > .pth > .pt > .bin > .safetensors
  ext_priority <- c("\\.ckpt$", "\\.pth$", "\\.pt$", "\\.bin$", "\\.safetensors$")
  for (pat in ext_priority) {
    matches <- ckpt_files[grepl(pat, ckpt_files)]
    if (length(matches) > 0) return(matches[1])
  }
  return(ckpt_files[1])
}

#' Telecharger le modele MAESTRO depuis Hugging Face via hfhub
#'
#' @param repo_id Identifiant du depot HF (ex: "IGNF/MAESTRO_FLAIR-HUB_base")
#' @param token Token Hugging Face (optionnel)
#' @return Liste avec config et weights (chemins locaux)
telecharger_modele <- function(repo_id, token = NULL) {
  message("=== Telechargement du modele MAESTRO depuis Hugging Face ===")
  message(sprintf("Repository : %s", repo_id))

  if (!is.null(token)) {
    Sys.setenv(HUGGING_FACE_HUB_TOKEN = token)
  }

  fichiers_modele <- list()

  # Fichier de configuration
  tryCatch({
    fichiers_modele$config <- hfhub::hub_download(
      repo_id = repo_id,
      filename = "config.json"
    )
    message(sprintf("  Config : %s", fichiers_modele$config))
  }, error = function(e) {
    message("  Pas de config.json")
  })

  # Poids du modele : detecter le nom via l'API
  ckpt_name <- find_checkpoint_name(repo_id)
  if (!is.null(ckpt_name)) {
    message(sprintf("  Telechargement via hfhub : %s", ckpt_name))
    tryCatch({
      fichiers_modele$weights <- hfhub::hub_download(repo_id, ckpt_name)
      message(sprintf("  Poids : %s", fichiers_modele$weights))
    }, error = function(e) {
      message("  hfhub echoue: ", e$message)
    })
  }

  # Fallback : essayer les noms standards
  if (is.null(fichiers_modele$weights)) {
    noms_poids <- c("model.safetensors", "pytorch_model.bin",
                     "model.pt", "checkpoint.pth")
    for (nom in noms_poids) {
      tryCatch({
        fichiers_modele$weights <- hfhub::hub_download(repo_id, nom)
        message(sprintf("  Poids : %s", fichiers_modele$weights))
        break
      }, error = function(e) NULL)
    }
  }

  # Dernier fallback : snapshot complet
  if (is.null(fichiers_modele$weights)) {
    message("  Tentative de snapshot complet du repository...")
    tryCatch({
      snapshot_path <- hfhub::hub_snapshot(repo_id = repo_id)
      fichiers_modele$snapshot <- snapshot_path
      message(sprintf("  Snapshot : %s", snapshot_path))
    }, error = function(e) {
      stop("Impossible de telecharger le modele : ", e$message)
    })
  }

  return(fichiers_modele)
}

# =============================================================================
# 6. DECOUPAGE EN PATCHES ET EXTRACTION
# =============================================================================

#' Creer la grille de patches pour l'inference
#'
#' @param aoi sf object en Lambert-93
#' @param taille_patch_m Taille des patches en metres
#' @return sf grille de patches intersectant l'AOI
creer_grille_patches <- function(aoi, taille_patch_m = 50) {
  message("=== Creation de la grille de patches ===")

  bbox <- st_bbox(aoi)

  x_coords <- seq(bbox["xmin"], bbox["xmax"], by = taille_patch_m)
  y_coords <- seq(bbox["ymin"], bbox["ymax"], by = taille_patch_m)

  patches <- expand.grid(x = x_coords, y = y_coords)
  patches$xmax <- patches$x + taille_patch_m
  patches$ymax <- patches$y + taille_patch_m

  creer_poly <- function(i) {
    st_polygon(list(matrix(c(
      patches$x[i],    patches$y[i],
      patches$xmax[i], patches$y[i],
      patches$xmax[i], patches$ymax[i],
      patches$x[i],    patches$ymax[i],
      patches$x[i],    patches$y[i]
    ), ncol = 2, byrow = TRUE)))
  }

  geometries <- lapply(seq_len(nrow(patches)), creer_poly)
  grille <- st_sf(
    id = seq_len(nrow(patches)),
    geometry = st_sfc(geometries, crs = st_crs(aoi))
  )

  # Ne garder que les patches qui intersectent l'AOI
  intersects <- st_intersects(grille, st_union(aoi), sparse = FALSE)[, 1]
  grille <- grille[intersects, ]
  grille$id <- seq_len(nrow(grille))

  message(sprintf("  Patches generes : %d", nrow(grille)))
  message(sprintf("  Taille patch : %.0f m x %.0f m", taille_patch_m, taille_patch_m))

  return(grille)
}

#' Extraire les patches raster depuis un SpatRaster
#'
#' @param r SpatRaster multi-bandes
#' @param grille sf grille de patches
#' @param taille_pixels Taille cible en pixels
#' @return Liste de matrices numpy-ready (pixels x bandes)
extraire_patches_raster <- function(r, grille, taille_pixels = 250) {
  message("=== Extraction des patches raster ===")
  message(sprintf("  Raster : %d bandes, %d x %d px", nlyr(r), ncol(r), nrow(r)))

  patches_data <- list()

  for (i in seq_len(nrow(grille))) {
    ext_patch <- ext(st_bbox(grille[i, ]))
    patch <- crop(r, ext_patch)

    # Redimensionner au nombre de pixels souhaite
    if (ncol(patch) != taille_pixels || nrow(patch) != taille_pixels) {
      template <- rast(
        ext = ext_patch,
        nrows = taille_pixels, ncols = taille_pixels,
        crs = crs(r),
        nlyrs = nlyr(r)
      )
      patch <- resample(patch, template, method = "bilinear")
    }

    # Retourner sous forme matrice (H*W, C) reorganisee en (C, H, W) cote Python
    patches_data[[i]] <- values(patch)

    if (i %% 100 == 0 || i == nrow(grille)) {
      message(sprintf("  Patches extraits : %d / %d", i, nrow(grille)))
    }
  }

  message(sprintf("  Total patches extraits : %d", length(patches_data)))
  return(patches_data)
}

# =============================================================================
# 7. INFERENCE AVEC LE MODELE MAESTRO (via reticulate/Python)
# =============================================================================

#' Configurer l'environnement Python (pattern FLAIR-HUB)
configurer_python <- function(envname = "maestro") {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Le package 'reticulate' est requis. Installez-le avec : ",
         "install.packages('reticulate')")
  }
  library(reticulate)

  use_condaenv(envname, required = TRUE)
  message("Environnement conda configure: ", envname)

  # Verifier les modules disponibles
  modules <- c("torch", "numpy", "safetensors")
  ok <- TRUE
  for (mod in modules) {
    avail <- py_module_available(mod)
    message(sprintf("  Python %s: %s", mod, ifelse(avail, "OK", "MANQUANT")))
    if (!avail) ok <- FALSE
  }

  if (!ok) {
    stop("Modules Python manquants. Installez-les dans l'env '", envname, "':\n",
         "  conda activate ", envname, "\n",
         "  pip install torch numpy safetensors")
  }

  message("  Python configure.")
}

#' Executer l'inference MAESTRO sur les patches
#'
#' @param patches_data Liste de matrices de patches
#' @param fichiers_modele Liste avec config et weights
#' @param n_classes Nombre de classes (13 pour PureForest)
#' @param n_bands Nombre de bandes d'entree (4 = RGBI, 5 = RGBI+MNT)
#' @param utiliser_gpu Utiliser le GPU CUDA
#' @return Liste de predictions (codes de classes)
executer_inference <- function(patches_data, fichiers_modele, n_classes = 13L,
                                n_bands = 5L, utiliser_gpu = FALSE) {
  message("=== Inference MAESTRO ===")

  # Importer le module d'inference
  maestro <- reticulate::import_from_path("maestro_inference", path = ".")

  # Determiner le device
  torch <- reticulate::import("torch")
  np <- reticulate::import("numpy")

  device_str <- if (utiliser_gpu && torch$cuda$is_available()) {
    message("  Utilisation du GPU (CUDA)")
    "cuda"
  } else {
    message("  Utilisation du CPU")
    "cpu"
  }

  # Charger le modele
  chemin_poids <- fichiers_modele$weights %||% fichiers_modele$snapshot
  if (is.null(chemin_poids)) {
    stop("Impossible de trouver les poids du modele.")
  }

  modele <- maestro$charger_modele(
    chemin_poids = chemin_poids,
    n_classes = as.integer(n_classes),
    device = device_str,
    in_channels = as.integer(n_bands)
  )

  # Inference par batch
  message("  Lancement de l'inference...")
  n_patches <- length(patches_data)
  predictions <- vector("list", n_patches)
  batch_size <- 16L
  n_batches <- ceiling(n_patches / batch_size)
  patch_size <- as.integer(sqrt(nrow(patches_data[[1]])))

  for (b in seq_len(n_batches)) {
    debut <- (b - 1L) * batch_size + 1L
    fin <- min(b * batch_size, n_patches)
    indices <- debut:fin

    # Preparer le batch : (B, H*W, C) -> reshape en Python
    batch_arrays <- lapply(patches_data[indices], function(p) {
      np$array(p, dtype = np$float32)
    })
    batch_np <- np$stack(batch_arrays)

    # Reshape (B, H*W, C) -> (B, C, H, W) et predire
    preds <- maestro$predire_batch_from_values(
      modele, batch_np,
      patch_h = patch_size, patch_w = patch_size,
      device = device_str
    )

    for (j in seq_along(indices)) {
      predictions[[indices[j]]] <- preds[j]
    }

    if (b %% 10 == 0 || b == n_batches) {
      message(sprintf("  Batch %d / %d traite", b, n_batches))
    }
  }

  return(predictions)
}

# =============================================================================
# 8. POST-TRAITEMENT ET EXPORT
# =============================================================================

#' Assembler les resultats dans un GeoPackage
assembler_resultats <- function(grille, predictions, essences, dossier_sortie) {
  message("=== Assemblage des resultats ===")
  dir.create(dossier_sortie, showWarnings = FALSE, recursive = TRUE)

  grille$code_essence <- unlist(predictions)
  grille <- merge(grille, essences, by.x = "code_essence", by.y = "code", all.x = TRUE)

  chemin_gpkg <- file.path(dossier_sortie, "essences_forestieres.gpkg")
  st_write(grille, chemin_gpkg, delete_dsn = TRUE, quiet = TRUE)
  message(sprintf("  GeoPackage : %s", chemin_gpkg))

  # Statistiques
  message("\n=== Statistiques des essences detectees ===")
  stats <- as.data.frame(table(grille$classe))
  names(stats) <- c("Essence", "Nombre_patches")
  stats$Proportion <- round(stats$Nombre_patches / sum(stats$Nombre_patches) * 100, 1)
  stats <- stats[order(-stats$Nombre_patches), ]
  print(stats, row.names = FALSE)

  chemin_csv <- file.path(dossier_sortie, "statistiques_essences.csv")
  write.csv(stats, chemin_csv, row.names = FALSE)
  message(sprintf("  Statistiques CSV : %s", chemin_csv))

  return(grille)
}

#' Creer le raster de classification
creer_carte_raster <- function(grille, resolution, dossier_sortie) {
  message("=== Creation du raster de classification ===")

  bbox <- st_bbox(grille)
  template <- rast(
    xmin = bbox["xmin"], xmax = bbox["xmax"],
    ymin = bbox["ymin"], ymax = bbox["ymax"],
    res = resolution,
    crs = st_crs(grille)$wkt
  )

  raster_classe <- rasterize(vect(grille), template, field = "code_essence")

  chemin_tif <- file.path(dossier_sortie, "essences_forestieres.tif")
  writeRaster(raster_classe, chemin_tif, overwrite = TRUE)
  message(sprintf("  Raster GeoTIFF : %s", chemin_tif))

  return(raster_classe)
}

# =============================================================================
# 9. PIPELINE PRINCIPAL
# =============================================================================

main <- function() {
  message("========================================================")
  message(" MAESTRO - Reconnaissance des essences forestieres")
  message(" Modele IGNF via Hugging Face (hfhub)")
  message(" Donnees ortho + MNT via Geoplateforme IGN (WMS-R)")
  message("========================================================\n")

  # Etape 1 : Charger l'AOI
  aoi <- load_aoi(opt$aoi)

  # Etape 2 : Telecharger les donnees IGN
  ortho <- download_ortho_for_aoi(
    aoi, opt$output,
    millesime_ortho = opt$millesime_ortho,
    millesime_irc = opt$millesime_irc
  )

  # Etape 3 : Combiner RVB + IRC → RGBI (4 bandes)
  rgbi <- combine_rvb_irc(ortho$rvb, ortho$irc)

  # Sauvegarder le RGBI
  rgbi_path <- file.path(opt$output, "ortho_rgbi.tif")
  writeRaster(rgbi, rgbi_path, overwrite = TRUE)
  message(sprintf("RGBI sauvegarde: %s", rgbi_path))

  # Etape 4 : Telecharger le MNT (1m, reechantillonne a 0.2m sur grille RGBI)
  mnt_data <- download_mnt_for_aoi(aoi, opt$output, rgbi = rgbi)

  # Etape 5 : Combiner RGBI + MNT → 5 bandes
  if (!is.null(mnt_data)) {
    image_finale <- combine_rgbi_mnt(rgbi, mnt_data$mnt)
    n_bands <- 5L
  } else {
    message("  MNT non disponible, utilisation des 4 bandes RGBI seules")
    image_finale <- rgbi
    n_bands <- 4L
  }

  # Sauvegarder l'image finale
  finale_path <- file.path(opt$output, "image_finale.tif")
  writeRaster(image_finale, finale_path, overwrite = TRUE,
              gdal = c("COMPRESS=LZW"))
  message(sprintf("Image finale sauvegardee: %s (%d bandes)", finale_path, n_bands))

  # Etape 6 : Telecharger le modele MAESTRO
  fichiers_modele <- telecharger_modele(opt$model, opt$token)

  # Etape 7 : Configurer Python
  configurer_python()

  # Etape 8 : Creer la grille de patches
  taille_patch_m <- opt$patch_size * opt$resolution  # 250px * 0.2m = 50m
  grille <- creer_grille_patches(aoi, taille_patch_m)

  # Etape 9 : Extraire les patches
  patches_data <- extraire_patches_raster(image_finale, grille, opt$patch_size)

  # Etape 10 : Inference
  predictions <- executer_inference(
    patches_data, fichiers_modele,
    n_classes = 13L,
    n_bands = n_bands,
    utiliser_gpu = opt$gpu
  )

  # Etape 11 : Assembler et exporter
  resultats <- assembler_resultats(grille, predictions, ESSENCES, opt$output)

  # Etape 12 : Creer la carte raster
  creer_carte_raster(resultats, opt$resolution, opt$output)

  message("\n========================================================")
  message(" Traitement termine !")
  message(sprintf(" Resultats dans : %s/", opt$output))
  message("  - ortho_rvb.tif        : Orthophoto RVB")
  message("  - ortho_irc.tif        : Orthophoto IRC")
  message("  - ortho_rgbi.tif       : Image 4 bandes RGBI")
  message("  - mnt_1m.tif           : MNT RGE ALTI (reechantillonne 0.2m)")
  message("  - image_finale.tif     : Image multi-bandes pour inference")
  message("  - essences_forestieres.gpkg : Carte vectorielle des essences")
  message("  - essences_forestieres.tif  : Carte raster des essences")
  message("  - statistiques_essences.csv : Statistiques")
  message("========================================================")
}

# Lancer le pipeline
main()
