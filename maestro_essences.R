#!/usr/bin/env Rscript
# =============================================================================
# maestro_essences.R
# Reconnaissance des essences forestieres a partir d'une zone d'interet (AOI)
# en utilisant le modele MAESTRO de l'IGNF sur Hugging Face
# =============================================================================
#
# Utilisation :
#   Rscript maestro_essences.R --aoi aoi.gpkg [--output resultats/] [--model IGNF/MAESTRO_FLAIR-HUB_base]
#
# Pre-requis :
#   - R >= 4.1
#   - Packages R : hfhub, sf, terra, reticulate, jsonlite, optparse
#   - Python >= 3.11 avec : torch, torchvision, rasterio, geopandas, numpy
#   - Donnees raster (orthophotos IRC) couvrant l'AOI
# =============================================================================

# --- Installation des packages si necessaire ---
installer_si_absent <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(sprintf("Installation du package '%s'...", pkg))
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

paquets_requis <- c("hfhub", "sf", "terra", "reticulate", "jsonlite", "optparse")
invisible(lapply(paquets_requis, installer_si_absent))

library(hfhub)
library(sf)
library(terra)
library(reticulate)
library(jsonlite)
library(optparse)

# =============================================================================
# 1. CONFIGURATION
# =============================================================================

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
  make_option(c("-a", "--aoi"), type = "character", default = "aoi.gpkg",
              help = "Chemin vers le fichier GeoPackage de la zone d'interet [default: %default]"),
  make_option(c("-r", "--raster"), type = "character", default = NULL,
              help = "Chemin vers le raster d'entree (ortho IRC, 4 bandes: NIR-R-G-B) [optionnel]"),
  make_option(c("-o", "--output"), type = "character", default = "resultats",
              help = "Repertoire de sortie [default: %default]"),
  make_option(c("-m", "--model"), type = "character",
              default = "IGNF/MAESTRO_FLAIR-HUB_base",
              help = "Identifiant du modele Hugging Face [default: %default]"),
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
# 2. TELECHARGEMENT DU MODELE MAESTRO VIA HFHUB
# =============================================================================

telecharger_modele <- function(repo_id, token = NULL) {
  message("=== Telechargement du modele MAESTRO depuis Hugging Face ===")
  message(sprintf("Repository : %s", repo_id))

  # Configurer le token si fourni
  if (!is.null(token)) {
    Sys.setenv(HUGGING_FACE_HUB_TOKEN = token)
  }

  # Telecharger le snapshot complet du modele
  # hfhub met en cache les fichiers localement
  tryCatch({
    # Essayer de telecharger les fichiers du modele
    fichiers_modele <- list()

    # Fichier de configuration
    fichiers_modele$config <- hfhub::hub_download(
      repo_id = repo_id,
      filename = "config.json"
    )
    message(sprintf("  Config : %s", fichiers_modele$config))

    # Poids du modele (checkpoint PyTorch)
    noms_poids <- c("model.safetensors", "pytorch_model.bin",
                     "model.pt", "checkpoint.pth")

    for (nom in noms_poids) {
      tryCatch({
        fichiers_modele$weights <- hfhub::hub_download(
          repo_id = repo_id,
          filename = nom
        )
        message(sprintf("  Poids : %s", fichiers_modele$weights))
        break
      }, error = function(e) {
        # Essayer le nom suivant
      })
    }

    if (is.null(fichiers_modele$weights)) {
      # Tenter un snapshot complet
      message("  Tentative de snapshot complet du repository...")
      snapshot_path <- hfhub::hub_snapshot(repo_id = repo_id)
      fichiers_modele$snapshot <- snapshot_path
      message(sprintf("  Snapshot : %s", snapshot_path))
    }

    return(fichiers_modele)
  }, error = function(e) {
    stop(sprintf("Erreur lors du telechargement du modele : %s", e$message))
  })
}

# =============================================================================
# 3. LECTURE ET PREPARATION DE LA ZONE D'INTERET
# =============================================================================

lire_aoi <- function(chemin_aoi) {
  message("=== Lecture de la zone d'interet ===")

  if (!file.exists(chemin_aoi)) {
    stop(sprintf("Le fichier AOI '%s' n'existe pas.", chemin_aoi))
  }

  aoi <- sf::st_read(chemin_aoi, quiet = TRUE)
  message(sprintf("  Geometries : %d", nrow(aoi)))
  message(sprintf("  CRS : %s", sf::st_crs(aoi)$input))
  message(sprintf("  Etendue : %s", paste(round(sf::st_bbox(aoi), 2), collapse = ", ")))

  # Reprojecter en Lambert-93 (EPSG:2154) si necessaire
  # (systeme de reference standard pour les donnees IGN France)
  if (sf::st_crs(aoi)$epsg != 2154) {
    message("  Reprojection en Lambert-93 (EPSG:2154)...")
    aoi <- sf::st_transform(aoi, 2154)
  }

  return(aoi)
}

# =============================================================================
# 4. DECOUPAGE EN PATCHES
# =============================================================================

creer_grille_patches <- function(aoi, taille_patch_m = 50, chevauchement = 0) {

  message("=== Creation de la grille de patches ===")

  bbox <- sf::st_bbox(aoi)
  pas <- taille_patch_m * (1 - chevauchement)

  # Generer la grille
  x_coords <- seq(bbox["xmin"], bbox["xmax"], by = pas)
  y_coords <- seq(bbox["ymin"], bbox["ymax"], by = pas)

  patches <- expand.grid(x = x_coords, y = y_coords)
  patches$xmax <- patches$x + taille_patch_m
  patches$ymax <- patches$y + taille_patch_m

  # Creer les polygones des patches
  creer_poly <- function(i) {
    sf::st_polygon(list(matrix(c(
      patches$x[i],    patches$y[i],
      patches$xmax[i], patches$y[i],
      patches$xmax[i], patches$ymax[i],
      patches$x[i],    patches$ymax[i],
      patches$x[i],    patches$y[i]
    ), ncol = 2, byrow = TRUE)))
  }

  geometries <- lapply(seq_len(nrow(patches)), creer_poly)
  grille <- sf::st_sf(
    id = seq_len(nrow(patches)),
    geometry = sf::st_sfc(geometries, crs = sf::st_crs(aoi))
  )

  # Ne garder que les patches qui intersectent l'AOI
  intersects <- sf::st_intersects(grille, sf::st_union(aoi), sparse = FALSE)[, 1]
  grille <- grille[intersects, ]
  grille$id <- seq_len(nrow(grille))

  message(sprintf("  Patches generes : %d", nrow(grille)))
  message(sprintf("  Taille patch : %d m x %d m", taille_patch_m, taille_patch_m))


  return(grille)
}

# =============================================================================
# 5. EXTRACTION DES PATCHES RASTER
# =============================================================================

extraire_patches_raster <- function(raster_path, grille, taille_pixels = 250) {
  message("=== Extraction des patches raster ===")

  r <- terra::rast(raster_path)
  message(sprintf("  Raster : %s", raster_path))
  message(sprintf("  Bandes : %d", terra::nlyr(r)))
  message(sprintf("  Resolution : %.2f m", terra::res(r)[1]))

  patches_data <- list()

  for (i in seq_len(nrow(grille))) {
    ext_patch <- terra::ext(sf::st_bbox(grille[i, ]))
    patch <- terra::crop(r, ext_patch)

    # Redimensionner au nombre de pixels souhaite
    if (terra::ncol(patch) != taille_pixels || terra::nrow(patch) != taille_pixels) {
      template <- terra::rast(
        ext = ext_patch,
        nrows = taille_pixels, ncols = taille_pixels,
        crs = terra::crs(r),
        nlyrs = terra::nlyr(r)
      )
      patch <- terra::resample(patch, template, method = "bilinear")
    }

    patches_data[[i]] <- terra::values(patch)

    if (i %% 100 == 0) {
      message(sprintf("  Patches extraits : %d / %d", i, nrow(grille)))
    }
  }

  message(sprintf("  Total patches extraits : %d", length(patches_data)))
  return(patches_data)
}

# =============================================================================
# 6. INFERENCE AVEC LE MODELE MAESTRO (via reticulate/Python)
# =============================================================================

configurer_python <- function() {
  message("=== Configuration de l'environnement Python ===")

  # Chercher un environnement conda ou virtualenv existant
  envs <- tryCatch(reticulate::conda_list(), error = function(e) NULL)

  if (!is.null(envs) && "maestro" %in% envs$name) {
    reticulate::use_condaenv("maestro", required = FALSE)
  } else {
    # Utiliser le Python systeme
    reticulate::use_python(Sys.which("python3"), required = FALSE)
  }

  # Verifier les modules Python necessaires
  modules_requis <- c("torch", "numpy", "rasterio")
  for (mod in modules_requis) {
    if (!reticulate::py_module_available(mod)) {
      message(sprintf("  Module Python '%s' non trouve. Installation...", mod))
      reticulate::py_install(mod)
    }
  }

  message("  Python configure.")
}

executer_inference <- function(patches_data, fichiers_modele, n_classes = 13L,
                                utiliser_gpu = FALSE) {
  message("=== Inference MAESTRO ===")

  # Importer les modules Python
  torch <- reticulate::import("torch")
  np <- reticulate::import("numpy")

  # Determiner le device
  device <- if (utiliser_gpu && torch$cuda$is_available()) {
    message("  Utilisation du GPU (CUDA)")
    torch$device("cuda")
  } else {
    message("  Utilisation du CPU")
    torch$device("cpu")
  }

  # Charger le modele
  message("  Chargement du modele...")
  chemin_poids <- fichiers_modele$weights %||% fichiers_modele$snapshot

  if (is.null(chemin_poids)) {
    stop("Impossible de trouver les poids du modele.")
  }

  # Charger le checkpoint
  checkpoint <- torch$load(chemin_poids, map_location = device)

  # Charger la configuration du modele si disponible
  config <- NULL
  if (!is.null(fichiers_modele$config) && file.exists(fichiers_modele$config)) {
    config <- jsonlite::fromJSON(fichiers_modele$config)
    message(sprintf("  Architecture : %s", config$model_type %||% "MAE/ViT"))
  }

  # Construire le modele a partir du code MAESTRO
  # Le modele MAESTRO est un MAE (Masked Autoencoder) base sur un ViT
  maestro <- reticulate::import_from_path("maestro_inference",
                                           path = system.file("python",
                                                              package = .packageName,
                                                              lib.loc = NULL) %||% ".")

  modele <- maestro$charger_modele(chemin_poids, n_classes, device$type)

  # Inference par batch
  message("  Lancement de l'inference...")
  n_patches <- length(patches_data)
  predictions <- vector("list", n_patches)
  batch_size <- 16L
  n_batches <- ceiling(n_patches / batch_size)

  for (b in seq_len(n_batches)) {
    debut <- (b - 1L) * batch_size + 1L
    fin <- min(b * batch_size, n_patches)
    indices <- debut:fin

    # Preparer le batch : empiler les patches en tenseur
    batch_arrays <- lapply(patches_data[indices], function(p) {
      # p est une matrice (pixels x bandes), reorganiser en (bandes x H x W)
      np$array(p, dtype = np$float32)
    })
    batch_np <- np$stack(batch_arrays)

    # Convertir en tenseur PyTorch
    batch_tensor <- torch$from_numpy(batch_np)$to(device)

    # Normaliser
    batch_tensor <- batch_tensor / 255.0

    # Inference sans gradient
    with(torch$no_grad(), {
      logits <- modele(batch_tensor)
      preds <- torch$argmax(logits, dim = 1L)$cpu()$numpy()
    })

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
# 7. POST-TRAITEMENT ET EXPORT
# =============================================================================

assembler_resultats <- function(grille, predictions, essences, dossier_sortie) {
  message("=== Assemblage des resultats ===")

  dir.create(dossier_sortie, showWarnings = FALSE, recursive = TRUE)

  # Ajouter les predictions a la grille
  grille$code_essence <- unlist(predictions)
  grille <- merge(grille, essences, by.x = "code_essence", by.y = "code", all.x = TRUE)

  # Exporter en GeoPackage
  chemin_gpkg <- file.path(dossier_sortie, "essences_forestieres.gpkg")
  sf::st_write(grille, chemin_gpkg, delete_dsn = TRUE, quiet = TRUE)
  message(sprintf("  GeoPackage : %s", chemin_gpkg))

  # Statistiques
  message("\n=== Statistiques des essences detectees ===")
  stats <- as.data.frame(table(grille$classe))
  names(stats) <- c("Essence", "Nombre_patches")
  stats$Proportion <- round(stats$Nombre_patches / sum(stats$Nombre_patches) * 100, 1)
  stats <- stats[order(-stats$Nombre_patches), ]
  print(stats, row.names = FALSE)

  # Exporter les statistiques en CSV
  chemin_csv <- file.path(dossier_sortie, "statistiques_essences.csv")
  write.csv(stats, chemin_csv, row.names = FALSE)
  message(sprintf("\n  Statistiques CSV : %s", chemin_csv))

  return(grille)
}

creer_carte_raster <- function(grille, resolution, dossier_sortie) {
  message("=== Creation du raster de classification ===")

  bbox <- sf::st_bbox(grille)
  template <- terra::rast(
    xmin = bbox["xmin"], xmax = bbox["xmax"],
    ymin = bbox["ymin"], ymax = bbox["ymax"],
    res = resolution,
    crs = sf::st_crs(grille)$wkt
  )

  raster_classe <- terra::rasterize(
    terra::vect(grille),
    template,
    field = "code_essence"
  )

  chemin_tif <- file.path(dossier_sortie, "essences_forestieres.tif")
  terra::writeRaster(raster_classe, chemin_tif, overwrite = TRUE)
  message(sprintf("  Raster GeoTIFF : %s", chemin_tif))

  return(raster_classe)
}

# =============================================================================
# 8. PIPELINE PRINCIPAL
# =============================================================================

main <- function() {
  message("========================================================")
  message(" MAESTRO - Reconnaissance des essences forestieres")
  message(" Modele IGNF via Hugging Face (hfhub)")
  message("========================================================\n")

  # Etape 1 : Lire la zone d'interet
  aoi <- lire_aoi(opt$aoi)

  # Etape 2 : Telecharger le modele MAESTRO
  fichiers_modele <- telecharger_modele(opt$model, opt$token)

  # Etape 3 : Configurer Python (reticulate)
  configurer_python()

  # Etape 4 : Creer la grille de patches
  taille_patch_m <- opt$patch_size * opt$resolution  # ex: 250px * 0.2m = 50m
  grille <- creer_grille_patches(aoi, taille_patch_m)

  # Etape 5 : Extraire les patches du raster
  if (is.null(opt$raster)) {
    message("\n[INFO] Aucun raster fourni (--raster).")
    message("  Le script attend un raster d'orthophotographie IRC (4 bandes : NIR-R-G-B)")
    message("  couvrant la zone d'interet, a une resolution de 0.2 m.")
    message("  Vous pouvez telecharger les orthophotos depuis :")
    message("    - https://geoservices.ign.fr/bdortho")
    message("    - https://data.geopf.fr/")
    message("")
    message("  Exemple :")
    message("    Rscript maestro_essences.R --aoi aoi.gpkg --raster ortho_irc.tif")
    message("")

    # Mode demonstration : generer des predictions aleatoires
    message("[MODE DEMO] Generation de predictions de demonstration...")
    predictions <- lapply(seq_len(nrow(grille)), function(i) {
      sample(0:12, 1)
    })

  } else {
    patches_data <- extraire_patches_raster(opt$raster, grille, opt$patch_size)

    # Etape 6 : Inference
    predictions <- executer_inference(
      patches_data, fichiers_modele,
      n_classes = 13L,
      utiliser_gpu = opt$gpu
    )
  }

  # Etape 7 : Assembler les resultats
  resultats <- assembler_resultats(grille, predictions, ESSENCES, opt$output)

  # Etape 8 : Creer le raster de classification
  creer_carte_raster(resultats, opt$resolution, opt$output)

  message("\n========================================================")
  message(" Traitement termine !")
  message(sprintf(" Resultats dans : %s", opt$output))
  message("========================================================")
}

# Lancer le pipeline
main()
