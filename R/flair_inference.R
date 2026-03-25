# =============================================================================
# Inference FLAIR-HUB pour la segmentation d'occupation du sol
# =============================================================================

#' Chemin vers le module Python d'inference FLAIR
#'
#' @return Chemin du repertoire Python (character)
#' @export
flair_python_module_path <- function() {
  system.file("python", package = "maestro", mustWork = TRUE)
}

#' Charger un modele FLAIR pour la segmentation
#'
#' Charge un modele de segmentation semantique FLAIR (ResNet34-UNet ou
#' ConvNeXTV2-UPerNet) depuis un fichier de poids.
#'
#' @param chemin_poids Chemin vers le fichier de poids
#' @param n_classes Nombre de classes (19 pour CoSIA, 23 pour LPIS)
#' @param in_channels Nombre de canaux d'entree (4 = RGBI, 5 = RGBI+DEM)
#' @param encoder Architecture encodeur (ex: "resnet34", "convnextv2_nano")
#' @param decoder Architecture decodeur ("unet" ou "upernet")
#' @param device "cpu" ou "cuda"
#' @return Modele Python charge via reticulate
#' @export
charger_modele_flair <- function(chemin_poids, n_classes = 19L,
                                  in_channels = 4L,
                                  encoder = "resnet34", decoder = "unet",
                                  device = "cpu") {
  py_path <- flair_python_module_path()
  flair <- reticulate::import_from_path("flair_inference", path = py_path)

  flair$charger_modele_flair(
    chemin_poids = chemin_poids,
    n_classes = as.integer(n_classes),
    in_channels = as.integer(in_channels),
    encoder = encoder,
    decoder = decoder,
    device = device
  )
}

#' Telecharger un modele FLAIR pre-entraine depuis HuggingFace
#'
#' @param model_id Identifiant HuggingFace du modele
#'   (defaut: "IGNF/FLAIR-INC_rgbi_15cl_resnet34-unet")
#' @param token Token HuggingFace (optionnel)
#' @return Liste avec `weights` (chemin des poids) et `config` (configuration)
#' @export
telecharger_modele_flair <- function(model_id = "IGNF/FLAIR-INC_rgbi_15cl_resnet34-unet",
                                      token = NULL) {
  message(sprintf("=== Telechargement modele FLAIR: %s ===", model_id))

  # Utiliser le meme mecanisme que telecharger_modele() de MAESTRO
  telecharger_modele(model_id, token)
}

#' Creer des patches d'inference avec overlap
#'
#' Decoupe un raster en patches de taille fixe avec recouvrement pour
#' l'inference FLAIR (segmentation pixel).
#'
#' @param r SpatRaster a decouper
#' @param patch_size Taille des patches en pixels (defaut: 512)
#' @param overlap Recouvrement entre patches en pixels (defaut: 128)
#' @return Liste avec `patches` (liste de SpatRaster), `positions` (data.frame
#'   avec col, row, x, y), `raster_dim` (dimensions du raster original)
#' @export
creer_patches_inference <- function(r, patch_size = 512L, overlap = 128L) {
  stride <- patch_size - overlap
  ncol_r <- terra::ncol(r)
  nrow_r <- terra::nrow(r)
  res_x <- terra::res(r)[1]
  res_y <- terra::res(r)[2]
  ext <- terra::ext(r)

  # Positions en pixels
  col_starts <- seq(0, max(0, ncol_r - patch_size), by = stride)
  row_starts <- seq(0, max(0, nrow_r - patch_size), by = stride)

  # Ajouter les bords
  if (ncol_r > patch_size && (ncol_r - patch_size) %% stride != 0) {
    col_starts <- c(col_starts, ncol_r - patch_size)
  }
  if (nrow_r > patch_size && (nrow_r - patch_size) %% stride != 0) {
    row_starts <- c(row_starts, nrow_r - patch_size)
  }

  # Dedupliquer
  col_starts <- unique(col_starts)
  row_starts <- unique(row_starts)

  positions <- expand.grid(col = col_starts, row = row_starts)
  n_patches <- nrow(positions)
  message(sprintf("  %d patches (%dx%d, overlap %d, stride %d)",
                   n_patches, patch_size, patch_size, overlap, stride))

  patches <- vector("list", n_patches)
  for (i in seq_len(n_patches)) {
    c0 <- positions$col[i]
    r0 <- positions$row[i]

    xmin <- ext[1] + c0 * res_x
    xmax <- xmin + patch_size * res_x
    ymax <- ext[4] - r0 * res_y
    ymin <- ymax - patch_size * res_y

    patch_ext <- terra::ext(xmin, xmax, ymin, ymax)
    patches[[i]] <- terra::crop(r, patch_ext)
  }

  list(
    patches = patches,
    positions = positions,
    patch_size = patch_size,
    overlap = overlap,
    raster_dim = c(nrow_r, ncol_r),
    raster_ext = ext,
    raster_res = c(res_x, res_y),
    raster_crs = terra::crs(r)
  )
}

#' Executer l'inference FLAIR avec blending Hann
#'
#' Execute la segmentation semantique sur un raster complet en decoupant
#' en patches avec overlap et en fusionnant les predictions avec une
#' fenetre de Hann pour eviter les artefacts de bord.
#'
#' @param r SpatRaster d'entree (RGBI 4 bandes ou RGBI+DEM 5 bandes)
#' @param modele Modele FLAIR charge (issue de [charger_modele_flair()])
#' @param patch_size Taille des patches (defaut: 512)
#' @param overlap Recouvrement entre patches (defaut: 128)
#' @param n_classes Nombre de classes de sortie (defaut: 19)
#' @param utiliser_gpu Utiliser le GPU CUDA (defaut: FALSE)
#' @param batch_size Taille des batchs (defaut: 4)
#' @return SpatRaster mono-bande avec les classes predites
#' @export
executer_inference_flair <- function(r, modele, patch_size = 512L,
                                      overlap = 128L, n_classes = 19L,
                                      utiliser_gpu = FALSE,
                                      batch_size = 4L) {
  message("=== Inference FLAIR (segmentation semantique) ===")

  py_path <- flair_python_module_path()
  flair <- reticulate::import_from_path("flair_inference", path = py_path)
  np <- reticulate::import("numpy")
  torch <- reticulate::import("torch")

  device_str <- if (utiliser_gpu && torch$cuda$is_available()) {
    message("  Utilisation du GPU (CUDA)")
    "cuda"
  } else {
    message("  Utilisation du CPU")
    "cpu"
  }

  # Extraire les valeurs du raster complet
  vals <- terra::values(r)
  nrow_r <- terra::nrow(r)
  ncol_r <- terra::ncol(r)
  n_bands <- terra::nlyr(r)

  # Reorganiser en (C, H, W)
  image_chw <- array(0, dim = c(n_bands, nrow_r, ncol_r))
  for (b in seq_len(n_bands)) {
    band_vals <- vals[, b]
    band_vals[is.na(band_vals)] <- 0
    image_chw[b, , ] <- matrix(band_vals, nrow = nrow_r, ncol = ncol_r,
                                byrow = FALSE)
  }

  image_np <- np$array(image_chw, dtype = np$float32)

  # Appeler l'inference Python avec blending Hann
  classes_np <- flair$predire_raster_complet(
    modele = modele,
    image_np = image_np,
    patch_size = as.integer(patch_size),
    overlap = as.integer(overlap),
    n_classes = as.integer(n_classes),
    device = device_str,
    batch_size = as.integer(batch_size)
  )

  # Convertir en SpatRaster
  classes_mat <- as.matrix(reticulate::py_to_r(classes_np))

  result <- terra::rast(
    nrows = nrow_r, ncols = ncol_r,
    xmin = terra::ext(r)[1], xmax = terra::ext(r)[2],
    ymin = terra::ext(r)[3], ymax = terra::ext(r)[4],
    crs = terra::crs(r)
  )
  terra::values(result) <- as.integer(classes_mat)
  names(result) <- "classe"

  message(sprintf("  Resultat: %d x %d pixels, %d classes detectees",
                   ncol_r, nrow_r, length(unique(classes_mat))))

  result
}

#' Assembler les resultats FLAIR en GeoPackage et statistiques
#'
#' @param raster_classe SpatRaster mono-bande de classification
#' @param classes Table des classes (issue de [classes_cosia()] ou [classes_lpis()])
#' @param dossier_sortie Repertoire de sortie
#' @return Liste avec `raster` et `stats`
#' @export
assembler_resultats_flair <- function(raster_classe, classes = NULL,
                                       dossier_sortie = "outputs") {
  message("=== Assemblage des resultats FLAIR ===")
  dir.create(dossier_sortie, showWarnings = FALSE, recursive = TRUE)

  if (is.null(classes)) classes <- classes_cosia()

  # Sauvegarder le raster
  tif_path <- file.path(dossier_sortie, "occupation_sol.tif")
  terra::writeRaster(raster_classe, tif_path, overwrite = TRUE,
                     gdal = c("COMPRESS=LZW"))
  message(sprintf("  Raster GeoTIFF: %s", tif_path))

  # Statistiques par classe
  vals <- terra::values(raster_classe, na.rm = TRUE)
  freq_table <- table(vals)

  stats <- data.frame(
    code = as.integer(names(freq_table)),
    n_pixels = as.integer(freq_table),
    stringsAsFactors = FALSE
  )
  stats$proportion <- round(stats$n_pixels / sum(stats$n_pixels) * 100, 2)

  # Joindre les noms de classes
  stats <- merge(stats, classes, by = "code", all.x = TRUE)
  stats <- stats[order(-stats$n_pixels), ]

  csv_path <- file.path(dossier_sortie, "statistiques_occupation_sol.csv")
  write.csv(stats, csv_path, row.names = FALSE)
  message(sprintf("  Statistiques CSV: %s", csv_path))

  message("\n=== Statistiques d'occupation du sol ===")
  print(stats[, c("classe", "n_pixels", "proportion")], row.names = FALSE)

  list(raster = raster_classe, stats = stats)
}
