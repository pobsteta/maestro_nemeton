# =============================================================================
# Segmentation dense MAESTRO a 0.2m via decodeur sur tokens
# =============================================================================

#' Charger le decodeur de segmentation MAESTRO
#'
#' Charge un backbone MAESTRO pre-entraine et un decodeur de segmentation
#' sauvegarde pour produire des cartes d'essences a 0.2m de resolution.
#'
#' @param backbone_path Chemin vers le checkpoint MAESTRO (.ckpt)
#' @param decoder_path Chemin vers le decodeur de segmentation (.pt)
#' @param modalites Vecteur des modalites a charger
#' @param gpu Utiliser CUDA (defaut: FALSE)
#' @return Modele Python MAESTROSegmenter via reticulate
#' @export
charger_segmenter <- function(backbone_path, decoder_path,
                               modalites = c("aerial", "dem"),
                               gpu = FALSE) {
  message("=== Chargement du segmenter MAESTRO ===")

  configurer_python()

  py_path <- python_module_path()
  maestro <- reticulate::import_from_path("maestro_inference", path = py_path)
  seg_module <- reticulate::import_from_path("maestro_segmentation", path = py_path)

  torch <- reticulate::import("torch")
  device_str <- if (gpu && torch$cuda$is_available()) {
    message("  Utilisation du GPU (CUDA)")
    "cuda"
  } else {
    message("  Utilisation du CPU")
    "cpu"
  }

  # Charger le backbone
  backbone <- maestro$charger_modele(
    chemin_poids = backbone_path,
    n_classes = 13L,
    device = device_str,
    modalites = as.list(modalites)
  )

  # Charger le decodeur
  segmenter <- seg_module$charger_segmenter(
    backbone = backbone,
    decoder_path = decoder_path,
    device = device_str,
    freeze_backbone = TRUE
  )

  message("  Segmenter charge.")
  segmenter
}


#' Executer la segmentation MAESTRO sur un patch
#'
#' Predit la carte de segmentation a 0.2m pour un patch multimodal.
#'
#' @param segmenter Modele MAESTROSegmenter (issu de [charger_segmenter()])
#' @param modalites_data Liste nommee de SpatRasters par modalite,
#'   deja croppes sur l'emprise du patch
#' @param gpu Utiliser CUDA
#' @return Liste avec `classes` (matrice 250x250 int) et `probas` (array)
#' @keywords internal
.predire_patch_segmentation <- function(segmenter, modalites_data, gpu = FALSE) {
  py_path <- python_module_path()
  seg_module <- reticulate::import_from_path("maestro_segmentation", path = py_path)
  np <- reticulate::import("numpy")
  torch <- reticulate::import("torch")

  device_str <- if (gpu && torch$cuda$is_available()) "cuda" else "cpu"

  # Preparer les inputs numpy
  inputs <- list()
  for (mod_name in names(modalites_data)) {
    r <- modalites_data[[mod_name]]
    tp <- taille_patch_modalite(mod_name)
    vals <- terra::values(r)  # (H*W, C)
    C <- ncol(vals)
    arr <- array(vals, dim = c(tp, tp, C))
    # (H, W, C) -> (C, H, W)
    arr <- aperm(arr, c(3, 1, 2))
    inputs[[mod_name]] <- np$array(arr, dtype = np$float32)
  }

  # Predire
  result <- seg_module$predire_segmentation(
    segmenter = segmenter,
    inputs = inputs,
    device = device_str
  )

  result
}


#' Executer la segmentation dense MAESTRO sur toute l'AOI
#'
#' Decoupe l'AOI en patches de 50m, execute la segmentation par patch
#' via le backbone MAESTRO + decodeur, puis reassemble les predictions
#' en une carte continue a 0.2m de resolution.
#'
#' Les patches se chevauchent de 10m (overlap) et les zones de recouvrement
#' sont resolues par vote de la classe avec la probabilite maximale.
#'
#' @param segmenter Modele MAESTROSegmenter
#' @param modalites Liste nommee de SpatRasters complets pour l'AOI
#'   (ex: `list(aerial=rgbi, dem=dem)`)
#' @param aoi sf object en Lambert-93
#' @param output_dir Repertoire de sortie
#' @param patch_size Taille des patches en pixels (defaut: 250)
#' @param resolution Resolution en metres (defaut: 0.2)
#' @param overlap_m Recouvrement entre patches en metres (defaut: 10)
#' @param gpu Utiliser CUDA
#' @param batch_size Taille des batchs (defaut: 4)
#' @return SpatRaster mono-bande avec les codes NDP0 a 0.2m
#' @export
executer_segmentation <- function(segmenter, modalites, aoi,
                                    output_dir = "outputs",
                                    patch_size = 250L,
                                    resolution = 0.2,
                                    overlap_m = 10,
                                    gpu = FALSE,
                                    batch_size = 4L) {
  message("=== Segmentation dense MAESTRO (0.2m) ===")
  message(sprintf("  Modalites: %s", paste(names(modalites), collapse = ", ")))

  py_path <- python_module_path()
  seg_module <- reticulate::import_from_path("maestro_segmentation", path = py_path)
  np <- reticulate::import("numpy")
  torch <- reticulate::import("torch")

  device_str <- if (gpu && torch$cuda$is_available()) "cuda" else "cpu"

  taille_patch_m <- patch_size * resolution  # 50m
  pas_m <- taille_patch_m - overlap_m        # 40m

  # Creer la grille avec overlap
  bbox <- sf::st_bbox(aoi)
  x_coords <- seq(bbox["xmin"], bbox["xmax"], by = pas_m)
  y_coords <- seq(bbox["ymin"], bbox["ymax"], by = pas_m)
  grid <- expand.grid(x = x_coords, y = y_coords)
  grid$xmax <- grid$x + taille_patch_m
  grid$ymax <- grid$y + taille_patch_m

  n_patches <- nrow(grid)
  message(sprintf("  Grille: %d patches (%.0fm x %.0fm, overlap %.0fm)",
                   n_patches, taille_patch_m, taille_patch_m, overlap_m))

  # Raster de sortie (full AOI a 0.2m)
  ref <- modalites[[1]]
  ext_out <- terra::ext(bbox["xmin"], bbox["xmax"], bbox["ymin"], bbox["ymax"])
  raster_classes <- terra::rast(ext = ext_out, res = resolution,
                                 crs = terra::crs(ref))
  terra::values(raster_classes) <- NA_integer_

  # Raster de probabilites max (pour gerer l'overlap)
  raster_proba <- terra::rast(ext = ext_out, res = resolution,
                               crs = terra::crs(ref))
  terra::values(raster_proba) <- -1.0

  n_done <- 0L

  for (i in seq_len(n_patches)) {
    ext_patch <- terra::ext(grid$x[i], grid$xmax[i],
                             grid$y[i], grid$ymax[i])

    # Extraire les donnees par modalite
    patch_data <- list()
    valid <- TRUE
    for (mod_name in names(modalites)) {
      r <- modalites[[mod_name]]
      tp <- taille_patch_modalite(mod_name, patch_size)

      crop_r <- tryCatch(terra::crop(r, ext_patch), error = function(e) NULL)
      if (is.null(crop_r)) { valid <- FALSE; break }

      # Reechantillonner a la bonne taille
      if (terra::ncol(crop_r) != tp || terra::nrow(crop_r) != tp) {
        tmpl <- terra::rast(ext = ext_patch, nrows = tp, ncols = tp,
                             crs = terra::crs(r), nlyrs = terra::nlyr(r))
        crop_r <- terra::resample(crop_r, tmpl, method = "bilinear")
      }
      patch_data[[mod_name]] <- crop_r
    }
    if (!valid) next

    # Preparer les inputs numpy (batch de 1)
    inputs <- list()
    for (mod_name in names(patch_data)) {
      r <- patch_data[[mod_name]]
      tp <- taille_patch_modalite(mod_name, patch_size)
      vals <- terra::values(r)
      vals[is.na(vals)] <- 0
      C <- ncol(vals)
      # (H*W, C) -> (1, C, H, W)
      arr <- array(vals, dim = c(tp, tp, C))
      arr <- aperm(arr, c(3, 1, 2))
      arr <- array(arr, dim = c(1, dim(arr)))
      inputs[[mod_name]] <- np$array(arr, dtype = np$float32)
    }

    # Predire
    result <- seg_module$predire_segmentation(
      segmenter = segmenter,
      inputs = inputs,
      device = device_str
    )

    # Extraire classes et probas
    classes_np <- result$classes[1, , ]   # (H, W) = (250, 250)
    probas_np <- result$probas[1, , , ]   # (C, H, W)

    # Convertir en matrices R
    classes_mat <- as.matrix(reticulate::py_to_r(classes_np))
    probas_arr <- reticulate::py_to_r(probas_np)

    # Proba max par pixel
    n_cls <- dim(probas_arr)[1]
    proba_max <- apply(array(probas_arr, dim = c(n_cls, patch_size, patch_size)),
                        c(2, 3), max)

    # Ecrire dans le raster de sortie (garder le pixel avec la proba max)
    patch_rast_cls <- terra::rast(ext = ext_patch,
                                   nrows = patch_size, ncols = patch_size,
                                   crs = terra::crs(ref))
    terra::values(patch_rast_cls) <- as.integer(classes_mat)

    patch_rast_prob <- terra::rast(ext = ext_patch,
                                    nrows = patch_size, ncols = patch_size,
                                    crs = terra::crs(ref))
    terra::values(patch_rast_prob) <- proba_max

    # Merger par proba max (overlap resolution)
    # Crop les rasters de sortie sur l'emprise du patch
    ext_inter <- terra::intersect(terra::ext(raster_classes), ext_patch)
    if (is.null(ext_inter)) next

    cur_cls <- terra::crop(raster_classes, ext_inter)
    cur_prob <- terra::crop(raster_proba, ext_inter)
    new_cls <- terra::crop(patch_rast_cls, ext_inter)
    new_prob <- terra::crop(patch_rast_prob, ext_inter)

    # Aligner les resolutions si necessaire
    if (!all(dim(cur_cls) == dim(new_cls))) {
      new_cls <- terra::resample(new_cls, cur_cls, method = "near")
      new_prob <- terra::resample(new_prob, cur_prob, method = "bilinear")
    }

    # Mettre a jour la ou la nouvelle proba est superieure
    mask_better <- terra::values(new_prob) > terra::values(cur_prob)
    mask_better[is.na(mask_better)] <- TRUE

    vals_cls <- terra::values(cur_cls)
    vals_prob <- terra::values(cur_prob)
    vals_cls[mask_better] <- terra::values(new_cls)[mask_better]
    vals_prob[mask_better] <- terra::values(new_prob)[mask_better]

    # Reecrire dans les rasters complets via indexation row/col
    row_start <- terra::rowFromY(raster_classes, terra::ymax(ext_inter))
    row_end   <- terra::rowFromY(raster_classes, terra::ymin(ext_inter))
    col_start <- terra::colFromX(raster_classes, terra::xmin(ext_inter))
    col_end   <- terra::colFromX(raster_classes, terra::xmax(ext_inter))

    rows <- row_start:row_end
    cols <- col_start:col_end
    cells <- terra::cellFromRowColCombine(raster_classes, rows, cols)

    raster_classes[cells] <- vals_cls
    raster_proba[cells] <- vals_prob

    n_done <- n_done + 1L
    if (n_done %% 50 == 0 || n_done == n_patches) {
      message(sprintf("  Patches traites: %d / %d", n_done, n_patches))
    }
  }

  names(raster_classes) <- "classe_ndp0"

  # Sauvegarder
  out_path <- file.path(output_dir, "segmentation_ndp0.tif")
  terra::writeRaster(raster_classes, out_path, overwrite = TRUE,
                     datatype = "INT1U", gdal = c("COMPRESS=LZW"))
  message(sprintf("  Carte sauvegardee: %s", out_path))

  # Statistiques
  cls <- classes_ndp0()
  freq <- table(terra::values(raster_classes), useNA = "no")
  total <- sum(freq)
  message("\n=== Statistiques segmentation NDP0 ===")
  for (nm in names(freq)) {
    code <- as.integer(nm)
    pct <- round(as.integer(freq[nm]) / total * 100, 1)
    label <- cls$classe[cls$code == code]
    if (length(label) == 0) label <- "?"
    message(sprintf("  %d - %s: %.1f%%", code, label, pct))
  }

  raster_classes
}


#' Preparer les patches d'entrainement pour le decodeur de segmentation
#'
#' Decoupe les rasters multimodaux et le masque BD Foret NDP0 en patches
#' de 250x250 px, organises en structure train/val pour l'entrainement.
#'
#' @param modalites Liste nommee de SpatRasters (aerial, dem, s2, ...)
#' @param labels SpatRaster masque NDP0 (issu de [preparer_labels_ndp0()])
#' @param aoi sf object
#' @param output_dir Repertoire de sortie
#' @param patch_size Taille des patches en pixels (defaut: 250)
#' @param resolution Resolution en metres (defaut: 0.2)
#' @param val_ratio Proportion de patches pour la validation (defaut: 0.15)
#' @param min_forest_pct Pourcentage minimum de foret pour garder un patch
#'   (defaut: 10)
#' @return Liste avec le nombre de patches train et val
#' @export
preparer_patches_entrainement <- function(modalites, labels, aoi,
                                            output_dir = "data/segmentation",
                                            patch_size = 250L,
                                            resolution = 0.2,
                                            val_ratio = 0.15,
                                            min_forest_pct = 10) {
  message("=== Preparation des patches d'entrainement ===")

  taille_patch_m <- patch_size * resolution

  # Creer la grille
  grille <- creer_grille_patches(aoi, taille_patch_m)
  n_total <- nrow(grille)
  message(sprintf("  Grille: %d patches candidats", n_total))

  # Creer les repertoires
  for (split in c("train", "val")) {
    for (mod in c(names(modalites), "labels")) {
      dir.create(file.path(output_dir, split, mod), recursive = TRUE,
                 showWarnings = FALSE)
    }
  }

  # Split train/val
  set.seed(42)
  val_idx <- sort(sample.int(n_total, size = round(n_total * val_ratio)))
  splits <- rep("train", n_total)
  splits[val_idx] <- "val"

  n_kept <- 0L
  n_skipped <- 0L

  for (i in seq_len(n_total)) {
    ext_patch <- terra::ext(sf::st_bbox(grille[i, ]))
    split <- splits[i]
    patch_id <- sprintf("patch_%05d", i)

    # Extraire le label
    label_crop <- tryCatch(terra::crop(labels, ext_patch), error = function(e) NULL)
    if (is.null(label_crop)) { n_skipped <- n_skipped + 1L; next }

    # Reechantillonner a patch_size x patch_size
    tmpl <- terra::rast(ext = ext_patch, nrows = patch_size, ncols = patch_size,
                         crs = terra::crs(labels))
    label_crop <- terra::resample(label_crop, tmpl, method = "near")

    # Filtrer les patches avec trop peu de foret
    vals_label <- terra::values(label_crop)
    pct_forest <- sum(vals_label < 9, na.rm = TRUE) / length(vals_label) * 100
    if (pct_forest < min_forest_pct) { n_skipped <- n_skipped + 1L; next }

    # Sauvegarder le label
    label_path <- file.path(output_dir, split, "labels",
                             paste0(patch_id, ".tif"))
    terra::writeRaster(label_crop, label_path, overwrite = TRUE,
                       datatype = "INT1U")

    # Extraire et sauvegarder chaque modalite
    for (mod_name in names(modalites)) {
      r <- modalites[[mod_name]]
      tp <- taille_patch_modalite(mod_name, patch_size)

      crop_r <- tryCatch(terra::crop(r, ext_patch), error = function(e) NULL)
      if (is.null(crop_r)) next

      if (terra::ncol(crop_r) != tp || terra::nrow(crop_r) != tp) {
        tmpl_mod <- terra::rast(ext = ext_patch, nrows = tp, ncols = tp,
                                 crs = terra::crs(r), nlyrs = terra::nlyr(r))
        crop_r <- terra::resample(crop_r, tmpl_mod, method = "bilinear")
      }

      mod_path <- file.path(output_dir, split, mod_name,
                              paste0(patch_id, ".tif"))
      terra::writeRaster(crop_r, mod_path, overwrite = TRUE)
    }

    n_kept <- n_kept + 1L
    if (n_kept %% 100 == 0) {
      message(sprintf("  Patches extraits: %d / %d", n_kept, n_total))
    }
  }

  n_train <- sum(splits[seq_len(n_total)] == "train" &
                   seq_len(n_total) %in% which(rep(TRUE, n_total)))
  n_val <- n_kept - sum(splits == "train")

  message(sprintf("  Patches gardes: %d (train: %d, val: %d)",
                   n_kept, n_kept - length(val_idx), length(val_idx)))
  message(sprintf("  Patches ignores (< %d%% foret): %d",
                   min_forest_pct, n_skipped))
  message(sprintf("  Sortie: %s/", output_dir))

  invisible(list(n_train = n_kept - length(val_idx),
                 n_val = length(val_idx),
                 n_skipped = n_skipped))
}


#' Preparer les patches FLAIR-HUB pour l'entrainement du decodeur
#'
#' Organise les patches FLAIR-HUB existants (deja labellises avec
#' [labelliser_flair_bdforet()]) en structure train/val attendue par
#' `train_segmentation.py`. Les patches avec moins de `min_forest_pct`%
#' de foret sont exclus.
#'
#' Structure de sortie :
#' ```
#' output_dir/
#'   train/
#'     aerial/   patch_00001.tif, ...
#'     dem/      patch_00001.tif, ...
#'     labels/   patch_00001.tif, ...
#'   val/
#'     aerial/   ...
#'     dem/      ...
#'     labels/   ...
#' ```
#'
#' @param flair_dir Repertoire racine FLAIR-HUB contenant aerial/, dem/,
#'   labels_ndp0/ (issu de [labelliser_flair_bdforet()])
#' @param output_dir Repertoire de sortie pour les patches reorganises
#' @param modalites Vecteur des modalites a inclure (defaut: `c("aerial", "dem")`)
#' @param val_ratio Proportion de patches pour la validation (defaut: 0.15)
#' @param min_forest_pct Pourcentage minimum de foret pour garder un patch
#'   (defaut: 10)
#' @param domaines Vecteur de domaines a utiliser (`NULL` = tous)
#' @param max_patches Nombre maximum de patches a utiliser (`NULL` = tous)
#' @return Liste avec n_train, n_val, n_skipped
#' @export
preparer_flair_segmentation <- function(flair_dir = "data/flair_hub",
                                          output_dir = "data/segmentation",
                                          modalites = c("aerial", "dem"),
                                          val_ratio = 0.15,
                                          min_forest_pct = 10,
                                          domaines = NULL,
                                          max_patches = NULL) {
  message("=== Preparation patches FLAIR-HUB pour segmentation ===")

  # Verifier que les labels NDP0 existent
  labels_dir <- file.path(flair_dir, "labels_ndp0")
  if (!dir.exists(labels_dir)) {
    stop("Dossier labels_ndp0/ introuvable. Lancez d'abord labelliser_flair_bdforet()")
  }

  # Detecter les domaines
  if (is.null(domaines)) {
    domaines <- list.dirs(file.path(flair_dir, "aerial"),
                          recursive = FALSE, full.names = FALSE)
    if (length(domaines) == 0) domaines <- ""
  }

  # Collecter tous les patches avec label NDP0
  patch_list <- data.frame(
    patch_id = character(0),
    domaine = character(0),
    label_path = character(0),
    stringsAsFactors = FALSE
  )

  for (dom in domaines) {
    if (nchar(dom) > 0) {
      lbl_dir <- file.path(labels_dir, dom)
      aer_dir <- file.path(flair_dir, "aerial", dom)
    } else {
      lbl_dir <- labels_dir
      aer_dir <- file.path(flair_dir, "aerial")
    }

    if (!dir.exists(lbl_dir)) next

    label_files <- list.files(lbl_dir, pattern = "\\.tif$", full.names = TRUE)

    # Index des fichiers aerial (recherche recursive car FLAIR-HUB extrait
    # dans des sous-dossiers imbriques)
    aerial_tifs <- list.files(aer_dir, pattern = "\\.tif$",
                               recursive = TRUE, full.names = TRUE)
    aerial_index <- stats::setNames(aerial_tifs,
                                     tools::file_path_sans_ext(basename(aerial_tifs)))

    for (lf in label_files) {
      pid <- tools::file_path_sans_ext(basename(lf))
      # Verifier que le aerial correspondant existe (recherche par nom de base)
      if (!(pid %in% names(aerial_index))) next

      patch_list <- rbind(patch_list, data.frame(
        patch_id = pid,
        domaine = dom,
        label_path = lf,
        stringsAsFactors = FALSE
      ))
    }
  }

  message(sprintf("  %d patches avec labels NDP0 trouves", nrow(patch_list)))

  if (nrow(patch_list) == 0) {
    stop("Aucun patch avec label NDP0 trouve")
  }

  # Filtrer par pourcentage de foret
  message("  Filtrage par couverture forestiere...")
  keep <- logical(nrow(patch_list))
  for (i in seq_len(nrow(patch_list))) {
    lbl <- terra::rast(patch_list$label_path[i])
    vals <- terra::values(lbl)
    pct <- sum(vals < 9, na.rm = TRUE) / sum(!is.na(vals)) * 100
    keep[i] <- pct >= min_forest_pct
  }
  n_skipped <- sum(!keep)
  patch_list <- patch_list[keep, ]
  message(sprintf("  %d patches forestiers (>= %d%%), %d ignores",
                   nrow(patch_list), min_forest_pct, n_skipped))

  # Limiter le nombre de patches si demande
  if (!is.null(max_patches) && nrow(patch_list) > max_patches) {
    set.seed(42)
    patch_list <- patch_list[sample.int(nrow(patch_list), max_patches), ]
    message(sprintf("  Limite a %d patches", max_patches))
  }

  # Split train/val
  set.seed(42)
  n_total <- nrow(patch_list)
  val_idx <- sort(sample.int(n_total, size = round(n_total * val_ratio)))
  patch_list$split <- "train"
  patch_list$split[val_idx] <- "val"

  # Creer les repertoires
  for (sp in c("train", "val")) {
    for (mod in c(modalites, "labels")) {
      dir.create(file.path(output_dir, sp, mod), recursive = TRUE,
                 showWarnings = FALSE)
    }
  }

  # Copier/lier les fichiers
  message("  Copie des patches...")
  n_done <- 0L

  for (i in seq_len(n_total)) {
    pid <- patch_list$patch_id[i]
    dom <- patch_list$domaine[i]
    sp <- patch_list$split[i]
    new_id <- sprintf("patch_%05d", i)

    # Label
    src_label <- patch_list$label_path[i]
    dst_label <- file.path(output_dir, sp, "labels", paste0(new_id, ".tif"))
    file.copy(src_label, dst_label, overwrite = TRUE)

    # Modalites
    for (mod in modalites) {
      # Recherche recursive pour gerer les sous-dossiers FLAIR-HUB
      if (nchar(dom) > 0) {
        mod_dir <- file.path(flair_dir, mod, dom)
      } else {
        mod_dir <- file.path(flair_dir, mod)
      }

      src_mod <- NULL
      if (dir.exists(mod_dir)) {
        candidates <- list.files(mod_dir,
                                  pattern = paste0("^", gsub("([.+?^${}()|\\[\\]])", "\\\\\\1", pid), "\\.tif$"),
                                  recursive = TRUE, full.names = TRUE)
        if (length(candidates) > 0) src_mod <- candidates[1]
      }

      if (!is.null(src_mod) && file.exists(src_mod)) {
        dst_mod <- file.path(output_dir, sp, mod, paste0(new_id, ".tif"))
        file.copy(src_mod, dst_mod, overwrite = TRUE)
      }
    }

    n_done <- n_done + 1L
    if (n_done %% 200 == 0) {
      message(sprintf("    %d / %d patches copies", n_done, n_total))
    }
  }

  n_train <- sum(patch_list$split == "train")
  n_val <- sum(patch_list$split == "val")

  message(sprintf("\n=== Patches prets : %d train, %d val ===", n_train, n_val))
  message(sprintf("  Sortie: %s/", output_dir))
  message(sprintf("  Modalites: %s", paste(modalites, collapse = ", ")))
  message("")
  message("Pour lancer l'entrainement :")
  message(sprintf("  python inst/python/train_segmentation.py \\"))
  message(sprintf("    --checkpoint MAESTRO_pretrain.ckpt \\"))
  message(sprintf("    --data-dir %s \\", output_dir))
  message(sprintf("    --modalites %s \\", paste(modalites, collapse = ",")))
  message(sprintf("    --epochs 50 --batch-size 8 --lr 1e-3 --gpu"))

  invisible(list(n_train = n_train, n_val = n_val, n_skipped = n_skipped))
}
