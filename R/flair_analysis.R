# =============================================================================
# Analyse et evaluation des resultats FLAIR
# =============================================================================

#' Calculer les statistiques d'occupation du sol
#'
#' Produit un tableau de statistiques par classe a partir d'un raster
#' de classification.
#'
#' @param raster_classe SpatRaster mono-bande de classification
#' @param classes Table des classes (issue de [classes_cosia()] ou [classes_lpis()])
#' @return data.frame avec colonnes code, classe, n_pixels, proportion,
#'   surface_ha
#' @export
compute_landcover_stats <- function(raster_classe, classes = NULL) {
  if (is.null(classes)) classes <- classes_cosia()

  vals <- terra::values(raster_classe, na.rm = TRUE)
  freq_table <- table(vals)

  stats <- data.frame(
    code = as.integer(names(freq_table)),
    n_pixels = as.integer(freq_table),
    stringsAsFactors = FALSE
  )

  total_pixels <- sum(stats$n_pixels)
  stats$proportion <- round(stats$n_pixels / total_pixels * 100, 2)

  # Surface en hectares

  res <- terra::res(raster_classe)
  pixel_area_m2 <- res[1] * res[2]
  stats$surface_ha <- round(stats$n_pixels * pixel_area_m2 / 10000, 4)

  stats <- merge(stats, classes, by = "code", all.x = TRUE)
  stats[order(-stats$n_pixels), ]
}

#' Correler l'occupation du sol avec le DEM
#'
#' Calcule les statistiques d'altitude par classe d'occupation du sol.
#'
#' @param raster_classe SpatRaster de classification
#' @param dem SpatRaster du Modele Numerique de Terrain
#' @param classes Table des classes
#' @return data.frame avec altitude moyenne, min, max par classe
#' @export
cross_landcover_dem <- function(raster_classe, dem, classes = NULL) {
  if (is.null(classes)) classes <- classes_cosia()

  # Aligner le DEM sur le raster de classification
  if (!terra::compareGeom(raster_classe, dem, stopOnError = FALSE)) {
    dem <- terra::resample(dem, raster_classe, method = "bilinear")
  }

  cls_vals <- terra::values(raster_classe, na.rm = FALSE)[, 1]
  dem_vals <- terra::values(dem, na.rm = FALSE)[, 1]

  valid <- !is.na(cls_vals) & !is.na(dem_vals)
  cls_vals <- cls_vals[valid]
  dem_vals <- dem_vals[valid]

  unique_cls <- sort(unique(cls_vals))

  result <- do.call(rbind, lapply(unique_cls, function(cl) {
    dem_cl <- dem_vals[cls_vals == cl]
    data.frame(
      code = cl,
      alt_mean = round(mean(dem_cl), 1),
      alt_min = round(min(dem_cl), 1),
      alt_max = round(max(dem_cl), 1),
      alt_sd = round(sd(dem_cl), 1),
      n_pixels = length(dem_cl),
      stringsAsFactors = FALSE
    )
  }))

  result <- merge(result, classes[, c("code", "classe")], by = "code",
                  all.x = TRUE)
  result[order(-result$n_pixels), ]
}

#' Evaluer les predictions par rapport a une reference
#'
#' Calcule les metriques de precision : accuracy globale, mean IoU,
#' et IoU par classe.
#'
#' @param prediction SpatRaster de prediction (classification)
#' @param reference SpatRaster de reference (verite terrain)
#' @param n_classes Nombre de classes (defaut: 19)
#' @param classes Table des classes (optionnel, pour les noms)
#' @return Liste avec `accuracy`, `mean_iou`, `per_class_iou` (data.frame)
#' @export
evaluer_predictions <- function(prediction, reference, n_classes = 19L,
                                 classes = NULL) {
  message("=== Evaluation des predictions ===")

  # Aligner si necessaire
  if (!terra::compareGeom(prediction, reference, stopOnError = FALSE)) {
    reference <- terra::resample(reference, prediction, method = "near")
  }

  pred_vals <- terra::values(prediction, na.rm = FALSE)[, 1]
  ref_vals <- terra::values(reference, na.rm = FALSE)[, 1]

  valid <- !is.na(pred_vals) & !is.na(ref_vals)
  pred_vals <- pred_vals[valid]
  ref_vals <- ref_vals[valid]

  # Accuracy globale
  accuracy <- sum(pred_vals == ref_vals) / length(pred_vals)
  message(sprintf("  Accuracy globale: %.2f%%", accuracy * 100))

  # IoU par classe
  iou_per_class <- numeric(n_classes)
  for (cl in seq_len(n_classes) - 1L) {
    pred_cl <- pred_vals == cl
    ref_cl <- ref_vals == cl
    intersection <- sum(pred_cl & ref_cl)
    union <- sum(pred_cl | ref_cl)
    iou_per_class[cl + 1] <- if (union > 0) intersection / union else NA_real_
  }

  # Mean IoU (en ignorant les classes absentes)
  valid_iou <- iou_per_class[!is.na(iou_per_class)]
  mean_iou <- if (length(valid_iou) > 0) mean(valid_iou) else 0

  message(sprintf("  Mean IoU: %.2f%%", mean_iou * 100))
  message(sprintf("  Classes evaluees: %d / %d", length(valid_iou), n_classes))

  # Data.frame par classe
  per_class <- data.frame(
    code = 0:(n_classes - 1),
    iou = round(iou_per_class * 100, 2),
    stringsAsFactors = FALSE
  )

  if (!is.null(classes)) {
    per_class <- merge(per_class, classes[, c("code", "classe")],
                       by = "code", all.x = TRUE)
  }

  per_class <- per_class[order(-per_class$iou), ]

  list(
    accuracy = accuracy,
    mean_iou = mean_iou,
    per_class_iou = per_class
  )
}

#' Exporter un raster de classification
#'
#' Sauvegarde un raster avec compression LZW.
#'
#' @param r SpatRaster a exporter
#' @param output_path Chemin du fichier de sortie
#' @return Chemin du fichier (invisible)
#' @export
export_raster <- function(r, output_path) {
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  terra::writeRaster(r, output_path, overwrite = TRUE,
                     gdal = c("COMPRESS=LZW"))
  message(sprintf("  Raster exporte: %s", output_path))
  invisible(output_path)
}
