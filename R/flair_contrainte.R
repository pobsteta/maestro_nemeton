# =============================================================================
# Contrainte FLAIR sur la segmentation MAESTRO
#
# Utilise la classification FLAIR (feuillus/resineux) pour contraindre
# les predictions de segmentation MAESTRO (10 classes NDP0).
# =============================================================================

#' Contraindre la segmentation MAESTRO par la classification FLAIR
#'
#' Post-traitement qui utilise la carte FLAIR (CoSIA 19 classes) pour :
#' 1. Forcer les pixels non-foret a la classe NDP0 = 9
#' 2. Contraindre les pixels "Conifere" FLAIR aux essences resineuses
#' 3. Contraindre les pixels "Feuillu" FLAIR aux essences feuillues
#' 4. Gerer les pixels "Mixte" en gardant la prediction MAESTRO
#'
#' @param raster_seg SpatRaster de segmentation MAESTRO (classes NDP0 0-9)
#' @param raster_flair SpatRaster de classification FLAIR (classes CoSIA 1-19)
#' @param garder_mixte Logical. Garder la prediction MAESTRO pour les pixels
#'   "Mixte conifere+feuillu" (classe 17) ? Si `FALSE`, force en feuillus
#'   divers (defaut `TRUE`).
#' @param garder_ligneux Logical. Traiter les pixels "Ligneux" (classe 18)
#'   comme foret ? (defaut `TRUE`).
#' @return Liste avec :
#'   - `raster` : SpatRaster contraint
#'   - `stats` : data.frame avec les corrections appliquees
#' @export
#' @examples
#' \dontrun{
#' # Lancer FLAIR puis contraindre
#' flair_result <- flair_pipeline("data/aoi.gpkg", output_dir = "outputs")
#' raster_flair <- terra::rast("outputs/occupation_sol.tif")
#'
#' result <- contraindre_par_flair(raster_seg, raster_flair)
#' terra::plot(result$raster)
#' }
contraindre_par_flair <- function(raster_seg,
                                   raster_flair,
                                   garder_mixte = TRUE,
                                   garder_ligneux = TRUE) {

  message("=== Contrainte FLAIR sur segmentation MAESTRO ===")

  # Aligner FLAIR sur la grille MAESTRO si necessaire
  if (!terra::compareGeom(raster_seg, raster_flair, stopOnError = FALSE)) {
    message("  Alignement FLAIR -> grille MAESTRO (nearest neighbor)...")
    raster_flair <- terra::resample(raster_flair, raster_seg, method = "near")
  }

  # Classes CoSIA forestieres (0-based : argmax du modele Python)
  # Correspondance : index 0-based = code classes_cosia() - 1
  CLS_CONIFERE <- 5L   # "Conifere" = code CoSIA 6, index 5
  CLS_FEUILLU  <- 6L   # "Feuillu" = code CoSIA 7, index 6
  CLS_MIXTE    <- 16L  # "Mixte conifere+feuillu" = code 17, index 16
  CLS_LIGNEUX  <- 17L  # "Ligneux" = code 18, index 17
  CLS_COUPE    <- 15L  # "Coupe forestiere" = code 16, index 15

  # Classes NDP0 par type
  ndp0_feuillus <- c(0L, 1L, 2L, 7L, 8L)  # Chene, Hetre, Chataignier, Peuplier, Feuillus divers
  ndp0_resineux <- c(3L, 4L, 5L, 6L)       # Pin, Epicea/Sapin, Douglas, Meleze
  ndp0_non_foret <- 9L

  # Classes FLAIR considerees comme foret
  cls_foret <- c(CLS_CONIFERE, CLS_FEUILLU)
  if (garder_mixte) cls_foret <- c(cls_foret, CLS_MIXTE)
  if (garder_ligneux) cls_foret <- c(cls_foret, CLS_LIGNEUX)
  cls_foret <- c(cls_foret, CLS_COUPE)  # Coupe = zone forestiere temporairement deboisee

  # Extraire les valeurs
  seg_vals <- terra::values(raster_seg, na.rm = FALSE)[, 1]
  flair_vals <- terra::values(raster_flair, na.rm = FALSE)[, 1]

  n_total <- length(seg_vals)
  n_valid <- sum(!is.na(seg_vals) & !is.na(flair_vals))

  # Copie pour modifications
  result_vals <- seg_vals

  # Compteurs de corrections
  n_non_foret <- 0L
  n_conifere_corr <- 0L
  n_feuillu_corr <- 0L
  n_inchange <- 0L

  # 1. Pixels FLAIR = non-foret -> NDP0 = 9
  mask_non_foret <- !is.na(flair_vals) & !(flair_vals %in% cls_foret)
  mask_etait_foret <- mask_non_foret & !is.na(seg_vals) & seg_vals != ndp0_non_foret
  n_non_foret <- sum(mask_etait_foret)
  result_vals[mask_non_foret] <- ndp0_non_foret

  # 2. Pixels FLAIR = Conifere -> garder uniquement essences resineuses
  mask_conifere <- !is.na(flair_vals) & flair_vals == CLS_CONIFERE
  mask_conf_feuillu <- mask_conifere & !is.na(seg_vals) & seg_vals %in% ndp0_feuillus
  n_conifere_corr <- sum(mask_conf_feuillu)
  # Remplacer par l'essence resineuse avec la proba la plus haute ?
  # Simplification : forcer Pin (classe la plus frequente des resineux)
  # TODO: utiliser les probas MAESTRO pour choisir le meilleur resineux
  result_vals[mask_conf_feuillu] <- 3L  # Pin par defaut

  # 3. Pixels FLAIR = Feuillu -> garder uniquement essences feuillues
  mask_feuillu <- !is.na(flair_vals) & flair_vals == CLS_FEUILLU
  mask_feu_resineux <- mask_feuillu & !is.na(seg_vals) & seg_vals %in% ndp0_resineux
  n_feuillu_corr <- sum(mask_feu_resineux)
  # Remplacer par Feuillus divers (classe generique)
  result_vals[mask_feu_resineux] <- 8L  # Feuillus divers par defaut

  # 4. Pixels FLAIR = Mixte : garder la prediction MAESTRO (pas de contrainte)
  # 5. Pixels FLAIR = Ligneux : garder la prediction MAESTRO
  # 6. Pixels FLAIR = Coupe forestiere : forcer non-foret
  mask_coupe <- !is.na(flair_vals) & flair_vals == CLS_COUPE
  result_vals[mask_coupe] <- ndp0_non_foret

  n_inchange <- n_valid - n_non_foret - n_conifere_corr - n_feuillu_corr

  # Ecrire le resultat
  raster_result <- terra::rast(raster_seg)
  terra::values(raster_result) <- result_vals
  names(raster_result) <- "classe_ndp0_flair"

  # Statistiques
  stats <- data.frame(
    correction = c(
      "Pixels valides",
      "Non-foret (FLAIR) -> NDP0=9",
      "Conifere (FLAIR) + feuillu (MAESTRO) -> resineux",
      "Feuillu (FLAIR) + resineux (MAESTRO) -> feuillu",
      "Coupe forestiere -> non-foret",
      "Inchanges"
    ),
    n_pixels = c(
      n_valid,
      n_non_foret,
      n_conifere_corr,
      n_feuillu_corr,
      sum(mask_coupe & !is.na(seg_vals) & seg_vals != ndp0_non_foret),
      n_inchange
    ),
    stringsAsFactors = FALSE
  )
  stats$proportion <- round(stats$n_pixels / n_valid * 100, 2)

  message(sprintf("  Pixels traites      : %d", n_valid))
  message(sprintf("  Non-foret forces    : %d (%.1f%%)",
                   n_non_foret, n_non_foret / n_valid * 100))
  message(sprintf("  Conifere corriges   : %d (%.1f%%)",
                   n_conifere_corr, n_conifere_corr / n_valid * 100))
  message(sprintf("  Feuillu corriges    : %d (%.1f%%)",
                   n_feuillu_corr, n_feuillu_corr / n_valid * 100))
  message(sprintf("  Inchanges           : %d (%.1f%%)",
                   n_inchange, n_inchange / n_valid * 100))

  list(raster = raster_result, stats = stats)
}


#' Lancer FLAIR puis contraindre la segmentation MAESTRO
#'
#' Pipeline complet qui :
#' 1. Execute l'inference FLAIR pour obtenir une carte feuillus/resineux
#' 2. Applique la contrainte sur la segmentation MAESTRO existante
#'
#' @param raster_seg SpatRaster de segmentation MAESTRO (classes NDP0)
#' @param rgbi SpatRaster RGBI 4 bandes pour l'inference FLAIR
#' @param dem SpatRaster DEM (optionnel, pour le modele RGBI-DEM 5 bandes)
#' @param output_dir Repertoire de sortie
#' @param model_flair Identifiant du modele FLAIR HuggingFace
#'   (defaut `"IGNF/FLAIR-HUB_RGBI_19cl"`).
#' @param gpu Logical. Utiliser le GPU ? (defaut `FALSE`).
#' @return Liste avec `raster` contraint et `stats` des corrections
#' @export
#' @examples
#' \dontrun{
#' result <- pipeline_flair_contrainte(
#'   raster_seg = terra::rast("outputs/segmentation_ndp0.tif"),
#'   rgbi       = terra::rast("outputs/ortho_rgbi.tif"),
#'   output_dir = "outputs"
#' )
#' }
pipeline_flair_contrainte <- function(raster_seg,
                                       rgbi,
                                       dem = NULL,
                                       output_dir = "outputs",
                                       model_flair = "IGNF/FLAIR-INC_rgbi_15cl_resnet34-unet",
                                       gpu = FALSE) {

  message("=== Pipeline FLAIR + contrainte segmentation ===")

  # 1. Preparer l'input FLAIR
  if (!is.null(dem) && grepl("DEM", model_flair)) {
    message("  Modele FLAIR avec DEM (5 bandes)")
    input_flair <- c(rgbi, dem[[1]])
  } else {
    input_flair <- rgbi
  }

  # 2. Telecharger et charger le modele FLAIR
  message("  Telechargement du modele FLAIR: ", model_flair)
  poids_flair <- telecharger_modele_flair(model_flair)
  modele_flair <- charger_modele_flair(poids_flair, n_bands = terra::nlyr(input_flair))

  # 3. Inference FLAIR
  message("  Inference FLAIR...")
  raster_flair <- executer_inference_flair(
    modele_flair, input_flair,
    utiliser_gpu = gpu
  )

  # 4. Sauvegarder la carte FLAIR
  flair_path <- file.path(output_dir, "occupation_sol_flair.tif")
  terra::writeRaster(raster_flair, flair_path, overwrite = TRUE,
                     datatype = "INT1U", gdal = c("COMPRESS=LZW"))
  message("  Carte FLAIR sauvegardee: ", flair_path)

  # 5. Contraindre
  result <- contraindre_par_flair(raster_seg, raster_flair)

  # 6. Sauvegarder le resultat contraint
  out_path <- file.path(output_dir, "segmentation_ndp0_flair.tif")
  terra::writeRaster(result$raster, out_path, overwrite = TRUE,
                     datatype = "INT1U", gdal = c("COMPRESS=LZW"))
  message("  Segmentation contrainte sauvegardee: ", out_path)

  result
}
