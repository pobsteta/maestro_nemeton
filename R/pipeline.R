#' Executer le pipeline complet de reconnaissance des essences forestieres
#'
#' Pipeline de bout en bout : charge l'AOI, telecharge les donnees IGN
#' (ortho RVB, IRC, DEM), combine les bandes, telecharge le modele MAESTRO,
#' decoupe en patches, execute l'inference multi-modale et exporte les resultats.
#'
#' Le modele MAESTRO attend des entrees multi-modales separees :
#'   - aerial : RGBI 4 bandes (0.2m)
#'   - dem    : DSM+DTM 2 bandes (1m, resample 0.2m)
#'
#' @param aoi_path Chemin vers le fichier GeoPackage de la zone d'interet
#' @param output_dir Repertoire de sortie (defaut: `"outputs"`)
#' @param model_id Identifiant du modele Hugging Face
#'   (defaut: `"IGNF/MAESTRO_FLAIR-HUB_base"`)
#' @param millesime_ortho Millesime de l'ortho RVB (`NULL` = plus recent)
#' @param millesime_irc Millesime de l'ortho IRC (`NULL` = plus recent)
#' @param patch_size Taille des patches en pixels (defaut: 250)
#' @param resolution Resolution spatiale en metres (defaut: 0.2)
#' @param gpu Utiliser le GPU CUDA (defaut: FALSE)
#' @param token Token Hugging Face (optionnel)
#' @return Liste invisible avec `grille` (sf) et `raster` (SpatRaster)
#' @export
#' @examples
#' \dontrun{
#' # Millesime par defaut
#' maestro_pipeline("data/aoi.gpkg")
#'
#' # Millesime specifique
#' maestro_pipeline("data/aoi.gpkg",
#'                   millesime_ortho = 2023,
#'                   millesime_irc = 2023)
#' }
maestro_pipeline <- function(aoi_path = "data/aoi.gpkg",
                              output_dir = "outputs",
                              model_id = "IGNF/MAESTRO_FLAIR-HUB_base",
                              millesime_ortho = NULL,
                              millesime_irc = NULL,
                              patch_size = 250L,
                              resolution = 0.2,
                              gpu = FALSE,
                              token = NULL) {
  message("========================================================")
  message(" MAESTRO - Reconnaissance des essences forestieres")
  message(" Modele IGNF multi-modal (aerial + DEM)")
  message(" Donnees ortho + DEM via Geoplateforme IGN (WMS-R)")
  message("========================================================\n")

  # 1. Charger l'AOI
  aoi <- load_aoi(aoi_path)

  # 2. Telecharger les ortho IGN
  ortho <- download_ortho_for_aoi(
    aoi, output_dir,
    millesime_ortho = millesime_ortho,
    millesime_irc = millesime_irc
  )

  # 3. Combiner RVB + IRC -> RGBI (4 bandes)
  rgbi <- combine_rvb_irc(ortho$rvb, ortho$irc)

  rgbi_path <- file.path(output_dir, "ortho_rgbi.tif")
  terra::writeRaster(rgbi, rgbi_path, overwrite = TRUE)
  message(sprintf("RGBI sauvegarde: %s", rgbi_path))

  # 4. Telecharger le DEM (DSM + DTM, 2 bandes)
  dem_data <- download_dem_for_aoi(aoi, output_dir, rgbi = rgbi)

  # Preparer les modalites disponibles
  modalites <- list(aerial = rgbi)
  modalites_noms <- c("aerial")

  if (!is.null(dem_data)) {
    dem <- aligner_dem_sur_rgbi(dem_data$dem, rgbi)
    modalites$dem <- dem
    modalites_noms <- c(modalites_noms, "dem")
    message(sprintf("  DEM: %d bandes (source DSM: %s)",
                     terra::nlyr(dem), dem_data$dsm_source))
  } else {
    message("  DEM non disponible, utilisation de aerial seul")
  }

  # 5. Sauvegarder les rasters pour reference
  for (mod_name in names(modalites)) {
    mod_path <- file.path(output_dir, sprintf("modalite_%s.tif", mod_name))
    terra::writeRaster(modalites[[mod_name]], mod_path, overwrite = TRUE,
                       gdal = c("COMPRESS=LZW"))
    message(sprintf("  %s: %s (%d bandes)", mod_name, mod_path,
                     terra::nlyr(modalites[[mod_name]])))
  }

  # 6. Telecharger le modele
  fichiers_modele <- telecharger_modele(model_id, token)

  # 7. Configurer Python
  configurer_python()

  # 8. Grille de patches
  taille_patch_m <- patch_size * resolution
  grille <- creer_grille_patches(aoi, taille_patch_m)

  # 9. Extraire les patches (multi-modal)
  patches_multimodal <- extraire_patches_multimodal(modalites, grille, patch_size)

  # 10. Inference multi-modale
  predictions <- executer_inference_multimodal(
    patches_multimodal, fichiers_modele,
    n_classes = 13L,
    modalites = modalites_noms,
    utiliser_gpu = gpu
  )

  # 11. Assembler et exporter
  resultats <- assembler_resultats(grille, predictions,
                                    dossier_sortie = output_dir)

  # 12. Carte raster
  raster_carte <- creer_carte_raster(resultats, resolution, output_dir)

  message("\n========================================================")
  message(" Traitement termine !")
  message(sprintf(" Modalites utilisees: %s", paste(modalites_noms, collapse = " + ")))
  message(sprintf(" Resultats dans : %s/", output_dir))
  message("========================================================")

  invisible(list(grille = resultats, raster = raster_carte))
}
