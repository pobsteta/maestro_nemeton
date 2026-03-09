#' Executer le pipeline FLAIR d'occupation du sol
#'
#' Pipeline de bout en bout : charge l'AOI, telecharge les donnees IGN
#' (ortho RVB, IRC, DEM optionnel), combine les bandes, telecharge le modele
#' FLAIR pre-entraine, execute la segmentation semantique pixel-a-pixel
#' avec blending Hann, et exporte les resultats.
#'
#' Les modeles FLAIR produisent des cartes de classification par pixel,
#' contrairement a MAESTRO qui classifie par patch.
#'
#' @param aoi_path Chemin vers le fichier GeoPackage de la zone d'interet
#' @param output_dir Repertoire de sortie (defaut: `"outputs"`)
#' @param model_id Identifiant du modele Hugging Face
#'   (defaut: `"IGNF/FLAIR-INC_RGBI_15cl"`)
#' @param encoder Architecture encodeur (defaut: `"resnet34"`)
#' @param decoder Architecture decodeur (defaut: `"unet"`)
#' @param n_classes Nombre de classes (defaut: 19 pour CoSIA)
#' @param use_dem Utiliser le DEM comme 5e bande (defaut: FALSE)
#' @param millesime_ortho Millesime de l'ortho RVB (`NULL` = plus recent)
#' @param millesime_irc Millesime de l'ortho IRC (`NULL` = plus recent)
#' @param patch_size Taille des patches en pixels (defaut: 512)
#' @param overlap Recouvrement entre patches (defaut: 128)
#' @param gpu Utiliser le GPU CUDA (defaut: FALSE)
#' @param token Token Hugging Face (optionnel)
#' @return Liste invisible avec `raster` (SpatRaster) et `stats` (data.frame)
#' @export
#' @examples
#' \dontrun{
#' # Classification basique RGBI
#' flair_pipeline("data/aoi.gpkg")
#'
#' # Avec DEM (5 bandes, LC-B)
#' flair_pipeline("data/aoi.gpkg", use_dem = TRUE,
#'                 model_id = "IGNF/FLAIR-HUB_RGBI-DEM_19cl")
#' }
flair_pipeline <- function(aoi_path = "data/aoi.gpkg",
                            output_dir = "outputs",
                            model_id = "IGNF/FLAIR-INC_RGBI_15cl",
                            encoder = "resnet34",
                            decoder = "unet",
                            n_classes = 19L,
                            use_dem = FALSE,
                            millesime_ortho = NULL,
                            millesime_irc = NULL,
                            patch_size = 512L,
                            overlap = 128L,
                            gpu = FALSE,
                            token = NULL) {
  message("========================================================")
  message(" FLAIR - Classification d'occupation du sol")
  message(" Segmentation semantique pixel-a-pixel")
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

  # 4. DEM optionnel (5e bande)
  n_bands <- 4L
  input_raster <- rgbi

  if (use_dem) {
    dem_data <- download_dem_for_aoi(aoi, output_dir, rgbi = rgbi)
    if (!is.null(dem_data)) {
      dem <- aligner_dem_sur_rgbi(dem_data$dem, rgbi)
      # Prendre seulement la premiere bande du DEM (CHM ou DTM)
      input_raster <- c(rgbi, dem[[1]])
      n_bands <- 5L
      message(sprintf("  Input: RGBI + DEM (%d bandes)", n_bands))
    } else {
      message("  DEM non disponible, utilisation de RGBI seul")
    }
  }

  # 5. Calculer les indices spectraux
  message("\n=== Indices spectraux ===")
  ndvi <- compute_ndvi(rgbi)
  ndvi_path <- file.path(output_dir, "ndvi.tif")
  terra::writeRaster(ndvi, ndvi_path, overwrite = TRUE, gdal = c("COMPRESS=LZW"))
  message(sprintf("  NDVI sauvegarde: %s", ndvi_path))

  # 6. Telecharger le modele FLAIR
  fichiers_modele <- telecharger_modele_flair(model_id, token)

  # 7. Configurer Python
  configurer_python()

  # 8. Charger le modele
  chemin_poids <- fichiers_modele$weights %||% fichiers_modele$snapshot
  if (is.null(chemin_poids)) {
    stop("Impossible de trouver les poids du modele FLAIR.")
  }

  modele <- charger_modele_flair(
    chemin_poids, n_classes = n_classes,
    in_channels = n_bands,
    encoder = encoder, decoder = decoder,
    device = if (gpu) "cuda" else "cpu"
  )

  # 9. Inference avec blending Hann
  raster_classe <- executer_inference_flair(
    input_raster, modele,
    patch_size = patch_size,
    overlap = overlap,
    n_classes = n_classes,
    utiliser_gpu = gpu
  )

  # 10. Assembler les resultats
  resultats <- assembler_resultats_flair(
    raster_classe,
    classes = classes_cosia(),
    dossier_sortie = output_dir
  )

  message("\n========================================================")
  message(" Traitement FLAIR termine !")
  message(sprintf(" Configuration: %s + %s (%d bandes, %d classes)",
                   encoder, decoder, n_bands, n_classes))
  message(sprintf(" Resultats dans : %s/", output_dir))
  message("========================================================")

  invisible(resultats)
}
