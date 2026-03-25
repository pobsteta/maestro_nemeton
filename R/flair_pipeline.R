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
#'   (defaut: `"IGNF/FLAIR-INC_rgbi_15cl_resnet34-unet"`)
#' @param encoder Architecture encodeur (defaut: `"resnet34"`)
#' @param decoder Architecture decodeur (defaut: `"unet"`)
#' @param n_classes Nombre de classes du modele (defaut: 19, les checkpoints
#'   FLAIR-INC ont 19 canaux de sortie meme pour 15 classes actives)
#' @param dem_channels Canaux DEM a ajouter. `NULL` = pas de DEM (RGBI seul, 4 bandes).
#'   Vecteur de noms parmi `c("DSM", "DTM", "SLOPE", "ASPECT", "TPI", "TWI")`.
#'   Ex: `c("SLOPE", "TWI")` = 6 bandes (RGBI + pente + TWI).
#'   Ex: `"DTM"` = 5 bandes (RGBI + DTM classique).
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
#' # Classification basique RGBI (4 bandes)
#' flair_pipeline("data/aoi.gpkg")
#'
#' # Avec pente + TWI (6 bandes, optimal foret)
#' flair_pipeline("data/aoi.gpkg",
#'                 dem_channels = c("SLOPE", "TWI"),
#'                 model_id = "IGNF/FLAIR-INC_rgbie_15cl_resnet34-unet")
#'
#' # Avec DTM classique (5 bandes)
#' flair_pipeline("data/aoi.gpkg",
#'                 dem_channels = "DTM",
#'                 model_id = "IGNF/FLAIR-INC_rgbie_15cl_resnet34-unet")
#' }
flair_pipeline <- function(aoi_path = "data/aoi.gpkg",
                            output_dir = "outputs",
                            model_id = "IGNF/FLAIR-INC_rgbi_15cl_resnet34-unet",
                            encoder = "resnet34",
                            decoder = "unet",
                            n_classes = 19L,
                            dem_channels = NULL,
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

  # 4. DEM optionnel (canaux terrain configurable)
  input_raster <- rgbi
  n_bands <- 4L

  if (!is.null(dem_channels)) {
    dem_channels <- toupper(dem_channels)
    # Pour FLAIR, on telecharge le DEM a 1m puis on resample a 0.2m
    # car FLAIR concatene toutes les bandes a la meme resolution
    dem_data <- download_dem_for_aoi(aoi, output_dir,
                                      dem_channels = dem_channels)
    if (!is.null(dem_data)) {
      # Reechantillonner le DEM de 1m vers 0.2m pour correspondre au RGBI
      dem_aligned <- terra::resample(dem_data$dem, rgbi, method = "bilinear")
      input_raster <- c(rgbi, dem_aligned)
      n_bands <- 4L + terra::nlyr(dem_aligned)
      message(sprintf("  Input: RGBI + %s (%d bandes)",
                       paste(dem_channels, collapse = " + "), n_bands))
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

  # 10. Assembler les resultats (Python remappe toujours vers CoSIA 0-15)
  resultats <- assembler_resultats_flair(
    raster_classe,
    classes = classes_table,
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
