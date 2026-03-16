#' Executer le pipeline complet de reconnaissance des essences forestieres
#'
#' Pipeline de bout en bout : charge l'AOI, telecharge les donnees IGN
#' (ortho RVB, IRC, DEM), combine les bandes, telecharge le modele MAESTRO,
#' decoupe en patches, execute l'inference multi-modale et exporte les resultats.
#'
#' Le modele MAESTRO supporte 6 modalites :
#'   - aerial : RGBI 4 bandes (0.2m) — ortho IGN
#'   - dem    : DSM+DTM 2 bandes (1m, resample 0.2m)
#'   - s2     : Sentinel-2 L2A 10 bandes (10m)
#'   - s1_asc : Sentinel-1 ascending VV+VH 2 bandes (10m)
#'   - s1_des : Sentinel-1 descending VV+VH 2 bandes (10m)
#'   - spot   : SPOT RGBI 3 bandes (1.6m) — non disponible via WMS
#'
#' @param aoi_path Chemin vers le fichier GeoPackage de la zone d'interet
#' @param output_dir Repertoire de sortie (defaut: `"outputs"`)
#' @param model_id Identifiant du modele Hugging Face
#'   (defaut: `"IGNF/MAESTRO_FLAIR-HUB_base"`)
#' @param checkpoint Chemin vers un modele fine-tune (.pt). Si fourni,
#'   utilise ce modele au lieu du pretrained MAESTRO. Le nombre de classes
#'   et les essences sont lus du checkpoint.
#'   Ex: `"outputs/training/maestro_treesatai_best.pt"`
#' @param millesime_ortho Millesime de l'ortho RVB (`NULL` = plus recent)
#' @param millesime_irc Millesime de l'ortho IRC (`NULL` = plus recent)
#' @param patch_size Taille des patches en pixels (defaut: 250)
#' @param resolution Resolution spatiale en metres (defaut: 0.2)
#' @param use_s2 Inclure Sentinel-2 (10 bandes) (defaut: FALSE)
#' @param use_s1 Inclure Sentinel-1 ascending + descending (defaut: FALSE)
#' @param date_sentinel Date cible pour les images Sentinel (format "YYYY-MM-DD",
#'   NULL = ete de l'annee en cours). Ignore si `annees_sentinel` est fourni.
#' @param annees_sentinel Vecteur d'annees pour un composite multi-annuel
#'   (ex: `2021:2024`). Telecharge plusieurs scenes par annee et calcule
#'   un composite median pixel par pixel, robuste aux nuages et a la secheresse.
#' @param saison Saison cible pour le composite multitemporel : "ete" (defaut),
#'   "printemps", "automne", "annee", ou vecteur de 2 mois c(debut, fin)
#' @param max_scenes_par_annee Nombre max de scenes a retenir par annee
#'   pour le composite (defaut: 3, les moins nuageuses)
#' @param gpu Utiliser le GPU CUDA (defaut: FALSE)
#' @param token Token Hugging Face (optionnel)
#' @return Liste invisible avec `grille` (sf) et `raster` (SpatRaster)
#' @export
#' @examples
#' \dontrun{
#' # PureForest (13 classes, modele pretrained)
#' maestro_pipeline("data/aoi.gpkg")
#'
#' # TreeSatAI (7 classes, modele fine-tune)
#' maestro_pipeline("data/aoi.gpkg",
#'                   checkpoint = "outputs/training/maestro_treesatai_best.pt")
#'
#' # Toutes les modalites : aerial + DEM + S2 + S1
#' maestro_pipeline("data/aoi.gpkg", use_s2 = TRUE, use_s1 = TRUE)
#'
#' # Avec une date cible pour Sentinel
#' maestro_pipeline("data/aoi.gpkg", use_s2 = TRUE,
#'                   date_sentinel = "2024-07-15")
#'
#' # Composite multi-annuel (3 etes, median pixel par pixel)
#' maestro_pipeline("data/aoi.gpkg", use_s2 = TRUE, use_s1 = TRUE,
#'                   annees_sentinel = 2022:2024, saison = "ete")
#'
#' # Composite sur 5 annees, saison complete
#' maestro_pipeline("data/aoi.gpkg", use_s2 = TRUE, use_s1 = TRUE,
#'                   annees_sentinel = 2020:2024, saison = "annee")
#' }
maestro_pipeline <- function(aoi_path = "data/aoi.gpkg",
                              output_dir = "outputs",
                              model_id = "IGNF/MAESTRO_FLAIR-HUB_base",
                              checkpoint = NULL,
                              millesime_ortho = NULL,
                              millesime_irc = NULL,
                              patch_size = 250L,
                              resolution = 0.2,
                              n_classes = 7L,
                              use_s2 = FALSE,
                              use_s1 = FALSE,
                              date_sentinel = NULL,
                              annees_sentinel = NULL,
                              saison = "ete",
                              max_scenes_par_annee = 3L,
                              gpu = FALSE,
                              token = NULL) {
  # --- Validation des entrees ---
  # Checkpoint fine-tune
  if (!is.null(checkpoint)) {
    if (!file.exists(checkpoint)) {
      stop(sprintf("Checkpoint introuvable: %s", checkpoint))
    }
    if (!grepl("\\.(pt|pth|ckpt|safetensors)$", checkpoint, ignore.case = TRUE)) {
      stop("Le checkpoint doit etre un fichier .pt, .pth, .ckpt ou .safetensors")
    }
    fsize <- file.size(checkpoint)
    if (fsize < 1000) {
      stop(sprintf("Checkpoint trop petit (%d octets) - fichier corrompu ?", fsize))
    }
  }

  # Modalites valides
  valid_modalities <- c("aerial", "dem", "s2", "s1_asc", "s1_des", "spot")
  if (use_s1 && use_s2) {
    expected_mods <- c("aerial", "dem", "s2", "s1_asc", "s1_des")
  } else if (use_s2) {
    expected_mods <- c("aerial", "dem", "s2")
  } else if (use_s1) {
    expected_mods <- c("aerial", "dem", "s1_asc", "s1_des")
  } else {
    expected_mods <- c("aerial", "dem")
  }
  message(sprintf("  Modalites attendues: %s", paste(expected_mods, collapse = ", ")))

  # AOI
  if (!file.exists(aoi_path)) {
    stop(sprintf("Fichier AOI introuvable: %s", aoi_path))
  }

  # Determiner le mode: fine-tune ou pretrained
  is_finetune <- !is.null(checkpoint)

  message("========================================================")
  if (is_finetune) {
    message(" MAESTRO - Reconnaissance des essences forestieres")
    message(sprintf(" Modele fine-tune: %s", basename(checkpoint)))
  } else {
    message(" MAESTRO - Reconnaissance des essences forestieres")
    message(" Modele IGNF multi-modal (aerial + DEM + S2 + S1)")
  }
  message(" Donnees ortho + DEM via Geoplateforme IGN (WMS-R)")
  if (use_s2) message(" + Sentinel-2 (10 bandes, via STAC Planetary Computer)")
  if (use_s1) message(" + Sentinel-1 (VV+VH, via STAC Planetary Computer)")
  if (!is.null(annees_sentinel)) {
    saison_label <- if (is.character(saison)) saison else
      paste(saison, collapse = "-")
    message(sprintf(" + Mode multitemporel: %d annees (%s-%s), saison = %s",
                     length(annees_sentinel), min(annees_sentinel),
                     max(annees_sentinel), saison_label))
    message(sprintf("   Composite median (max %d scenes/annee)",
                     max_scenes_par_annee))
  }
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

  # 5. Sentinel-2 (optionnel)
  if (use_s2) {
    s2 <- download_s2_for_aoi(aoi, output_dir,
                                date_cible = date_sentinel,
                                annees_sentinel = annees_sentinel,
                                saison = saison,
                                max_scenes_par_annee = max_scenes_par_annee)
    if (!is.null(s2)) {
      s2 <- aligner_sentinel(s2, rgbi, target_res = 10)
      modalites$s2 <- s2
      modalites_noms <- c(modalites_noms, "s2")
      message(sprintf("  S2: %d bandes, 10m", terra::nlyr(s2)))
    }
  }

  # 6. Sentinel-1 (optionnel)
  if (use_s1) {
    s1 <- download_s1_for_aoi(aoi, output_dir,
                                date_cible = date_sentinel,
                                annees_sentinel = annees_sentinel,
                                saison = saison,
                                max_scenes_par_annee = max_scenes_par_annee)
    if (!is.null(s1)) {
      if (!is.null(s1$s1_asc)) {
        s1_asc <- aligner_sentinel(s1$s1_asc, rgbi, target_res = 10)
        modalites$s1_asc <- s1_asc
        modalites_noms <- c(modalites_noms, "s1_asc")
        message(sprintf("  S1 ascending: %d bandes (VV, VH), 10m",
                         terra::nlyr(s1_asc)))
      }
      if (!is.null(s1$s1_des)) {
        s1_des <- aligner_sentinel(s1$s1_des, rgbi, target_res = 10)
        modalites$s1_des <- s1_des
        modalites_noms <- c(modalites_noms, "s1_des")
        message(sprintf("  S1 descending: %d bandes (VV, VH), 10m",
                         terra::nlyr(s1_des)))
      }
    }
  }

  # 7. Sauvegarder les rasters pour reference
  for (mod_name in names(modalites)) {
    mod_path <- file.path(output_dir, sprintf("modalite_%s.tif", mod_name))
    terra::writeRaster(modalites[[mod_name]], mod_path, overwrite = TRUE,
                       gdal = c("COMPRESS=LZW"))
    message(sprintf("  %s: %s (%d bandes)", mod_name, mod_path,
                     terra::nlyr(modalites[[mod_name]])))
  }

  # 8. Telecharger le modele (sauf si checkpoint fine-tune fourni)
  if (is_finetune) {
    fichiers_modele <- NULL
  } else if (file.exists(model_id) &&
      grepl("\\.(pt|pth|ckpt|safetensors)$", model_id)) {
    message(sprintf("  Utilisation du checkpoint local: %s", model_id))
    fichiers_modele <- list(weights = model_id)
  } else {
    fichiers_modele <- telecharger_modele(model_id, token)
  }

  # 9. Configurer Python
  configurer_python()

  # 10. Grille de patches
  taille_patch_m <- patch_size * resolution
  grille <- creer_grille_patches(aoi, taille_patch_m)

  # 11. Extraire les patches (multi-modal)
  patches_multimodal <- extraire_patches_multimodal(modalites, grille, patch_size)

  # 12. Inference multi-modale
  if (is_finetune) {
    predictions <- executer_inference_multimodal(
      patches_multimodal, fichiers_modele = NULL,
      modalites = modalites_noms,
      utiliser_gpu = gpu,
      checkpoint = checkpoint
    )
  } else {
    predictions <- executer_inference_multimodal(
      patches_multimodal, fichiers_modele,
      n_classes = n_classes,
      modalites = modalites_noms,
      utiliser_gpu = gpu
    )
  }

  # 13. Assembler et exporter
  essences <- if (is_finetune) essences_treesatai() else essences_pureforest()
  resultats <- assembler_resultats(grille, predictions,
                                    essences = essences,
                                    dossier_sortie = output_dir)

  # 14. Carte raster
  raster_carte <- creer_carte_raster(resultats, resolution, output_dir)

  message("\n========================================================")
  message(" Traitement termine !")
  message(sprintf(" Modalites utilisees: %s", paste(modalites_noms, collapse = " + ")))
  message(sprintf(" Resultats dans : %s/", output_dir))
  message("========================================================")

  invisible(list(grille = resultats, raster = raster_carte))
}


#' Pipeline de segmentation dense MAESTRO a 0.2m
#'
#' Pipeline complet : telecharge les donnees multimodales, charge le backbone
#' MAESTRO + decodeur de segmentation, et produit une carte d'essences
#' forestieres a 0.2m de resolution (classes NDP0, 10 classes).
#'
#' @param aoi_path Chemin vers le fichier GeoPackage de la zone d'interet
#' @param backbone_path Chemin vers le checkpoint MAESTRO pre-entraine (.ckpt)
#' @param decoder_path Chemin vers le decodeur de segmentation (.pt)
#' @param output_dir Repertoire de sortie (defaut: `"outputs"`)
#' @param millesime_ortho Millesime de l'ortho RVB (`NULL` = plus recent)
#' @param millesime_irc Millesime de l'ortho IRC (`NULL` = plus recent)
#' @param patch_size Taille des patches en pixels (defaut: 250)
#' @param resolution Resolution spatiale en metres (defaut: 0.2)
#' @param overlap_m Recouvrement entre patches en metres (defaut: 10)
#' @param use_s2 Inclure Sentinel-2 (defaut: FALSE)
#' @param use_s1 Inclure Sentinel-1 (defaut: FALSE)
#' @param date_sentinel Date cible pour les images Sentinel
#' @param annees_sentinel Vecteur d'annees pour un composite multi-annuel
#' @param saison Saison cible pour le composite multitemporel
#' @param max_scenes_par_annee Nombre max de scenes par annee (defaut: 3)
#' @param gpu Utiliser le GPU CUDA (defaut: FALSE)
#' @return SpatRaster mono-bande avec les codes NDP0 a 0.2m
#' @export
#' @examples
#' \dontrun{
#' # Segmentation avec aerial + DEM
#' maestro_segmentation_pipeline(
#'   "data/aoi.gpkg",
#'   backbone_path = "models/MAESTRO_pretrain.ckpt",
#'   decoder_path = "models/segmenter_ndp0_best.pt"
#' )
#'
#' # Avec toutes les modalites
#' maestro_segmentation_pipeline(
#'   "data/aoi.gpkg",
#'   backbone_path = "models/MAESTRO_pretrain.ckpt",
#'   decoder_path = "models/segmenter_ndp0_best.pt",
#'   use_s2 = TRUE, use_s1 = TRUE
#' )
#' }
maestro_segmentation_pipeline <- function(aoi_path = "data/aoi.gpkg",
                                            backbone_path,
                                            decoder_path,
                                            output_dir = "outputs",
                                            millesime_ortho = NULL,
                                            millesime_irc = NULL,
                                            patch_size = 250L,
                                            resolution = 0.2,
                                            overlap_m = 10,
                                            use_s2 = FALSE,
                                            use_s1 = FALSE,
                                            date_sentinel = NULL,
                                            annees_sentinel = NULL,
                                            saison = "ete",
                                            max_scenes_par_annee = 3L,
                                            gpu = FALSE) {
  # --- Validation ---
  if (!file.exists(aoi_path)) {
    stop(sprintf("Fichier AOI introuvable: %s", aoi_path))
  }
  if (!file.exists(backbone_path)) {
    stop(sprintf("Checkpoint backbone introuvable: %s", backbone_path))
  }
  if (!file.exists(decoder_path)) {
    stop(sprintf("Decodeur segmentation introuvable: %s", decoder_path))
  }

  # Determiner les modalites
  modalites_noms <- c("aerial", "dem")
  if (use_s2) modalites_noms <- c(modalites_noms, "s2")
  if (use_s1) modalites_noms <- c(modalites_noms, "s1_asc", "s1_des")

  message("========================================================")
  message(" MAESTRO - Segmentation dense a 0.2m (NDP0)")
  message(sprintf(" Backbone: %s", basename(backbone_path)))
  message(sprintf(" Decodeur: %s", basename(decoder_path)))
  message(sprintf(" Modalites: %s", paste(modalites_noms, collapse = " + ")))
  if (!is.null(annees_sentinel)) {
    message(sprintf(" Mode multitemporel: %d annees", length(annees_sentinel)))
  }
  message("========================================================\n")

  # 1. Charger l'AOI
  aoi <- load_aoi(aoi_path)

  # 2. Telecharger les ortho IGN
  ortho <- download_ortho_for_aoi(
    aoi, output_dir,
    millesime_ortho = millesime_ortho,
    millesime_irc = millesime_irc
  )

  # 3. Combiner RVB + IRC -> RGBI
  rgbi <- combine_rvb_irc(ortho$rvb, ortho$irc)
  rgbi_path <- file.path(output_dir, "ortho_rgbi.tif")
  terra::writeRaster(rgbi, rgbi_path, overwrite = TRUE)

  # 4. Telecharger le DEM
  dem_data <- download_dem_for_aoi(aoi, output_dir, rgbi = rgbi)

  modalites <- list(aerial = rgbi)
  if (!is.null(dem_data)) {
    modalites$dem <- aligner_dem_sur_rgbi(dem_data$dem, rgbi)
  }

  # 5. Sentinel-2
  if (use_s2) {
    s2 <- download_s2_for_aoi(aoi, output_dir,
                                date_cible = date_sentinel,
                                annees_sentinel = annees_sentinel,
                                saison = saison,
                                max_scenes_par_annee = max_scenes_par_annee)
    if (!is.null(s2)) {
      modalites$s2 <- aligner_sentinel(s2, rgbi, target_res = 10)
    }
  }

  # 6. Sentinel-1
  if (use_s1) {
    s1 <- download_s1_for_aoi(aoi, output_dir,
                                date_cible = date_sentinel,
                                annees_sentinel = annees_sentinel,
                                saison = saison,
                                max_scenes_par_annee = max_scenes_par_annee)
    if (!is.null(s1)) {
      if (!is.null(s1$s1_asc)) {
        modalites$s1_asc <- aligner_sentinel(s1$s1_asc, rgbi, target_res = 10)
      }
      if (!is.null(s1$s1_des)) {
        modalites$s1_des <- aligner_sentinel(s1$s1_des, rgbi, target_res = 10)
      }
    }
  }

  # 7. Configurer Python + charger le segmenter
  segmenter <- charger_segmenter(
    backbone_path = backbone_path,
    decoder_path = decoder_path,
    modalites = names(modalites),
    gpu = gpu
  )

  # 8. Segmentation dense
  raster_seg <- executer_segmentation(
    segmenter = segmenter,
    modalites = modalites,
    aoi = aoi,
    output_dir = output_dir,
    patch_size = patch_size,
    resolution = resolution,
    overlap_m = overlap_m,
    gpu = gpu
  )

  message("\n========================================================")
  message(" Segmentation terminee !")
  message(sprintf(" Carte NDP0 a 0.2m: %s/segmentation_ndp0.tif",
                   output_dir))
  message("========================================================")

  invisible(raster_seg)
}


#' Pipeline de preparation des donnees d'entrainement pour la segmentation
#'
#' Telecharge les donnees multimodales et la BD Foret V2 pour l'AOI,
#' rasterise les labels NDP0, et decoupe le tout en patches d'entrainement.
#'
#' @param aoi_path Chemin vers le fichier GeoPackage de la zone d'interet
#' @param output_dir Repertoire de sortie pour les patches
#' @param millesime_ortho Millesime de l'ortho RVB
#' @param millesime_irc Millesime de l'ortho IRC
#' @param patch_size Taille des patches en pixels (defaut: 250)
#' @param resolution Resolution en metres (defaut: 0.2)
#' @param val_ratio Proportion de validation (defaut: 0.15)
#' @param min_forest_pct Pourcentage minimum de foret par patch (defaut: 10)
#' @param use_s2 Inclure Sentinel-2 (defaut: FALSE)
#' @param use_s1 Inclure Sentinel-1 (defaut: FALSE)
#' @param date_sentinel Date cible pour Sentinel
#' @param annees_sentinel Vecteur d'annees pour composite
#' @param saison Saison cible
#' @param max_scenes_par_annee Nombre max de scenes par annee
#' @return Liste avec n_train, n_val, n_skipped
#' @export
preparer_donnees_segmentation <- function(aoi_path = "data/aoi.gpkg",
                                            output_dir = "data/segmentation",
                                            millesime_ortho = NULL,
                                            millesime_irc = NULL,
                                            patch_size = 250L,
                                            resolution = 0.2,
                                            val_ratio = 0.15,
                                            min_forest_pct = 10,
                                            use_s2 = FALSE,
                                            use_s1 = FALSE,
                                            date_sentinel = NULL,
                                            annees_sentinel = NULL,
                                            saison = "ete",
                                            max_scenes_par_annee = 3L) {
  if (!file.exists(aoi_path)) {
    stop(sprintf("Fichier AOI introuvable: %s", aoi_path))
  }

  message("========================================================")
  message(" MAESTRO - Preparation donnees entrainement segmentation")
  message("========================================================\n")

  tmp_dir <- file.path(output_dir, "tmp_rasters")
  if (!dir.exists(tmp_dir)) dir.create(tmp_dir, recursive = TRUE)

  # 1. Charger l'AOI
  aoi <- load_aoi(aoi_path)

  # 2. Telecharger les ortho IGN
  ortho <- download_ortho_for_aoi(
    aoi, tmp_dir,
    millesime_ortho = millesime_ortho,
    millesime_irc = millesime_irc
  )
  rgbi <- combine_rvb_irc(ortho$rvb, ortho$irc)

  # 3. DEM
  dem_data <- download_dem_for_aoi(aoi, tmp_dir, rgbi = rgbi)

  modalites <- list(aerial = rgbi)
  if (!is.null(dem_data)) {
    modalites$dem <- aligner_dem_sur_rgbi(dem_data$dem, rgbi)
  }

  # 4. Sentinel-2
  if (use_s2) {
    s2 <- download_s2_for_aoi(aoi, tmp_dir,
                                date_cible = date_sentinel,
                                annees_sentinel = annees_sentinel,
                                saison = saison,
                                max_scenes_par_annee = max_scenes_par_annee)
    if (!is.null(s2)) {
      modalites$s2 <- aligner_sentinel(s2, rgbi, target_res = 10)
    }
  }

  # 5. Sentinel-1
  if (use_s1) {
    s1 <- download_s1_for_aoi(aoi, tmp_dir,
                                date_cible = date_sentinel,
                                annees_sentinel = annees_sentinel,
                                saison = saison,
                                max_scenes_par_annee = max_scenes_par_annee)
    if (!is.null(s1)) {
      if (!is.null(s1$s1_asc)) {
        modalites$s1_asc <- aligner_sentinel(s1$s1_asc, rgbi, target_res = 10)
      }
      if (!is.null(s1$s1_des)) {
        modalites$s1_des <- aligner_sentinel(s1$s1_des, rgbi, target_res = 10)
      }
    }
  }

  # 6. Telecharger et rasteriser la BD Foret V2
  labels <- preparer_labels_ndp0(aoi, rgbi, tmp_dir)

  # 7. Decouper en patches
  result <- preparer_patches_entrainement(
    modalites = modalites,
    labels = labels,
    aoi = aoi,
    output_dir = output_dir,
    patch_size = patch_size,
    resolution = resolution,
    val_ratio = val_ratio,
    min_forest_pct = min_forest_pct
  )

  message("\n========================================================")
  message(" Donnees d'entrainement pretes !")
  message(sprintf(" %s/", output_dir))
  message("========================================================")

  invisible(result)
}
