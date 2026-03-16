# =============================================================================
# Telechargement et rasterisation de la BD Foret V2 (IGN)
# Source : WFS Geoplateforme (https://data.geopf.fr/wfs)
# =============================================================================

# --- Mapping BD Foret V2 -> classes NDP0 (10 classes) ---

#' Classes de segmentation NDP0 (10 classes)
#'
#' Schema de regroupement des codes TFV de la BD Foret V2 en 10 classes
#' pour le premier niveau de produit (NDP0). Les classes sont compatibles
#' avec le backbone MAESTRO et utilisees pour entrainer le decodeur de
#' segmentation a 0.2m.
#'
#' @return data.frame avec code, classe, type et couleur
#' @export
classes_ndp0 <- function() {
  data.frame(
    code = 0:9,
    classe = c(
      "Chene",
      "Hetre",
      "Chataignier",
      "Pin",
      "Epicea/Sapin",
      "Douglas",
      "Meleze",
      "Peuplier",
      "Feuillus divers",
      "Non-foret"
    ),
    type = c(
      "feuillu", "feuillu", "feuillu",
      "resineux", "resineux", "resineux", "resineux",
      "feuillu", "feuillu",
      "autre"
    ),
    couleur = c(
      "#228B22", "#006400", "#8B4513",
      "#FF8C00", "#4169E1", "#8A2BE2", "#FFD700",
      "#90EE90", "#32CD32",
      "#D3D3D3"
    ),
    stringsAsFactors = FALSE
  )
}


#' Mapping des codes TFV vers les classes NDP0
#'
#' Retourne un data.frame de correspondance entre les codes TFV de la
#' BD Foret V2 et les 10 classes NDP0.
#'
#' @return data.frame avec colonnes tfv_pattern (regex) et code_ndp0
#' @export
mapping_tfv_ndp0 <- function() {
  data.frame(
    tfv_pattern = c(
      # 0 - Chene (decidus + sempervirents)
      "^FF1G01", "^FF1G06",
      # 1 - Hetre
      "^FF1-09",
      # 2 - Chataignier
      "^FF1-10",
      # 3 - Pin (maritime, sylvestre, laricio, Alep, crochets, autres, melanges)
      "^FF2-51", "^FF2-52", "^FF2-53", "^FF2-57", "^FF2G58",
      "^FF2-80", "^FF2-81",
      # 4 - Epicea/Sapin
      "^FF2G61",
      # 5 - Douglas
      "^FF2-64",
      # 6 - Meleze
      "^FF2-63",
      # 7 - Peuplier (peupleraie)
      "^FP",
      # 8 - Feuillus divers (robinier, autres feuillus, melanges feuillus,
      #     melanges feuillus/coniferes, melanges coniferes, autres coniferes)
      "^FF1-14", "^FF1-49", "^FF1-00", "^FF31", "^FF32",
      "^FF2-91", "^FF2-00", "^FF2-90"
    ),
    code_ndp0 = c(
      0L, 0L,
      1L,
      2L,
      3L, 3L, 3L, 3L, 3L, 3L, 3L,
      4L,
      5L,
      6L,
      7L,
      8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L
    ),
    stringsAsFactors = FALSE
  )
}


#' Convertir un code TFV en classe NDP0
#'
#' @param code_tfv Vecteur de codes TFV (character)
#' @return Vecteur d'entiers (codes NDP0, 9 = non-foret par defaut)
#' @export
tfv_to_ndp0 <- function(code_tfv) {
  mapping <- mapping_tfv_ndp0()
  result <- rep(9L, length(code_tfv))  # Non-foret par defaut

  for (i in seq_len(nrow(mapping))) {
    mask <- grepl(mapping$tfv_pattern[i], code_tfv)
    result[mask] <- mapping$code_ndp0[i]
  }

  result
}


# --- Telechargement BD Foret V2 via WFS ---

#' Telecharger la BD Foret V2 pour une AOI via WFS
#'
#' Interroge le service WFS de la Geoplateforme IGN pour telecharger
#' les polygones de la BD Foret V2 couvrant la zone d'interet.
#' Les polygones sont reprojectes en Lambert-93 et les codes TFV
#' sont convertis en classes NDP0.
#'
#' @param aoi sf object (AOI en Lambert-93)
#' @param output_dir Repertoire de sortie
#' @param wfs_url URL du service WFS (defaut: Geoplateforme)
#' @param layer_name Nom de la couche WFS BD Foret
#' @return sf data.frame avec les polygones BD Foret et la colonne `code_ndp0`
#' @export
download_bdforet_for_aoi <- function(aoi, output_dir,
                                      wfs_url = "https://data.geopf.fr/wfs/ows",
                                      layer_name = "BDFORET_V2:formation_vegetale") {
  message("=== Telechargement BD Foret V2 via WFS ===")

  cache_path <- file.path(output_dir, "bdforet_v2.gpkg")
  if (file.exists(cache_path)) {
    message("  Cache: ", cache_path)
    bdforet <- sf::st_read(cache_path, quiet = TRUE)
    return(bdforet)
  }

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # Bbox en WGS84 pour la requete WFS
  aoi_wgs84 <- sf::st_transform(aoi, 4326)
  bbox_wgs84 <- sf::st_bbox(aoi_wgs84)
  bbox_str <- sprintf("%f,%f,%f,%f,EPSG:4326",
                       bbox_wgs84["ymin"], bbox_wgs84["xmin"],
                       bbox_wgs84["ymax"], bbox_wgs84["xmax"])

  # Construire l'URL GetFeature
  wfs_params <- list(
    SERVICE = "WFS",
    VERSION = "2.0.0",
    REQUEST = "GetFeature",
    TYPENAMES = layer_name,
    BBOX = bbox_str,
    OUTPUTFORMAT = "application/json",
    COUNT = "50000"
  )
  query_str <- paste(names(wfs_params), wfs_params, sep = "=", collapse = "&")
  url <- paste0(wfs_url, "?", query_str)

  message(sprintf("  Couche WFS: %s", layer_name))
  message(sprintf("  Bbox: %.4f, %.4f, %.4f, %.4f (WGS84)",
                   bbox_wgs84["xmin"], bbox_wgs84["ymin"],
                   bbox_wgs84["xmax"], bbox_wgs84["ymax"]))

  # Telecharger le GeoJSON
  tmp_geojson <- tempfile(fileext = ".geojson")
  tryCatch({
    h <- curl::new_handle()
    curl::handle_setopt(h, followlocation = TRUE, timeout = 120L)
    curl::curl_download(url, tmp_geojson, handle = h)
  }, error = function(e) {
    # Fallback : essayer d'autres noms de couche
    alt_layers <- c(
      "LANDUSE.FORESTINVENTORY.V2:formation_vegetale",
      "bdforet_v2:formation_vegetale",
      "IGNF_BDFORET_V2:formation_vegetale"
    )
    success <- FALSE
    for (alt in alt_layers) {
      message(sprintf("  Essai couche alternative: %s", alt))
      alt_params <- wfs_params
      alt_params$TYPENAMES <- alt
      alt_query <- paste(names(alt_params), alt_params, sep = "=", collapse = "&")
      alt_url <- paste0(wfs_url, "?", alt_query)
      tryCatch({
        curl::curl_download(alt_url, tmp_geojson, handle = h)
        success <- TRUE
        break
      }, error = function(e2) NULL)
    }
    if (!success) {
      stop(sprintf("Echec WFS BD Foret: %s\nEssayez de telecharger manuellement depuis https://geoservices.ign.fr/bdforet",
                    e$message))
    }
  })

  # Lire le GeoJSON
  bdforet <- tryCatch({
    sf::st_read(tmp_geojson, quiet = TRUE)
  }, error = function(e) {
    stop(sprintf("Erreur lecture GeoJSON BD Foret: %s", e$message))
  })
  unlink(tmp_geojson)

  if (nrow(bdforet) == 0) {
    warning("Aucun polygone BD Foret V2 trouve pour cette AOI")
    return(NULL)
  }

  # Reprojeter en Lambert-93
  bdforet <- sf::st_transform(bdforet, 2154)

  # Clipper sur l'AOI
  bdforet <- sf::st_intersection(bdforet, sf::st_union(aoi))

  # Identifier la colonne TFV
  tfv_col <- NULL
  possible_cols <- c("CODE_TFV", "code_tfv", "tfv", "TFV")
  for (col in possible_cols) {
    if (col %in% names(bdforet)) {
      tfv_col <- col
      break
    }
  }

  if (is.null(tfv_col)) {
    # Chercher une colonne qui contient des codes TFV (commencent par FF, FP, LA, FO)
    for (col in names(bdforet)) {
      vals <- as.character(bdforet[[col]])
      if (any(grepl("^(FF|FP|LA|FO)", vals, ignore.case = FALSE))) {
        tfv_col <- col
        break
      }
    }
  }

  if (is.null(tfv_col)) {
    warning("Colonne CODE_TFV non trouvee dans la BD Foret. ",
            "Colonnes disponibles: ", paste(names(bdforet), collapse = ", "))
    bdforet$code_ndp0 <- 9L  # Non-foret par defaut
  } else {
    message(sprintf("  Colonne TFV: %s", tfv_col))
    bdforet$code_ndp0 <- tfv_to_ndp0(as.character(bdforet[[tfv_col]]))
  }

  # Statistiques
  cls <- classes_ndp0()
  freq <- table(bdforet$code_ndp0)
  message(sprintf("  %d polygones BD Foret V2", nrow(bdforet)))
  for (code in sort(unique(bdforet$code_ndp0))) {
    n <- as.integer(freq[as.character(code)])
    label <- cls$classe[cls$code == code]
    message(sprintf("    %d - %s: %d polygones", code, label, n))
  }

  # Sauvegarder en cache
  sf::st_write(bdforet, cache_path, delete_dsn = TRUE, quiet = TRUE)
  message(sprintf("  Sauvegarde: %s", cache_path))

  bdforet
}


#' Rasteriser la BD Foret V2 en masque de classes NDP0
#'
#' Convertit les polygones BD Foret V2 en raster a 0.2m de resolution
#' avec les codes de classes NDP0. Les pixels hors polygones recoivent
#' la classe 9 (Non-foret).
#'
#' @param bdforet sf data.frame avec colonne `code_ndp0`
#'   (issue de [download_bdforet_for_aoi()])
#' @param reference SpatRaster de reference pour l'emprise et la resolution
#'   (ex: ortho RGBI a 0.2m)
#' @param output_dir Repertoire de sortie
#' @return SpatRaster mono-bande (uint8) avec les codes NDP0
#' @export
rasteriser_bdforet <- function(bdforet, reference, output_dir = "outputs") {
  message("=== Rasterisation BD Foret V2 -> masque NDP0 ===")

  cache_path <- file.path(output_dir, "labels_ndp0.tif")
  if (file.exists(cache_path)) {
    message("  Cache: ", cache_path)
    return(terra::rast(cache_path))
  }

  # Creer un template a 0.2m calque sur le raster de reference
  ext_ref <- terra::ext(reference)
  template <- terra::rast(
    xmin = ext_ref[1], xmax = ext_ref[2],
    ymin = ext_ref[3], ymax = ext_ref[4],
    res = terra::res(reference),
    crs = terra::crs(reference)
  )

  # Remplir de 9 (non-foret) par defaut
  terra::values(template) <- 9L

  # Rasteriser les polygones BD Foret par code_ndp0
  # On rasterise chaque classe separement pour gerer les superpositions
  # (la derniere classe ecrite gagne, donc on rasterise non-foret en premier)
  codes_ordonnes <- sort(unique(bdforet$code_ndp0), decreasing = TRUE)

  bdforet_vect <- terra::vect(bdforet)

  for (code in codes_ordonnes) {
    mask_polygons <- bdforet_vect[bdforet_vect$code_ndp0 == code, ]
    if (length(mask_polygons) == 0) next

    layer <- terra::rasterize(mask_polygons, template, field = "code_ndp0")
    # Ecraser les pixels du template la ou le layer n'est pas NA
    valid <- !is.na(terra::values(layer))
    vals <- terra::values(template)
    vals[valid] <- terra::values(layer)[valid]
    terra::values(template) <- vals
  }

  names(template) <- "classe_ndp0"

  # Statistiques
  freq <- table(terra::values(template))
  total <- sum(freq)
  cls <- classes_ndp0()
  message(sprintf("  Raster: %d x %d px (%.1fm)",
                   terra::ncol(template), terra::nrow(template),
                   terra::res(template)[1]))
  for (nm in names(freq)) {
    code <- as.integer(nm)
    n <- as.integer(freq[nm])
    pct <- round(n / total * 100, 1)
    label <- cls$classe[cls$code == code]
    message(sprintf("    %d - %s: %.1f%%", code, label, pct))
  }

  # Sauvegarder
  terra::writeRaster(template, cache_path, overwrite = TRUE,
                     datatype = "INT1U", gdal = c("COMPRESS=LZW"))
  message(sprintf("  Sauvegarde: %s", cache_path))

  template
}


#' Telecharger et rasteriser la BD Foret V2 pour une AOI
#'
#' Fonction combinee : telecharge les polygones via WFS puis rasterise
#' a la resolution du raster de reference (0.2m).
#'
#' @param aoi sf object (AOI en Lambert-93)
#' @param reference SpatRaster de reference (ex: ortho RGBI a 0.2m)
#' @param output_dir Repertoire de sortie
#' @return SpatRaster mono-bande avec les codes NDP0
#' @export
preparer_labels_ndp0 <- function(aoi, reference, output_dir = "outputs") {
  bdforet <- download_bdforet_for_aoi(aoi, output_dir)
  if (is.null(bdforet)) {
    warning("BD Foret non disponible, masque non-foret partout")
    template <- terra::rast(
      ext = terra::ext(reference),
      res = terra::res(reference),
      crs = terra::crs(reference)
    )
    terra::values(template) <- 9L
    names(template) <- "classe_ndp0"
    return(template)
  }
  rasteriser_bdforet(bdforet, reference, output_dir)
}
