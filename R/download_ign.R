#' Construire le nom de couche WMS IGN selon le millesime
#'
#' @param type `"ortho"` ou `"irc"`
#' @param millesime `NULL` (mosaique la plus recente) ou entier (ex: 2023)
#' @return Nom de couche WMS (character)
#' @export
#' @examples
#' ign_layer_name("ortho")
#' ign_layer_name("irc", 2023)
ign_layer_name <- function(type = c("ortho", "irc"), millesime = NULL) {
  type <- match.arg(type)
  if (is.null(millesime)) {
    if (type == "ortho") return(.ign_config$LAYER_ORTHO)
    else                 return(.ign_config$LAYER_IRC)
  }
  millesime <- as.character(millesime)
  if (type == "ortho") {
    return(paste0("ORTHOIMAGERY.ORTHOPHOTOS", millesime))
  } else {
    return(paste0("ORTHOIMAGERY.ORTHOPHOTOS.IRC.", millesime))
  }
}

#' Telecharger une tuile WMS IGN
#'
#' Envoie une requete WMS 1.3.0 a la Geoplateforme IGN et retourne un
#' SpatRaster. Gere le retry avec backoff exponentiel.
#'
#' @param bbox Vecteur numerique `c(xmin, ymin, xmax, ymax)` en Lambert-93
#' @param layer Nom de couche WMS
#' @param res_m Resolution en metres (defaut: 0.2)
#' @param dest_file Chemin du fichier GeoTIFF de sortie
#' @param styles Style WMS (`""` pour ortho, `"normal"` pour elevation)
#' @param max_retries Nombre maximal de tentatives (defaut: 3)
#' @return SpatRaster ou NULL si echec
#' @export
download_wms_tile <- function(bbox, layer, res_m = .ign_config$RES_IGN,
                               dest_file, styles = "", max_retries = 3) {
  xmin <- bbox[1]; ymin <- bbox[2]; xmax <- bbox[3]; ymax <- bbox[4]

  width  <- round((xmax - xmin) / res_m)
  height <- round((ymax - ymin) / res_m)

  wms_url <- paste0(
    .ign_config$WMS_URL, "?",
    "SERVICE=WMS&VERSION=1.3.0&REQUEST=GetMap",
    "&LAYERS=", layer,
    "&CRS=EPSG:2154",
    "&BBOX=", paste(xmin, ymin, xmax, ymax, sep = ","),
    "&WIDTH=", width,
    "&HEIGHT=", height,
    "&FORMAT=image/geotiff",
    "&STYLES=", styles
  )

  h <- curl::new_handle()
  curl::handle_setopt(h, http_version = 2L)  # CURL_HTTP_VERSION_1_1

  tmp_file <- tempfile(fileext = ".tif")

  for (attempt in seq_len(max_retries)) {
    result <- tryCatch({
      curl::curl_download(url = wms_url, destfile = tmp_file,
                          quiet = TRUE, handle = h)

      r <- terra::rast(tmp_file)

      if (is.na(terra::crs(r)) || terra::crs(r) == "") {
        terra::crs(r) <- "EPSG:2154"
      }
      terra::ext(r) <- terra::ext(xmin, xmax, ymin, ymax)

      terra::writeRaster(r, dest_file, overwrite = TRUE)
      r <- terra::rast(dest_file)
      unlink(tmp_file)

      return(r)
    }, error = function(e) {
      e
    })

    if (!inherits(result, "error")) return(result)

    if (attempt < max_retries) {
      wait_s <- 2^attempt
      message(sprintf("  Retry %d/%d dans %ds (%s)",
                       attempt, max_retries, wait_s, result$message))
      Sys.sleep(wait_s)
    } else {
      unlink(tmp_file)
      warning("Echec WMS apres ", max_retries, " tentatives: ", result$message)
      return(NULL)
    }
  }
}

#' Telecharger une couche WMS IGN avec tuilage automatique
#'
#' Subdivise les grandes emprises en tuiles respectant la limite WMS de
#' 4096 px, les telecharge, puis les mosaique en un seul raster.
#'
#' @param bbox Vecteur numerique `c(xmin, ymin, xmax, ymax)` en Lambert-93
#' @param layer Nom de couche WMS
#' @param res_m Resolution en metres
#' @param output_dir Repertoire de sortie pour les tuiles temporaires
#' @param prefix Prefixe des fichiers temporaires
#' @param styles Style WMS
#' @return SpatRaster mosaique
#' @export
download_ign_tiled <- function(bbox, layer, res_m = .ign_config$RES_IGN,
                                output_dir, prefix = "ortho",
                                styles = "") {
  xmin <- bbox[1]; ymin <- bbox[2]; xmax <- bbox[3]; ymax <- bbox[4]
  tile_size_m <- .ign_config$WMS_MAX_PX * res_m

  x_starts <- seq(xmin, xmax, by = tile_size_m)
  y_starts <- seq(ymin, ymax, by = tile_size_m)

  n_tiles <- length(x_starts) * length(y_starts)
  message(sprintf("Telechargement %s: %d tuile(s) WMS...", prefix, n_tiles))

  tile_rasters <- list()
  idx <- 1

  for (x0 in x_starts) {
    for (y0 in y_starts) {
      x1 <- min(x0 + tile_size_m, xmax)
      y1 <- min(y0 + tile_size_m, ymax)

      if ((x1 - x0) < res_m * 2 || (y1 - y0) < res_m * 2) next

      tile_bbox <- c(x0, y0, x1, y1)
      tile_file <- file.path(output_dir,
                              sprintf("%s_tile_%03d.tif", prefix, idx))

      message(sprintf("  Tuile %d/%d [%.0f,%.0f - %.0f,%.0f]...",
                       idx, n_tiles, x0, y0, x1, y1))

      r <- download_wms_tile(tile_bbox, layer, res_m, tile_file,
                              styles = styles)
      if (!is.null(r)) {
        tile_rasters[[idx]] <- r
      }
      idx <- idx + 1
    }
  }

  if (length(tile_rasters) == 0) {
    stop("Aucune tuile WMS telechargee avec succes.")
  }

  if (length(tile_rasters) == 1) {
    mosaic <- tile_rasters[[1]]
  } else {
    message("Mosaiquage de ", length(tile_rasters), " tuiles...")
    mosaic <- do.call(terra::merge, tile_rasters)
  }

  return(mosaic)
}

#' Verifier qu'un raster WMS contient des donnees reelles
#'
#' Certaines couches millésimees ne couvrent pas toutes les zones. Le WMS
#' retourne alors un raster valide mais vide (pixels a 0 ou NA).
#'
#' @param r SpatRaster a valider
#' @param min_pct Pourcentage minimum de pixels non-vides requis (defaut: 5)
#' @return TRUE si le raster contient suffisamment de donnees
#' @export
validate_wms_data <- function(r, min_pct = 5) {
  if (is.null(r)) return(FALSE)
  vals <- terra::values(r[[1]])
  n_valid <- sum(!is.na(vals) & vals > 0)
  pct <- n_valid / length(vals) * 100
  if (pct < min_pct) {
    message(sprintf("  Donnees insuffisantes : %.1f%% de pixels valides (%d/%d)",
                    pct, n_valid, length(vals)))
  }
  return(pct >= min_pct)
}

#' Telecharger les ortho RVB et IRC pour une AOI
#'
#' Telecharge les orthophotos RVB et IRC depuis la Geoplateforme IGN via
#' WMS-R. Gere le cache (reutilisation des fichiers existants), le millesime
#' (annee au choix) et le fallback automatique vers la mosaique la plus
#' recente si le millesime demande n'est pas disponible.
#'
#' @param aoi sf object (AOI en Lambert-93)
#' @param output_dir Repertoire de sortie
#' @param millesime_ortho `NULL` ou entier (annee de l'ortho RVB)
#' @param millesime_irc `NULL` ou entier (annee de l'ortho IRC)
#' @return Liste avec `rvb`, `irc` (SpatRaster), `rvb_path`, `irc_path`,
#'   `millesime_ortho`, `millesime_irc`
#' @export
download_ortho_for_aoi <- function(aoi, output_dir,
                                    millesime_ortho = NULL,
                                    millesime_irc = NULL) {
  fs::dir_create(output_dir)

  layer_ortho <- ign_layer_name("ortho", millesime_ortho)
  layer_irc   <- ign_layer_name("irc",   millesime_irc)
  label_ortho <- if (is.null(millesime_ortho)) "plus recent" else millesime_ortho
  label_irc   <- if (is.null(millesime_irc))   "plus recent" else millesime_irc

  rvb_path <- file.path(output_dir, "ortho_rvb.tif")
  irc_path <- file.path(output_dir, "ortho_irc.tif")

  # Cache
  if (file.exists(rvb_path) && file.exists(irc_path)) {
    message("\n=== Ortho IGN deja presentes (cache) ===")
    rvb <- terra::rast(rvb_path)
    irc <- terra::rast(irc_path)
    names(rvb)[1:min(3, terra::nlyr(rvb))] <- c("Rouge", "Vert", "Bleu")[1:min(3, terra::nlyr(rvb))]
    names(irc)[1:min(3, terra::nlyr(irc))] <- c("PIR", "Rouge", "Vert")[1:min(3, terra::nlyr(irc))]
    return(list(rvb = rvb, irc = irc,
                rvb_path = rvb_path, irc_path = irc_path,
                millesime_ortho = millesime_ortho,
                millesime_irc = millesime_irc))
  }

  bbox <- as.numeric(sf::st_bbox(sf::st_union(aoi)))

  message(sprintf("\n=== Telechargement ortho IGN pour l'AOI ==="))
  message(sprintf("Emprise: %.0f, %.0f - %.0f, %.0f (Lambert-93)",
                   bbox[1], bbox[2], bbox[3], bbox[4]))
  message(sprintf("Taille: %.0f x %.0f m (%.2f ha)",
                   bbox[3] - bbox[1], bbox[4] - bbox[2],
                   (bbox[3] - bbox[1]) * (bbox[4] - bbox[2]) / 10000))
  message(sprintf("Millesime RVB: %s (couche: %s)", label_ortho, layer_ortho))
  message(sprintf("Millesime IRC: %s (couche: %s)", label_irc, layer_irc))

  # --- RVB ---
  message("\n--- Ortho RVB ---")
  rvb <- tryCatch(
    download_ign_tiled(bbox, layer = layer_ortho, res_m = .ign_config$RES_IGN,
                       output_dir = output_dir, prefix = "rvb"),
    error = function(e) { message("  Erreur RVB: ", e$message); NULL }
  )

  if (!is.null(millesime_ortho) &&
      (is.null(rvb) || !validate_wms_data(rvb))) {
    message(sprintf("  Millesime %s indisponible, fallback mosaique courante",
                    millesime_ortho))
    layer_ortho <- .ign_config$LAYER_ORTHO
    tile_files <- fs::dir_ls(output_dir, glob = "rvb_tile_*.tif")
    if (length(tile_files) > 0) fs::file_delete(tile_files)
    rvb <- download_ign_tiled(bbox, layer = .ign_config$LAYER_ORTHO,
                               res_m = .ign_config$RES_IGN,
                               output_dir = output_dir, prefix = "rvb")
  }
  if (is.null(rvb)) stop("Impossible de telecharger l'ortho RVB")
  names(rvb)[1:min(3, terra::nlyr(rvb))] <- c("Rouge", "Vert", "Bleu")[1:min(3, terra::nlyr(rvb))]

  # --- IRC ---
  message("\n--- Ortho IRC ---")
  irc <- tryCatch(
    download_ign_tiled(bbox, layer = layer_irc, res_m = .ign_config$RES_IGN,
                       output_dir = output_dir, prefix = "irc"),
    error = function(e) { message("  Erreur IRC: ", e$message); NULL }
  )

  if (!is.null(millesime_irc) &&
      (is.null(irc) || !validate_wms_data(irc))) {
    message(sprintf("  Millesime %s indisponible, fallback mosaique courante",
                    millesime_irc))
    layer_irc <- .ign_config$LAYER_IRC
    tile_files <- fs::dir_ls(output_dir, glob = "irc_tile_*.tif")
    if (length(tile_files) > 0) fs::file_delete(tile_files)
    irc <- download_ign_tiled(bbox, layer = .ign_config$LAYER_IRC,
                               res_m = .ign_config$RES_IGN,
                               output_dir = output_dir, prefix = "irc")
  }
  if (is.null(irc)) stop("Impossible de telecharger l'ortho IRC")
  names(irc)[1:min(3, terra::nlyr(irc))] <- c("PIR", "Rouge", "Vert")[1:min(3, terra::nlyr(irc))]

  # Crop + save
  aoi_vect <- terra::vect(sf::st_union(aoi))
  rvb <- terra::crop(rvb, aoi_vect)
  irc <- terra::crop(irc, aoi_vect)

  terra::writeRaster(rvb, rvb_path, overwrite = TRUE)
  terra::writeRaster(irc, irc_path, overwrite = TRUE)

  rvb <- terra::rast(rvb_path)
  irc <- terra::rast(irc_path)
  names(rvb)[1:min(3, terra::nlyr(rvb))] <- c("Rouge", "Vert", "Bleu")[1:min(3, terra::nlyr(rvb))]
  names(irc)[1:min(3, terra::nlyr(irc))] <- c("PIR", "Rouge", "Vert")[1:min(3, terra::nlyr(irc))]

  message(sprintf("\nRVB: %s (%d x %d px)", rvb_path, terra::ncol(rvb), terra::nrow(rvb)))
  message(sprintf("IRC: %s (%d x %d px)", irc_path, terra::ncol(irc), terra::nrow(irc)))

  # Cleanup tiles
  tile_files <- fs::dir_ls(output_dir, glob = "*_tile_*.tif")
  if (length(tile_files) > 0) fs::file_delete(tile_files)

  list(rvb = rvb, irc = irc,
       rvb_path = rvb_path, irc_path = irc_path,
       millesime_ortho = millesime_ortho,
       millesime_irc = millesime_irc)
}

#' Telecharger le MNT (RGE ALTI 1m) pour une AOI
#'
#' Telecharge le Modele Numerique de Terrain depuis la Geoplateforme IGN
#' via WMS-R. Le MNT est optionnellement reechantillonne a 0.2m pour
#' s'aligner sur la grille des orthophotos.
#'
#' @param aoi sf object en Lambert-93
#' @param output_dir Repertoire de sortie
#' @param rgbi SpatRaster de reference pour le reechantillonnage a 0.2m
#'   (NULL = garder la resolution native 1m)
#' @return Liste avec `mnt` (SpatRaster) et `mnt_path`, ou NULL si echec
#' @export
download_mnt_for_aoi <- function(aoi, output_dir, rgbi = NULL) {
  fs::dir_create(output_dir)

  mnt_path <- file.path(output_dir, "mnt_1m.tif")

  # Cache
  if (file.exists(mnt_path)) {
    message("\n=== MNT deja telecharge (cache) ===")
    mnt <- terra::rast(mnt_path)
    names(mnt) <- "MNT"
    return(list(mnt = mnt, mnt_path = mnt_path))
  }

  bbox <- as.numeric(sf::st_bbox(sf::st_union(aoi)))
  message(sprintf("\n=== Telechargement MNT IGN (RGE ALTI 1m via WMS-R) ==="))

  mnt <- tryCatch(
    download_ign_tiled(bbox, layer = .ign_config$LAYER_MNT, res_m = 1,
                        output_dir = output_dir, prefix = "mnt",
                        styles = "normal"),
    error = function(e) { message("  MNT non telecharge: ", e$message); NULL }
  )

  if (is.null(mnt)) {
    warning("Aucune donnee MNT telechargee.")
    return(NULL)
  }

  aoi_vect <- terra::vect(sf::st_union(aoi))
  mnt <- terra::crop(mnt, aoi_vect)
  names(mnt) <- "MNT"

  if (!is.null(rgbi)) {
    message("Reechantillonnage MNT de 1m vers 0.2m...")
    mnt <- terra::resample(mnt, rgbi, method = "bilinear")
  }

  terra::writeRaster(mnt, mnt_path, overwrite = TRUE, gdal = c("COMPRESS=LZW"))

  mnt_vals <- terra::values(mnt, na.rm = TRUE)
  if (length(mnt_vals) > 0 && any(is.finite(mnt_vals))) {
    message(sprintf("  Altitude MNT: %.0f - %.0f m",
                     min(mnt_vals, na.rm = TRUE), max(mnt_vals, na.rm = TRUE)))
  }

  message(sprintf("MNT: %s (%d x %d px)", mnt_path, terra::ncol(mnt), terra::nrow(mnt)))

  tile_files <- fs::dir_ls(output_dir, glob = "mnt_tile_*.tif")
  if (length(tile_files) > 0) fs::file_delete(tile_files)

  mnt <- terra::rast(mnt_path)
  names(mnt) <- "MNT"
  list(mnt = mnt, mnt_path = mnt_path)
}

#' Telecharger le DEM 2 bandes pour une AOI avec derives terrain
#'
#' Telecharge le DTM (RGE ALTI 1m) et le DSM (LiDAR HD) depuis la
#' Geoplateforme IGN via WMS-R, puis calcule les derives morphologiques
#' (pente, orientation, TPI, TWI) a la resolution native de 1m.
#'
#' Le DEM reste a 1m de resolution (50x50 pixels par patch de 50m).
#' L'utilisateur choisit 2 canaux parmi DSM, DTM, SLOPE, ASPECT, TPI, TWI.
#'
#' @param aoi sf object en Lambert-93
#' @param output_dir Repertoire de sortie
#' @param dem_channels Vecteur de 2 noms de canaux a utiliser parmi
#'   `c("DSM", "DTM", "SLOPE", "ASPECT", "TPI", "TWI")`.
#'   Defaut: `c("SLOPE", "TWI")` (les plus discriminants pour la foret).
#' @return Liste avec `dem` (SpatRaster 2 bandes a 1m), `dem_path`,
#'   `dsm_source`, `dem_channels`, ou NULL si echec
#' @export
download_dem_for_aoi <- function(aoi, output_dir, dem_channels = c("SLOPE", "TWI")) {
  fs::dir_create(output_dir)

  dem_channels <- toupper(dem_channels)
  valid_channels <- c("DSM", "DTM", "SLOPE", "ASPECT", "TPI", "TWI")
  unknown <- setdiff(dem_channels, valid_channels)
  if (length(unknown) > 0) {
    stop(sprintf("Canal(aux) DEM inconnu(s): %s. Valides: %s",
                 paste(unknown, collapse = ", "),
                 paste(valid_channels, collapse = ", ")))
  }
  if (length(dem_channels) < 1 || length(dem_channels) > 6) {
    stop("dem_channels doit contenir entre 1 et 6 noms de canaux")
  }

  # Suffixe de cache base sur les canaux choisis
  channels_tag <- tolower(paste(dem_channels, collapse = "_"))
  dem_path <- file.path(output_dir, sprintf("dem_%s_1m.tif", channels_tag))

  # Cache
  if (file.exists(dem_path)) {
    message(sprintf("\n=== DEM deja calcule (cache): %s ===",
                     paste(dem_channels, collapse = " + ")))
    dem <- terra::rast(dem_path)
    names(dem) <- dem_channels
    return(list(dem = dem, dem_path = dem_path, dsm_source = "cache",
                dem_channels = dem_channels))
  }

  bbox <- as.numeric(sf::st_bbox(sf::st_union(aoi)))
  aoi_vect <- terra::vect(sf::st_union(aoi))

  # --- DTM (MNT) : couverture nationale RGE ALTI 1m ---
  message(sprintf("\n=== Telechargement DEM MAESTRO (%s) a 1m ===",
                   paste(dem_channels, collapse = " + ")))
  message("--- DTM (MNT) : RGE ALTI 1m ---")

  dtm <- tryCatch(
    download_ign_tiled(bbox, layer = .ign_config$LAYER_MNT,
                        res_m = .ign_config$RES_DEM,
                        output_dir = output_dir, prefix = "dtm",
                        styles = "normal"),
    error = function(e) { message("  DTM non telecharge: ", e$message); NULL }
  )

  if (is.null(dtm)) {
    warning("Aucune donnee DTM telechargee.")
    return(NULL)
  }

  dtm <- terra::crop(dtm, aoi_vect)
  names(dtm) <- "DTM"

  # --- DSM (MNS) : LiDAR HD (couverture partielle) ---
  # Telecharge seulement si un canal DSM est demande
  dsm_source <- "dtm_copie"
  needs_dsm <- "DSM" %in% dem_channels

  if (needs_dsm) {
    message("--- DSM (MNS) : LiDAR HD ---")
    dsm <- tryCatch({
      r <- download_ign_tiled(bbox, layer = .ign_config$LAYER_MNS,
                               res_m = .ign_config$RES_DEM,
                               output_dir = output_dir, prefix = "dsm",
                               styles = "normal")
      r <- terra::crop(r, aoi_vect)
      if (validate_wms_data(r, min_pct = 10)) {
        dsm_source <- "lidar_hd"
        r
      } else {
        message("  DSM LiDAR HD: couverture insuffisante, utilisation du DTM")
        NULL
      }
    }, error = function(e) {
      message("  DSM LiDAR HD non disponible: ", e$message)
      message("  Fallback: duplication du DTM comme DSM")
      NULL
    })

    if (is.null(dsm)) {
      dsm <- dtm
      dsm_source <- "dtm_copie"
      message("  DSM = copie du DTM (pas de LiDAR HD disponible)")
    }
    names(dsm) <- "DSM"
  } else {
    dsm <- dtm
    names(dsm) <- "DSM"
    message(sprintf("  DSM non requis (canaux: %s)", paste(dem_channels, collapse = ", ")))
  }

  # --- Calcul des derives morphologiques a 1m ---
  needs_derives <- any(dem_channels %in% c("SLOPE", "ASPECT", "TPI", "TWI"))
  if (needs_derives) {
    derives <- calculer_derives_terrain(dtm)
  } else {
    derives <- list()
  }

  # --- Assembler les 2 canaux choisis ---
  dem <- assembler_dem_channels(dsm, dtm, derives, dem_channels)

  # Sauvegarder a 1m (resolution native)
  terra::writeRaster(dem, dem_path, overwrite = TRUE, gdal = c("COMPRESS=LZW"))

  dtm_vals <- terra::values(dtm, na.rm = TRUE)
  if (length(dtm_vals) > 0 && any(is.finite(dtm_vals))) {
    message(sprintf("  Altitude DTM: %.0f - %.0f m",
                     min(dtm_vals, na.rm = TRUE), max(dtm_vals, na.rm = TRUE)))
  }
  message(sprintf("  Source DSM: %s", dsm_source))
  message(sprintf("  Canaux DEM: %s", paste(dem_channels, collapse = " + ")))
  message(sprintf("  Resolution: 1m (50x50 px par patch de 50m)"))
  message(sprintf("DEM: %s (%d x %d px, %d bandes)",
                   dem_path, terra::ncol(dem), terra::nrow(dem), terra::nlyr(dem)))

  # Cleanup
  tile_files <- fs::dir_ls(output_dir, glob = "d[ts]m_tile_*.tif")
  if (length(tile_files) > 0) fs::file_delete(tile_files)

  dem <- terra::rast(dem_path)
  names(dem) <- dem_channels
  list(dem = dem, dem_path = dem_path, dsm_source = dsm_source,
       dem_channels = dem_channels)
}
