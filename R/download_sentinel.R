# =============================================================================
# Telechargement de donnees Sentinel-1 et Sentinel-2 via STAC API
# Backend : Microsoft Planetary Computer (acces libre, COG natif)
# Utilise rstac pour la recherche et le signing automatique
# Adapte de tree_sat_nemeton (08_download_satellite.R)
# =============================================================================

# --- Configuration STAC Planetary Computer ---

#' @noRd
.stac_config <- function() {
  list(
    stac_url      = "https://planetarycomputer.microsoft.com/api/stac/v1",
    s2_collection = "sentinel-2-l2a",
    s1_collection = "sentinel-1-rtc",
    s2_bands      = c("B02", "B03", "B04", "B05", "B06", "B07",
                       "B08", "B8A", "B11", "B12"),
    s1_assets     = c("vv", "vh"),
    max_results   = 500L
  )
}

# --- Configuration Sentinel ---

#' Configuration des bandes Sentinel-2
#'
#' Les 10 bandes spectrales utilisees par MAESTRO pour la modalite S2.
#'
#' @return Liste nommee avec nom, longueur d'onde et resolution par bande
#' @export
s2_bands_config <- function() {
  list(
    B02 = list(name = "Blue",     wavelength = 490,  resolution = 10),
    B03 = list(name = "Green",    wavelength = 560,  resolution = 10),
    B04 = list(name = "Red",      wavelength = 665,  resolution = 10),
    B05 = list(name = "RedEdge1", wavelength = 705,  resolution = 20),
    B06 = list(name = "RedEdge2", wavelength = 740,  resolution = 20),
    B07 = list(name = "RedEdge3", wavelength = 783,  resolution = 20),
    B08 = list(name = "NIR",      wavelength = 842,  resolution = 10),
    B8A = list(name = "NIR2",     wavelength = 865,  resolution = 20),
    B11 = list(name = "SWIR1",    wavelength = 1610, resolution = 20),
    B12 = list(name = "SWIR2",    wavelength = 2190, resolution = 20)
  )
}

# --- Lecture COG distante ---

#' Lire un raster COG distant avec fallback telechargement complet
#'
#' Essaie d'abord /vsicurl/ (lecture partielle), puis telecharge le fichier
#' complet si la lecture partielle echoue.
#'
#' @param url URL du fichier COG
#' @param aoi_vect SpatVector de l'AOI (en WGS84)
#' @param buffer_m Buffer en metres autour de l'AOI pour le crop
#' @return SpatRaster croppe sur l'AOI, ou NULL si echec
#' @noRd
.read_remote_cog <- function(url, aoi_vect, buffer_m = 1000) {
  # Essai 1 : lecture partielle via /vsicurl/
  r <- tryCatch({
    r <- terra::rast(paste0("/vsicurl/", url))
    aoi_native <- terra::project(aoi_vect, terra::crs(r))
    crop_ext <- terra::ext(terra::buffer(aoi_native, buffer_m))
    terra::crop(r, crop_ext)
  }, error = function(e) NULL)

  if (!is.null(r)) return(r)

  # Essai 2 : telechargement complet puis lecture locale
  r <- tryCatch({
    tmp <- tempfile(fileext = ".tif")
    utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
    r <- terra::rast(tmp)
    aoi_native <- terra::project(aoi_vect, terra::crs(r))
    crop_ext <- terra::ext(terra::buffer(aoi_native, buffer_m))
    r <- terra::crop(r, crop_ext)
    unlink(tmp)
    r
  }, error = function(e) NULL)

  r
}

# --- Recherche STAC via rstac ---

#' Rechercher des scenes Sentinel-2 via STAC (Planetary Computer)
#'
#' Interroge le catalogue STAC de Microsoft Planetary Computer pour
#' trouver des scenes Sentinel-2 L2A. Les URLs sont automatiquement
#' signees pour l'acces aux donnees COG.
#'
#' @param bbox Vecteur numerique c(xmin, ymin, xmax, ymax) en WGS84
#' @param start_date Date de debut (format "YYYY-MM-DD")
#' @param end_date Date de fin (format "YYYY-MM-DD")
#' @param max_cloud Couverture nuageuse maximale en % (defaut: 30)
#' @return Liste avec `scenes` (data.frame) et `items` (items STAC signes),
#'   ou NULL si aucune scene trouvee
#' @export
search_s2_stac <- function(bbox, start_date, end_date, max_cloud = 30) {
  message("=== Recherche STAC Sentinel-2 L2A (Planetary Computer) ===")
  conf <- .stac_config()

  datetime_str <- paste0(start_date, "T00:00:00Z/", end_date, "T23:59:59Z")

  items <- tryCatch({
    rstac::stac(conf$stac_url) |>
      rstac::stac_search(
        collections = conf$s2_collection,
        bbox        = bbox,
        datetime    = datetime_str,
        limit       = conf$max_results
      ) |>
      rstac::get_request()
  }, error = function(e) {
    message(sprintf("  [STAC] Erreur: %s", e$message))
    return(NULL)
  })

  if (is.null(items) || length(items$features) == 0) {
    message("  Aucune scene S2 trouvee")
    return(NULL)
  }

  # Filtrage client-side de la couverture nuageuse
  items$features <- Filter(function(feat) {
    cc <- feat$properties$`eo:cloud_cover`
    !is.null(cc) && cc <= max_cloud
  }, items$features)

  if (length(items$features) == 0) {
    message(sprintf("  Aucune scene S2 avec nuages <= %d%%", max_cloud))
    return(NULL)
  }

  # Signer les URLs automatiquement via Planetary Computer
  items_signed <- rstac::items_sign(items,
                                     sign_fn = rstac::sign_planetary_computer())

  # Construire le data.frame des scenes
  scenes <- lapply(items_signed$features, function(feat) {
    data.frame(
      id          = feat$id %||% NA_character_,
      datetime    = feat$properties$datetime %||% NA_character_,
      date        = as.Date(substr(feat$properties$datetime %||% "", 1, 10)),
      cloud_cover = feat$properties$`eo:cloud_cover` %||% NA_real_,
      source      = "planetary",
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, scenes)
  result <- result[order(result$cloud_cover), ]

  message(sprintf("  %d scenes Sentinel-2 L2A trouvees", nrow(result)))

  list(scenes = result, items = items_signed)
}

#' Rechercher des scenes Sentinel-1 via STAC (Planetary Computer)
#'
#' Recherche des produits Sentinel-1 RTC (Radiometric Terrain Corrected)
#' sur Microsoft Planetary Computer. Les produits RTC sont deja corriges
#' du terrain, avec les polarisations VV et VH disponibles en COG.
#'
#' @param bbox Vecteur numerique c(xmin, ymin, xmax, ymax) en WGS84
#' @param start_date Date de debut
#' @param end_date Date de fin
#' @param orbit_direction Direction d'orbite: "ascending", "descending" ou "both"
#' @return Liste avec `scenes` (data.frame) et `items` (items STAC signes),
#'   ou NULL si aucun produit trouve
#' @export
search_s1_stac <- function(bbox, start_date, end_date,
                            orbit_direction = "both") {
  message("=== Recherche STAC Sentinel-1 RTC (Planetary Computer) ===")
  conf <- .stac_config()

  datetime_str <- paste0(start_date, "T00:00:00Z/", end_date, "T23:59:59Z")

  items <- tryCatch({
    rstac::stac(conf$stac_url) |>
      rstac::stac_search(
        collections = conf$s1_collection,
        bbox        = bbox,
        datetime    = datetime_str,
        limit       = conf$max_results
      ) |>
      rstac::get_request()
  }, error = function(e) {
    message(sprintf("  [STAC] Erreur: %s", e$message))
    return(NULL)
  })

  if (is.null(items) || length(items$features) == 0) {
    message("  Aucun produit S1 trouve")
    return(NULL)
  }

  # Filtrage par direction d'orbite si specifie
  if (orbit_direction != "both") {
    target <- tolower(orbit_direction)
    items$features <- Filter(function(feat) {
      orb <- feat$properties$`sat:orbit_state`
      !is.null(orb) && tolower(orb) == target
    }, items$features)

    if (length(items$features) == 0) {
      message(sprintf("  Aucun produit S1 en orbite %s", orbit_direction))
      return(NULL)
    }
  }

  # Signer les URLs
  items_signed <- rstac::items_sign(items,
                                     sign_fn = rstac::sign_planetary_computer())

  # Construire le data.frame des scenes
  scenes <- lapply(items_signed$features, function(feat) {
    orb <- feat$properties$`sat:orbit_state` %||% NA_character_
    data.frame(
      id              = feat$id %||% NA_character_,
      datetime        = feat$properties$datetime %||% NA_character_,
      date            = as.Date(substr(feat$properties$datetime %||% "", 1, 10)),
      orbit_direction = orb,
      source          = "planetary",
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, scenes)
  result <- result[order(result$date), ]

  message(sprintf("  %d produits Sentinel-1 RTC trouves", nrow(result)))

  list(scenes = result, items = items_signed)
}

# --- Telechargement S2 ---

#' Telecharger une image Sentinel-2 pour une AOI
#'
#' Telecharge les 10 bandes spectrales Sentinel-2 L2A depuis
#' Microsoft Planetary Computer (COG en acces libre). Utilise rstac
#' pour la recherche et le signing automatique des URLs.
#'
#' @param aoi sf object en Lambert-93
#' @param output_dir Repertoire de sortie
#' @param date_cible Date cible (format "YYYY-MM-DD", optionnel)
#' @return SpatRaster avec les 10 bandes S2, ou NULL
#' @export
download_s2_for_aoi <- function(aoi, output_dir, date_cible = NULL) {
  message("=== Telechargement Sentinel-2 pour l'AOI ===")

  s2_path <- file.path(output_dir, "sentinel2.tif")
  if (file.exists(s2_path)) {
    message("  Cache: ", s2_path)
    return(terra::rast(s2_path))
  }

  aoi_wgs84 <- sf::st_transform(aoi, 4326)
  bbox_wgs84 <- as.numeric(sf::st_bbox(aoi_wgs84))

  # Determiner la periode de recherche
  if (is.null(date_cible)) {
    annee <- format(Sys.Date(), "%Y")
    start_date <- paste0(annee, "-06-01")
    end_date <- paste0(annee, "-09-30")
  } else {
    d <- as.Date(date_cible)
    start_date <- as.character(d - 30)
    end_date <- as.character(d + 30)
  }

  search_result <- search_s2_stac(bbox_wgs84, start_date, end_date,
                                    max_cloud = 20)

  if (is.null(search_result)) {
    message("  Aucune scene S2, essai sur l'annee entiere...")
    annee <- if (!is.null(date_cible)) format(as.Date(date_cible), "%Y") else
      format(Sys.Date(), "%Y")
    search_result <- search_s2_stac(bbox_wgs84,
                                      paste0(annee, "-01-01"),
                                      paste0(annee, "-12-31"),
                                      max_cloud = 30)
  }

  if (is.null(search_result)) {
    warning("Aucune scene Sentinel-2 trouvee pour cette AOI")
    return(NULL)
  }

  scenes <- search_result$scenes
  items_signed <- search_result$items
  best <- scenes[1, ]
  message(sprintf("  Scene selectionnee: %s (nuages: %.0f%%)",
                   best$id, best$cloud_cover))

  # Trouver le feature signe correspondant
  sel_feature <- NULL
  for (feat in items_signed$features) {
    if (feat$id == best$id) {
      sel_feature <- feat
      break
    }
  }

  if (is.null(sel_feature)) {
    warning("Feature signe non trouve pour la scene selectionnee")
    return(NULL)
  }

  # Telecharger chaque bande via COG
  conf <- .stac_config()
  aoi_vect <- terra::vect(aoi_wgs84)
  layers <- list()

  for (band in conf$s2_bands) {
    url <- sel_feature$assets[[band]]$href
    if (is.null(url)) {
      message(sprintf("  [WARN] Asset %s non disponible", band))
      next
    }

    message(sprintf("  Telechargement bande %s...", band))
    r <- .read_remote_cog(url, aoi_vect)

    if (!is.null(r)) {
      names(r) <- band
      layers[[band]] <- r
      message(sprintf("  %s OK", band))
    } else {
      message(sprintf("  [WARN] Echec telechargement bande %s", band))
    }
  }

  if (length(layers) == 0) {
    warning("Aucune bande S2 telechargee")
    return(NULL)
  }

  message(sprintf("  %d/%d bandes S2 telechargees", length(layers),
                   length(conf$s2_bands)))

  if (length(layers) < length(conf$s2_bands)) {
    missing <- setdiff(conf$s2_bands, names(layers))
    warning(sprintf("Bandes manquantes: %s", paste(missing, collapse = ", ")))
  }

  # Reprojeter en Lambert-93 et reechantillonner a 10m
  bbox_l93 <- sf::st_bbox(aoi)
  ext_aoi <- terra::ext(bbox_l93["xmin"], bbox_l93["xmax"],
                         bbox_l93["ymin"], bbox_l93["ymax"])

  # Template 10m en Lambert-93
  template <- terra::rast(ext = ext_aoi, res = 10, crs = "EPSG:2154")

  for (i in seq_along(layers)) {
    band_name <- names(layers[[i]])
    layers[[i]] <- tryCatch({
      r <- terra::project(layers[[i]], "EPSG:2154")
      r <- terra::resample(r, template, method = "bilinear")
      names(r) <- band_name
      r
    }, error = function(e) {
      message(sprintf("  [WARN] Echec reprojection %s: %s", band_name,
                       e$message))
      NULL
    })
  }
  layers <- Filter(Negate(is.null), layers)

  if (length(layers) == 0) {
    warning("Aucune bande S2 valide apres reprojection")
    return(NULL)
  }

  # Combiner les bandes dans l'ordre
  s2_raster <- tryCatch(
    do.call(c, layers),
    error = function(e) {
      message(sprintf("  [WARN] Echec combinaison bandes: %s", e$message))
      NULL
    }
  )

  if (is.null(s2_raster)) return(NULL)

  # Sauvegarder
  tryCatch({
    terra::writeRaster(s2_raster, s2_path, overwrite = TRUE,
                       gdal = c("COMPRESS=LZW"))
    message(sprintf("  Sentinel-2 sauvegarde: %s (%d bandes)",
                     s2_path, terra::nlyr(s2_raster)))
  }, error = function(e) {
    message(sprintf("  [WARN] Echec sauvegarde S2: %s", e$message))
  })

  s2_raster
}

# --- Telechargement S1 ---

#' Telecharger les donnees Sentinel-1 pour une AOI
#'
#' Telecharge les polarisations VV et VH depuis Planetary Computer
#' (collection sentinel-1-rtc, deja corrige du terrain).
#' Les valeurs lineaires (gamma0) sont converties en dB : 10 * log10(val).
#'
#' @param aoi sf object en Lambert-93
#' @param output_dir Repertoire de sortie
#' @param date_cible Date cible (optionnel)
#' @return Liste avec `s1_asc` et `s1_des` (SpatRaster 2 bandes VV+VH chacun),
#'   ou NULL si non disponible
#' @export
download_s1_for_aoi <- function(aoi, output_dir, date_cible = NULL) {
  message("=== Telechargement Sentinel-1 pour l'AOI ===")

  s1_asc_path <- file.path(output_dir, "sentinel1_asc.tif")
  s1_des_path <- file.path(output_dir, "sentinel1_des.tif")

  if (file.exists(s1_asc_path) && file.exists(s1_des_path)) {
    message("  Cache: S1 ascending + descending")
    return(list(
      s1_asc = terra::rast(s1_asc_path),
      s1_des = terra::rast(s1_des_path)
    ))
  }

  aoi_wgs84 <- sf::st_transform(aoi, 4326)
  bbox_wgs84 <- as.numeric(sf::st_bbox(aoi_wgs84))

  if (is.null(date_cible)) {
    annee <- format(Sys.Date(), "%Y")
    start_date <- paste0(annee, "-06-01")
    end_date <- paste0(annee, "-09-30")
  } else {
    d <- as.Date(date_cible)
    start_date <- as.character(d - 30)
    end_date <- as.character(d + 30)
  }

  result <- list(s1_asc = NULL, s1_des = NULL)

  # --- Ascending ---
  if (!file.exists(s1_asc_path)) {
    search_asc <- search_s1_stac(bbox_wgs84, start_date, end_date,
                                   orbit_direction = "ascending")
    if (!is.null(search_asc)) {
      result$s1_asc <- .download_s1_orbit(search_asc, aoi, "ascending")
      if (!is.null(result$s1_asc)) {
        terra::writeRaster(result$s1_asc, s1_asc_path, overwrite = TRUE,
                           gdal = c("COMPRESS=LZW"))
        message(sprintf("  S1 ascending sauvegarde: %s", s1_asc_path))
      }
    }
  } else {
    result$s1_asc <- terra::rast(s1_asc_path)
    message("  Cache: S1 ascending")
  }

  # --- Descending ---
  if (!file.exists(s1_des_path)) {
    search_des <- search_s1_stac(bbox_wgs84, start_date, end_date,
                                   orbit_direction = "descending")
    if (!is.null(search_des)) {
      result$s1_des <- .download_s1_orbit(search_des, aoi, "descending")
      if (!is.null(result$s1_des)) {
        terra::writeRaster(result$s1_des, s1_des_path, overwrite = TRUE,
                           gdal = c("COMPRESS=LZW"))
        message(sprintf("  S1 descending sauvegarde: %s", s1_des_path))
      }
    }
  } else {
    result$s1_des <- terra::rast(s1_des_path)
    message("  Cache: S1 descending")
  }

  has_data <- !is.null(result$s1_asc) || !is.null(result$s1_des)
  if (!has_data) {
    warning("Aucune donnee Sentinel-1 disponible")
    return(NULL)
  }

  result
}

#' Telecharger les bandes S1 pour une orbite donnee
#' @noRd
.download_s1_orbit <- function(search_result, aoi, orbit) {
  scenes <- search_result$scenes
  items_signed <- search_result$items
  best <- scenes[1, ]
  message(sprintf("  Scene S1 %s: %s", orbit, best$id))

  # Trouver le feature signe
  sel_feature <- NULL
  for (feat in items_signed$features) {
    if (feat$id == best$id) {
      sel_feature <- feat
      break
    }
  }

  if (is.null(sel_feature)) {
    message(sprintf("  [WARN] Feature signe non trouve pour S1 %s", orbit))
    return(NULL)
  }

  conf <- .stac_config()
  aoi_wgs84 <- sf::st_transform(aoi, 4326)
  aoi_vect <- terra::vect(aoi_wgs84)
  layers <- list()

  for (asset_name in conf$s1_assets) {
    pol <- toupper(asset_name)
    url <- sel_feature$assets[[asset_name]]$href

    if (is.null(url)) {
      message(sprintf("  [WARN] Asset S1 '%s' non disponible", asset_name))
      next
    }

    message(sprintf("  Telechargement %s %s...", orbit, pol))
    r <- .read_remote_cog(url, aoi_vect)

    if (!is.null(r)) {
      # Conversion gamma0 lineaire -> dB
      vals <- terra::values(r)
      vals[vals <= 0] <- NA
      vals <- 10 * log10(vals)
      terra::values(r) <- vals
      names(r) <- pol
      layers[[asset_name]] <- r
      message(sprintf("  %s %s OK", orbit, pol))
    } else {
      message(sprintf("  [WARN] Echec telechargement S1 %s %s", orbit, pol))
    }
  }

  if (length(layers) == 0) {
    message(sprintf("  Aucune bande S1 %s telechargee", orbit))
    return(NULL)
  }

  # Reprojeter en Lambert-93 et reechantillonner a 10m
  bbox_l93 <- sf::st_bbox(aoi)
  ext_aoi <- terra::ext(bbox_l93["xmin"], bbox_l93["xmax"],
                         bbox_l93["ymin"], bbox_l93["ymax"])
  template <- terra::rast(ext = ext_aoi, res = 10, crs = "EPSG:2154")

  for (i in seq_along(layers)) {
    pol_name <- names(layers[[i]])
    layers[[i]] <- tryCatch({
      r <- terra::project(layers[[i]], "EPSG:2154")
      r <- terra::resample(r, template, method = "bilinear")
      names(r) <- pol_name
      r
    }, error = function(e) {
      message(sprintf("  [WARN] Echec reprojection S1 %s: %s", pol_name,
                       e$message))
      NULL
    })
  }
  layers <- Filter(Negate(is.null), layers)

  if (length(layers) == 0) return(NULL)

  s1 <- tryCatch(
    do.call(c, layers),
    error = function(e) {
      message(sprintf("  [WARN] Echec combinaison S1 %s: %s", orbit,
                       e$message))
      NULL
    }
  )

  if (!is.null(s1)) {
    message(sprintf("  S1 %s: %d bandes (VV, VH)", orbit, terra::nlyr(s1)))
  }

  s1
}

# --- Alignement ---

#' Aligner un raster Sentinel sur la grille de l'AOI
#'
#' Reechantillonne un raster Sentinel (10m) pour qu'il couvre exactement
#' la meme emprise que le raster de reference, tout en gardant sa
#' resolution native (10m).
#'
#' @param sentinel SpatRaster Sentinel (S1 ou S2)
#' @param reference SpatRaster de reference (ex: RGBI a 0.2m)
#' @param target_res Resolution cible en metres (defaut: 10)
#' @return SpatRaster aligne sur l'emprise de reference
#' @export
aligner_sentinel <- function(sentinel, reference, target_res = 10) {
  ext_ref <- terra::ext(reference)

  template <- terra::rast(
    xmin = ext_ref[1], xmax = ext_ref[2],
    ymin = ext_ref[3], ymax = ext_ref[4],
    res = target_res,
    crs = terra::crs(reference)
  )

  # S'assurer que le CRS correspond
  if (terra::crs(sentinel) != terra::crs(reference)) {
    sentinel <- terra::project(sentinel, terra::crs(reference))
  }

  result <- terra::resample(sentinel, template, method = "bilinear")
  names(result) <- names(sentinel)
  result
}
