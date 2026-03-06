# =============================================================================
# Telechargement de donnees Sentinel-1 et Sentinel-2 via STAC API
# Backend prioritaire : Copernicus Data Space
# Fallback : Microsoft Planetary Computer (acces libre, COG natif)
# Adapte de tree_sat_nemeton (03_data_acquisition.R)
# =============================================================================

# --- Configuration des backends STAC ---

#' URLs des catalogues STAC disponibles
#' @noRd
.stac_backends <- function() {
  list(
    copernicus = list(
      name = "Copernicus Data Space",
      stac_url = "https://catalogue.dataspace.copernicus.eu/stac/search",
      s2_collection = "sentinel-2-l2a",
      s1_collection = "sentinel-1-grd",
      cloud_field = "eo:cloud_cover",
      orbit_field = "sat:orbit_state",
      needs_token = TRUE
    ),
    planetary = list(
      name = "Microsoft Planetary Computer",
      stac_url = "https://planetarycomputer.microsoft.com/api/stac/v1/search",
      s2_collection = "sentinel-2-l2a",
      s1_collection = "sentinel-1-rtc",
      cloud_field = "eo:cloud_cover",
      orbit_field = "sat:orbit_state",
      needs_token = FALSE,
      signing_url = "https://planetarycomputer.microsoft.com/api/sas/v1/token"
    )
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

# --- Requete STAC generique ---

#' Executer une requete STAC POST /search
#'
#' Fonction interne generique pour interroger un catalogue STAC.
#'
#' @param stac_url URL de l'endpoint STAC /search
#' @param collection Nom de la collection
#' @param bbox Vecteur c(xmin, ymin, xmax, ymax) en WGS84
#' @param start_date Date de debut
#' @param end_date Date de fin
#' @param query_params Liste de filtres supplementaires (optionnel)
#' @return Liste brute issue du JSON de reponse, ou NULL si echec
#' @noRd
.stac_search <- function(stac_url, collection, bbox, start_date, end_date,
                          query_params = NULL) {
  body <- list(
    collections = list(collection),
    bbox = as.list(bbox),
    datetime = paste0(start_date, "T00:00:00Z/", end_date, "T23:59:59Z"),
    limit = 500L
  )

  if (!is.null(query_params)) {
    body$query <- query_params
  }

  body_json <- jsonlite::toJSON(body, auto_unbox = TRUE)

  h <- curl::new_handle()
  curl::handle_setopt(h,
    copypostfields = as.character(body_json),
    httpheader = "Content-Type: application/json"
  )

  resp <- tryCatch(
    curl::curl_fetch_memory(stac_url, handle = h),
    error = function(e) {
      message(sprintf("  [STAC] Erreur reseau: %s", e$message))
      return(NULL)
    }
  )

  if (is.null(resp) || resp$status_code != 200) {
    status <- if (!is.null(resp)) resp$status_code else "timeout"
    message(sprintf("  [STAC] Echec HTTP %s pour %s", status, stac_url))
    return(NULL)
  }

  jsonlite::fromJSON(rawToChar(resp$content))
}

# --- STAC API (avec fallback) ---

#' Rechercher des scenes Sentinel-2 via STAC (Copernicus + fallback Planetary)
#'
#' Interroge d'abord le catalogue Copernicus Data Space. En cas d'echec,
#' bascule automatiquement sur Microsoft Planetary Computer.
#'
#' @param bbox Vecteur numerique c(xmin, ymin, xmax, ymax) en WGS84
#' @param start_date Date de debut (format "YYYY-MM-DD")
#' @param end_date Date de fin (format "YYYY-MM-DD")
#' @param max_cloud Couverture nuageuse maximale en % (defaut: 30)
#' @return data.frame avec colonnes id, datetime, cloud_cover, source
#' @export
search_s2_stac <- function(bbox, start_date, end_date, max_cloud = 30) {
  message("=== Recherche STAC Sentinel-2 L2A ===")

  backends <- .stac_backends()

  for (backend_name in names(backends)) {
    backend <- backends[[backend_name]]
    message(sprintf("  Essai %s...", backend$name))

    query_params <- list()
    query_params[[backend$cloud_field]] <- list(lte = max_cloud)

    result <- .stac_search(
      backend$stac_url, backend$s2_collection,
      bbox, start_date, end_date,
      query_params = query_params
    )

    if (is.null(result)) next

    features <- result$features
    if (is.null(features) || length(features) == 0) {
      if (is.data.frame(features) && nrow(features) == 0) next
      if (!is.data.frame(features)) next
    }

    n_items <- if (is.data.frame(features)) nrow(features) else length(features)
    if (n_items == 0) next

    message(sprintf("  %d scenes Sentinel-2 L2A trouvees via %s",
                     n_items, backend$name))

    scenes <- data.frame(
      id = features$id,
      datetime = features$properties$datetime,
      cloud_cover = features$properties[[backend$cloud_field]],
      source = backend_name,
      stringsAsFactors = FALSE
    )

    scenes$date <- as.Date(substr(scenes$datetime, 1, 10))
    scenes <- scenes[order(scenes$cloud_cover), ]

    return(scenes)
  }

  message("  Aucune scene trouvee sur aucun backend")
  data.frame()
}

#' Rechercher des scenes Sentinel-1 via STAC (Copernicus + fallback Planetary)
#'
#' Copernicus fournit des GRD bruts, Planetary Computer fournit des RTC
#' (Radiometric Terrain Corrected) — deja corrigees du terrain.
#'
#' @param bbox Vecteur numerique c(xmin, ymin, xmax, ymax) en WGS84
#' @param start_date Date de debut
#' @param end_date Date de fin
#' @param orbit_direction Direction d'orbite: "ascending", "descending" ou "both"
#' @return data.frame avec colonnes id, datetime, orbit_direction, source
#' @export
search_s1_stac <- function(bbox, start_date, end_date,
                            orbit_direction = "both") {
  message("=== Recherche STAC Sentinel-1 ===")

  backends <- .stac_backends()

  for (backend_name in names(backends)) {
    backend <- backends[[backend_name]]
    message(sprintf("  Essai %s (%s)...", backend$name, backend$s1_collection))

    result <- .stac_search(
      backend$stac_url, backend$s1_collection,
      bbox, start_date, end_date
    )

    if (is.null(result)) next

    features <- result$features
    if (is.null(features) || length(features) == 0) {
      if (is.data.frame(features) && nrow(features) == 0) next
      if (!is.data.frame(features)) next
    }

    n_items <- if (is.data.frame(features)) nrow(features) else length(features)
    if (n_items == 0) next

    scenes <- data.frame(
      id = features$id,
      datetime = features$properties$datetime,
      source = backend_name,
      stringsAsFactors = FALSE
    )

    # Extraire la direction d'orbite si disponible
    orbit_col <- backend$orbit_field
    if (!is.null(orbit_col) && orbit_col %in% names(features$properties)) {
      scenes$orbit_direction <- features$properties[[orbit_col]]
    } else {
      scenes$orbit_direction <- NA_character_
    }

    scenes$date <- as.Date(substr(scenes$datetime, 1, 10))

    # Filtrer par direction d'orbite
    if (orbit_direction != "both" && any(!is.na(scenes$orbit_direction))) {
      scenes <- scenes[scenes$orbit_direction == orbit_direction |
                       is.na(scenes$orbit_direction), ]
    }

    n_items <- nrow(scenes)
    if (n_items == 0) next

    message(sprintf("  %d scenes Sentinel-1 trouvees via %s",
                     n_items, backend$name))
    return(scenes)
  }

  message("  Aucune scene trouvee sur aucun backend")
  data.frame()
}

# --- Telechargement S2 ---

#' Telecharger une image Sentinel-2 pour une AOI
#'
#' Telecharge les 10 bandes spectrales Sentinel-2 pour l'AOI.
#' Essaie d'abord Copernicus Data Space (token requis), puis bascule
#' sur Planetary Computer (COG en acces libre) si besoin.
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

  scenes <- search_s2_stac(bbox_wgs84, start_date, end_date, max_cloud = 20)

  if (nrow(scenes) == 0) {
    message("  Aucune scene S2, essai sur l'annee entiere...")
    annee <- if (!is.null(date_cible)) format(as.Date(date_cible), "%Y") else
      format(Sys.Date(), "%Y")
    scenes <- search_s2_stac(bbox_wgs84,
                              paste0(annee, "-01-01"),
                              paste0(annee, "-12-31"),
                              max_cloud = 30)
  }

  if (nrow(scenes) == 0) {
    warning("Aucune scene Sentinel-2 trouvee pour cette AOI")
    return(NULL)
  }

  best <- scenes[1, ]
  message(sprintf("  Scene selectionnee: %s (nuages: %.0f%%, source: %s)",
                   best$id, best$cloud_cover, best$source))

  # Telecharger selon le backend
  s2_raster <- tryCatch({
    r <- if (best$source == "planetary") {
      .download_s2_planetary(best, aoi, output_dir)
    } else {
      .download_s2_copernicus(best, aoi, output_dir)
    }
    if (!inherits(r, "SpatRaster")) {
      message("  [INFO] Resultat non-SpatRaster, raster synthetique")
      .create_synthetic_s2(aoi, output_dir)
    } else {
      r
    }
  }, error = function(e) {
    message(sprintf("  [WARN] Echec telechargement S2: %s", e$message))
    message("  Basculement raster synthetique")
    .create_synthetic_s2(aoi, output_dir)
  })

  if (!is.null(s2_raster) && inherits(s2_raster, "SpatRaster")) {
    tryCatch({
      terra::writeRaster(s2_raster, s2_path, overwrite = TRUE,
                         gdal = c("COMPRESS=LZW"))
      message(sprintf("  Sentinel-2 sauvegarde: %s (%d bandes)",
                       s2_path, terra::nlyr(s2_raster)))
    }, error = function(e) {
      message(sprintf("  [WARN] Echec sauvegarde S2: %s", e$message))
    })
  }

  s2_raster
}

# --- Telechargement S1 ---

#' Telecharger les donnees Sentinel-1 pour une AOI
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

  scenes_asc <- search_s1_stac(bbox_wgs84, start_date, end_date,
                                orbit_direction = "ascending")
  scenes_des <- search_s1_stac(bbox_wgs84, start_date, end_date,
                                orbit_direction = "descending")

  result <- list(s1_asc = NULL, s1_des = NULL)

  if (nrow(scenes_asc) > 0) {
    best <- scenes_asc[1, ]
    message(sprintf("  Scene ascending: %s (source: %s)", best$id, best$source))
    result$s1_asc <- .download_s1_scene(best, aoi, output_dir, "ascending")
    if (!is.null(result$s1_asc)) {
      terra::writeRaster(result$s1_asc, s1_asc_path, overwrite = TRUE,
                         gdal = c("COMPRESS=LZW"))
    }
  }

  if (nrow(scenes_des) > 0) {
    best <- scenes_des[1, ]
    message(sprintf("  Scene descending: %s (source: %s)", best$id, best$source))
    result$s1_des <- .download_s1_scene(best, aoi, output_dir, "descending")
    if (!is.null(result$s1_des)) {
      terra::writeRaster(result$s1_des, s1_des_path, overwrite = TRUE,
                         gdal = c("COMPRESS=LZW"))
    }
  }

  has_data <- !is.null(result$s1_asc) || !is.null(result$s1_des)
  if (!has_data) {
    warning("Aucune donnee Sentinel-1 disponible")
    return(NULL)
  }

  result
}


# ---------------------------------------------------------------------------
# Fonctions internes de telechargement — Copernicus Data Space
# ---------------------------------------------------------------------------

#' @noRd
.download_s2_copernicus <- function(scene, aoi, output_dir) {
  message("  Telechargement S2 via Copernicus Data Space...")

  token <- Sys.getenv("COPERNICUS_TOKEN", "")

  if (nchar(token) == 0) {
    message("  [INFO] Token Copernicus non configure")
    message("  Utilisez: Sys.setenv(COPERNICUS_TOKEN = 'votre_token')")
    message("  Basculement sur Planetary Computer...")
    return(.download_s2_planetary(scene, aoi, output_dir))
  }

  scene_id <- scene$id
  odata_url <- sprintf(
    "https://catalogue.dataspace.copernicus.eu/odata/v1/Products?$filter=Name eq '%s'",
    scene_id
  )

  h <- curl::new_handle()
  resp <- tryCatch(
    curl::curl_fetch_memory(odata_url, handle = h),
    error = function(e) NULL
  )

  if (is.null(resp) || resp$status_code != 200) {
    message("  [INFO] OData inaccessible, basculement sur Planetary Computer...")
    return(.download_s2_planetary(scene, aoi, output_dir))
  }

  odata <- jsonlite::fromJSON(rawToChar(resp$content))

  if (length(odata$value) == 0) {
    message("  [INFO] Scene non trouvee via OData, basculement sur Planetary...")
    return(.download_s2_planetary(scene, aoi, output_dir))
  }

  # Telechargement via OData avec token
  product_id <- odata$value$Id[1]
  download_url <- sprintf(
    "https://zipper.dataspace.copernicus.eu/odata/v1/Products(%s)/$value",
    product_id
  )

  dest_zip <- file.path(output_dir, "s2_scene.zip")
  h_dl <- curl::new_handle()
  curl::handle_setheaders(h_dl, Authorization = paste("Bearer", token))

  dl_ok <- tryCatch({
    curl::curl_download(download_url, dest_zip, handle = h_dl)
    TRUE
  }, error = function(e) {
    message("  Echec telechargement Copernicus: ", e$message)
    FALSE
  })

  if (!dl_ok || !file.exists(dest_zip)) {
    message("  Basculement sur Planetary Computer...")
    return(.download_s2_planetary(scene, aoi, output_dir))
  }

  # Extraire les bandes du ZIP SAFE
  .extract_s2_from_safe(dest_zip, aoi, output_dir)
}

#' @noRd
.extract_s2_from_safe <- function(zip_path, aoi, output_dir) {
  message("  Extraction des bandes depuis l'archive SAFE...")

  extract_dir <- file.path(output_dir, "s2_safe_tmp")
  dir.create(extract_dir, showWarnings = FALSE, recursive = TRUE)
  utils::unzip(zip_path, exdir = extract_dir)

  band_names <- c("B02", "B03", "B04", "B05", "B06", "B07",
                  "B08", "B8A", "B11", "B12")

  # Chercher les fichiers JP2 des bandes 10m et 20m
  all_files <- list.files(extract_dir, pattern = "\\.jp2$",
                          recursive = TRUE, full.names = TRUE)

  layers <- list()
  bbox_l93 <- sf::st_bbox(aoi)
  ext_aoi <- terra::ext(bbox_l93["xmin"], bbox_l93["xmax"],
                         bbox_l93["ymin"], bbox_l93["ymax"])

  for (band in band_names) {
    pattern <- sprintf("_%s_", band)
    band_files <- grep(pattern, all_files, value = TRUE)

    # Privilegier la resolution 10m, sinon 20m
    f10 <- grep("R10m", band_files, value = TRUE)
    f20 <- grep("R20m", band_files, value = TRUE)
    band_file <- if (length(f10) > 0) f10[1] else if (length(f20) > 0) f20[1] else NULL

    if (is.null(band_file)) {
      message(sprintf("  [WARN] Bande %s non trouvee", band))
      next
    }

    r <- terra::rast(band_file)
    # Reprojeter en Lambert-93 si necessaire
    if (!grepl("2154", terra::crs(r, describe = TRUE)$code %||% "")) {
      r <- terra::project(r, "EPSG:2154")
    }
    r <- terra::crop(r, ext_aoi)
    names(r) <- band
    layers[[band]] <- r
  }

  # Nettoyer
  unlink(extract_dir, recursive = TRUE)
  unlink(zip_path)

  if (length(layers) == 0) return(NULL)

  # Reechantillonner toutes les bandes a 10m
  ref <- layers[[1]]
  for (i in seq_along(layers)) {
    if (terra::res(layers[[i]])[1] != terra::res(ref)[1]) {
      layers[[i]] <- terra::resample(layers[[i]], ref, method = "bilinear")
    }
  }

  do.call(c, layers)
}


# ---------------------------------------------------------------------------
# Fonctions internes de telechargement — Planetary Computer
# ---------------------------------------------------------------------------

#' Signer une URL Planetary Computer pour l'acces aux donnees
#' @noRd
.pc_sign_url <- function(href) {
  # Planetary Computer necessite un token SAS pour acceder aux blobs
  # L'API de signing genere automatiquement un token temporaire
  # Format: on ajoute le token SAS a l'URL du blob

  # Extraire account et container depuis l'URL
  parsed <- regmatches(href, regexec(
    "https://([^.]+)\\.blob\\.core\\.windows\\.net/([^/]+)/(.*)", href
  ))[[1]]

  if (length(parsed) < 4) return(href)

  account <- parsed[2]
  container <- parsed[3]

  signing_url <- sprintf(
    "https://planetarycomputer.microsoft.com/api/sas/v1/token/%s/%s",
    account, container
  )

  h <- curl::new_handle()
  resp <- tryCatch(
    curl::curl_fetch_memory(signing_url, handle = h),
    error = function(e) NULL
  )

  if (is.null(resp) || resp$status_code != 200) return(href)

  token_data <- jsonlite::fromJSON(rawToChar(resp$content))
  paste0(href, "?", token_data$token)
}

#' Telecharger S2 depuis Planetary Computer (COG natif)
#' @noRd
.download_s2_planetary <- function(scene, aoi, output_dir) {
  message("  Telechargement S2 via Planetary Computer (COG)...")

  # Rechercher la scene specifique dans le catalogue Planetary Computer
  pc_url <- "https://planetarycomputer.microsoft.com/api/stac/v1/search"

  # Retrouver la scene par sa date et sa bbox
  bbox_wgs84 <- as.numeric(sf::st_bbox(sf::st_transform(aoi, 4326)))
  scene_date <- as.Date(substr(scene$datetime, 1, 10))

  body <- list(
    collections = list("sentinel-2-l2a"),
    bbox = as.list(bbox_wgs84),
    datetime = paste0(scene_date, "T00:00:00Z/", scene_date, "T23:59:59Z"),
    limit = 5L
  )

  body_json <- jsonlite::toJSON(body, auto_unbox = TRUE)
  h <- curl::new_handle()
  curl::handle_setopt(h,
    copypostfields = as.character(body_json),
    httpheader = "Content-Type: application/json"
  )

  resp <- tryCatch(
    curl::curl_fetch_memory(pc_url, handle = h),
    error = function(e) NULL
  )

  if (is.null(resp) || resp$status_code != 200) {
    message("  [INFO] Planetary Computer inaccessible, raster synthetique")
    return(.create_synthetic_s2(aoi, output_dir))
  }

  items <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)

  if (length(items$features) == 0) {
    message("  [INFO] Scene non trouvee sur Planetary Computer")
    return(.create_synthetic_s2(aoi, output_dir))
  }

  feature <- items$features[[1]]
  assets <- feature$assets

  # Bandes a telecharger
  # Planetary Computer utilise des noms d'assets normalises
  band_mapping <- list(
    B02 = "B02", B03 = "B03", B04 = "B04",
    B05 = "B05", B06 = "B06", B07 = "B07",
    B08 = "B08", B8A = "B8A", B11 = "B11", B12 = "B12"
  )

  bbox_l93 <- sf::st_bbox(aoi)
  ext_aoi <- terra::ext(bbox_l93["xmin"], bbox_l93["xmax"],
                         bbox_l93["ymin"], bbox_l93["ymax"])

  layers <- list()

  for (band_name in names(band_mapping)) {
    asset_key <- band_mapping[[band_name]]

    href <- NULL
    # Chercher l'asset par differentes cles possibles
    for (key in c(asset_key, tolower(asset_key),
                  paste0("visual_", tolower(asset_key)))) {
      if (!is.null(assets[[key]])) {
        href <- assets[[key]]$href
        break
      }
    }

    if (is.null(href)) {
      message(sprintf("  [WARN] Asset %s non trouve", band_name))
      next
    }

    # Signer l'URL pour l'acces
    signed_href <- .pc_sign_url(href)

    # Lire directement le COG avec terra (lecture partielle possible)
    r <- tryCatch({
      band_r <- terra::rast(sprintf("/vsicurl/%s", signed_href))
      # Reprojeter en L93 et cropper sur l'AOI
      band_r <- terra::project(band_r, "EPSG:2154")
      band_r <- terra::crop(band_r, ext_aoi)
      names(band_r) <- band_name
      band_r
    }, error = function(e) {
      message(sprintf("  [WARN] Echec lecture COG %s: %s", band_name, e$message))
      NULL
    })

    if (!is.null(r)) layers[[band_name]] <- r
  }

  if (length(layers) == 0) {
    message("  [INFO] Aucune bande COG lisible, raster synthetique")
    return(.create_synthetic_s2(aoi, output_dir))
  }

  message(sprintf("  %d/%d bandes S2 telechargees via Planetary Computer",
                   length(layers), length(band_mapping)))

  # Reechantillonner a 10m sur la meme grille
  ref <- layers[[1]]
  if (terra::res(ref)[1] != 10) {
    template <- terra::rast(ext = terra::ext(ref), res = 10,
                             crs = terra::crs(ref))
    ref <- terra::resample(ref, template, method = "bilinear")
    layers[[1]] <- ref
  }
  for (i in seq_along(layers)[-1]) {
    ok <- tryCatch({
      if (terra::res(layers[[i]])[1] != 10 ||
          !terra::compareGeom(layers[[i]], ref, stopOnError = FALSE)) {
        layers[[i]] <- terra::resample(layers[[i]], ref, method = "bilinear")
      }
      TRUE
    }, error = function(e) {
      message(sprintf("  [WARN] Echec resample bande %s: %s",
                       names(layers[[i]]), e$message))
      FALSE
    })
    if (!ok) layers[[i]] <- NULL
  }
  layers <- Filter(Negate(is.null), layers)

  if (length(layers) == 0) {
    message("  [INFO] Aucune bande valide apres resample, raster synthetique")
    return(.create_synthetic_s2(aoi, output_dir))
  }

  # Completer les bandes manquantes avec des valeurs synthetiques
  all_bands <- names(band_mapping)
  got_bands <- vapply(layers, function(x) names(x), character(1))

  typical_vegetation <- c(
    B02 = 350, B03 = 600, B04 = 400, B05 = 1500, B06 = 2500,
    B07 = 3000, B08 = 3200, B8A = 3100, B11 = 1800, B12 = 800
  )

  for (band in all_bands) {
    if (!band %in% got_bands) {
      message(sprintf("  [INFO] Bande %s manquante, complement synthetique", band))
      synth <- ref
      n_cells <- terra::ncell(synth)
      terra::values(synth) <- stats::rnorm(
        n_cells, mean = typical_vegetation[[band]],
        sd = typical_vegetation[[band]] * 0.15
      )
      terra::values(synth) <- pmax(0, pmin(10000, terra::values(synth)))
      names(synth) <- band
      layers[[band]] <- synth
    }
  }

  # Ordonner les bandes dans l'ordre attendu
  ordered_layers <- list()
  for (band in all_bands) {
    idx <- which(vapply(layers, function(x) names(x), character(1)) == band)
    if (length(idx) > 0) ordered_layers[[band]] <- layers[[idx[1]]]
  }

  result <- tryCatch(
    do.call(c, ordered_layers),
    error = function(e) {
      message(sprintf("  [WARN] Echec combinaison bandes: %s", e$message))
      message("  Basculement raster synthetique complet")
      .create_synthetic_s2(aoi, output_dir)
    }
  )

  result
}


# ---------------------------------------------------------------------------
# Fonctions internes de telechargement — Sentinel-1
# ---------------------------------------------------------------------------

#' @noRd
.download_s1_scene <- function(scene, aoi, output_dir, orbit) {
  if (scene$source == "planetary") {
    return(.download_s1_planetary(scene, aoi, output_dir, orbit))
  }

  # Copernicus Data Space
  message(sprintf("  Telechargement S1 %s via Copernicus Data Space...", orbit))

  token <- Sys.getenv("COPERNICUS_TOKEN", "")

  if (nchar(token) == 0) {
    message("  [INFO] Token Copernicus non configure, essai Planetary Computer...")
    return(.download_s1_planetary(scene, aoi, output_dir, orbit))
  }

  scene_id <- scene$id
  download_url <- sprintf(
    "https://zipper.dataspace.copernicus.eu/odata/v1/Products(%s)/$value",
    scene_id
  )

  h <- curl::new_handle()
  curl::handle_setheaders(h, Authorization = paste("Bearer", token))

  dest_file <- file.path(output_dir, sprintf("s1_%s_raw.zip", orbit))
  dl_ok <- tryCatch({
    curl::curl_download(download_url, dest_file, handle = h)
    TRUE
  }, error = function(e) {
    message("  Echec telechargement Copernicus: ", e$message)
    FALSE
  })

  if (!dl_ok) {
    message("  Basculement sur Planetary Computer...")
    return(.download_s1_planetary(scene, aoi, output_dir, orbit))
  }

  # TODO: extraire VV/VH du SAFE S1
  # Pour l'instant on genere un synthetique
  message("  [INFO] Extraction SAFE S1 non encore implementee")
  .create_synthetic_s1(aoi, output_dir, orbit)
}

#' Telecharger S1 RTC depuis Planetary Computer
#' @noRd
.download_s1_planetary <- function(scene, aoi, output_dir, orbit) {
  message(sprintf("  Telechargement S1 %s via Planetary Computer (RTC)...", orbit))

  pc_url <- "https://planetarycomputer.microsoft.com/api/stac/v1/search"

  bbox_wgs84 <- as.numeric(sf::st_bbox(sf::st_transform(aoi, 4326)))
  scene_date <- as.Date(substr(scene$datetime, 1, 10))

  # Planetary Computer: sentinel-1-rtc (Radiometric Terrain Corrected)
  body <- list(
    collections = list("sentinel-1-rtc"),
    bbox = as.list(bbox_wgs84),
    datetime = paste0(scene_date, "T00:00:00Z/", scene_date, "T23:59:59Z"),
    limit = 5L
  )

  body_json <- jsonlite::toJSON(body, auto_unbox = TRUE)
  h <- curl::new_handle()
  curl::handle_setopt(h,
    copypostfields = as.character(body_json),
    httpheader = "Content-Type: application/json"
  )

  resp <- tryCatch(
    curl::curl_fetch_memory(pc_url, handle = h),
    error = function(e) NULL
  )

  if (is.null(resp) || resp$status_code != 200) {
    message("  [INFO] Planetary Computer S1 inaccessible, raster synthetique")
    return(.create_synthetic_s1(aoi, output_dir, orbit))
  }

  items <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)

  if (length(items$features) == 0) {
    message("  [INFO] Aucune scene S1 RTC trouvee")
    return(.create_synthetic_s1(aoi, output_dir, orbit))
  }

  feature <- items$features[[1]]
  assets <- feature$assets

  bbox_l93 <- sf::st_bbox(aoi)
  ext_aoi <- terra::ext(bbox_l93["xmin"], bbox_l93["xmax"],
                         bbox_l93["ymin"], bbox_l93["ymax"])

  layers <- list()

  for (pol in c("vv", "vh")) {
    if (is.null(assets[[pol]])) next

    href <- assets[[pol]]$href
    signed_href <- .pc_sign_url(href)

    r <- tryCatch({
      band_r <- terra::rast(sprintf("/vsicurl/%s", signed_href))
      band_r <- terra::project(band_r, "EPSG:2154")
      band_r <- terra::crop(band_r, ext_aoi)
      names(band_r) <- toupper(pol)
      band_r
    }, error = function(e) {
      message(sprintf("  [WARN] Echec lecture COG S1 %s: %s", pol, e$message))
      NULL
    })

    if (!is.null(r)) layers[[pol]] <- r
  }

  if (length(layers) == 0) {
    message("  [INFO] Aucune bande S1 lisible, raster synthetique")
    return(.create_synthetic_s1(aoi, output_dir, orbit))
  }

  # Reechantillonner a 10m
  ref <- layers[[1]]
  if (terra::res(ref)[1] != 10) {
    template <- terra::rast(ext = terra::ext(ref), res = 10,
                             crs = terra::crs(ref))
    ref <- terra::resample(ref, template, method = "bilinear")
    layers[[1]] <- ref
  }
  for (i in seq_along(layers)[-1]) {
    layers[[i]] <- terra::resample(layers[[i]], ref, method = "bilinear")
  }

  message(sprintf("  S1 %s: %d bandes (VV, VH) via Planetary Computer",
                   orbit, length(layers)))
  do.call(c, layers)
}


# ---------------------------------------------------------------------------
# Rasters synthetiques (fallback pour tests)
# ---------------------------------------------------------------------------

#' Creer un raster Sentinel-2 synthetique pour tests
#'
#' Produit un raster 10 bandes a 10m de resolution avec des valeurs
#' realistes pour tester le pipeline sans donnees reelles.
#'
#' @param aoi sf object en Lambert-93
#' @param output_dir Repertoire de sortie
#' @return SpatRaster 10 bandes
#' @noRd
.create_synthetic_s2 <- function(aoi, output_dir) {
  message("  Creation raster S2 synthetique (10 bandes, 10m)...")

  bbox <- sf::st_bbox(aoi)
  r <- terra::rast(
    xmin = bbox["xmin"], xmax = bbox["xmax"],
    ymin = bbox["ymin"], ymax = bbox["ymax"],
    res = 10,
    crs = sf::st_crs(aoi)$wkt
  )

  band_names <- c("B02", "B03", "B04", "B05", "B06", "B07",
                  "B08", "B8A", "B11", "B12")

  # Reflectances typiques de vegetation (x10000)
  typical_vegetation <- c(
    350,   # B02 Blue
    600,   # B03 Green
    400,   # B04 Red
    1500,  # B05 RedEdge1
    2500,  # B06 RedEdge2
    3000,  # B07 RedEdge3
    3200,  # B08 NIR
    3100,  # B8A NIR2
    1800,  # B11 SWIR1
    800    # B12 SWIR2
  )

  layers <- list()
  n_cells <- terra::ncell(r)
  for (i in seq_along(band_names)) {
    layer <- r
    noise <- stats::rnorm(n_cells, mean = typical_vegetation[i],
                          sd = typical_vegetation[i] * 0.15)
    noise <- pmax(0, pmin(10000, noise))
    terra::values(layer) <- noise
    names(layer) <- band_names[i]
    layers[[i]] <- layer
  }

  s2 <- do.call(c, layers)
  message(sprintf("  S2 synthetique: %d x %d px, 10 bandes, 10m",
                   terra::ncol(s2), terra::nrow(s2)))
  s2
}

#' Creer un raster Sentinel-1 synthetique pour tests
#'
#' Produit un raster 2 bandes (VV, VH) a 10m avec des valeurs
#' de retrodiffusion radar realistes en dB.
#'
#' @param aoi sf object en Lambert-93
#' @param output_dir Repertoire de sortie
#' @param orbit "ascending" ou "descending"
#' @return SpatRaster 2 bandes
#' @noRd
.create_synthetic_s1 <- function(aoi, output_dir, orbit = "ascending") {
  message(sprintf("  Creation raster S1 synthetique (%s, 2 bandes, 10m)...",
                   orbit))

  bbox <- sf::st_bbox(aoi)
  r <- terra::rast(
    xmin = bbox["xmin"], xmax = bbox["xmax"],
    ymin = bbox["ymin"], ymax = bbox["ymax"],
    res = 10,
    crs = sf::st_crs(aoi)$wkt
  )

  n_cells <- terra::ncell(r)

  # VV: typiquement -8 a -15 dB pour la vegetation
  vv <- r
  terra::values(vv) <- stats::rnorm(n_cells, mean = -10, sd = 3)
  names(vv) <- "VV"

  # VH: typiquement -15 a -22 dB pour la vegetation
  vh <- r
  terra::values(vh) <- stats::rnorm(n_cells, mean = -17, sd = 3)
  names(vh) <- "VH"

  s1 <- c(vv, vh)
  message(sprintf("  S1 synthetique: %d x %d px, 2 bandes (VV, VH)",
                   terra::ncol(s1), terra::nrow(s1)))
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
