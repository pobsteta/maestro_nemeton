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

# --- Periodes temporelles ---

#' Construire les periodes de recherche multi-annuelles
#'
#' A partir d'un vecteur d'annees et d'une saison, genere les intervalles
#' de dates a interroger via STAC.
#'
#' @param annees_sentinel Vecteur d'annees (ex: 2021:2024)
#' @param saison Saison cible : "ete" (juin-sept), "printemps" (mars-mai),
#'   "automne" (sept-nov), "annee" (jan-dec), ou vecteur de 2 mois c(debut, fin)
#' @return data.frame avec colonnes `start_date` et `end_date`
#' @export
build_date_ranges <- function(annees_sentinel, saison = "ete") {
  # Saisons predefinies (mois debut, mois fin)
  saisons <- list(
    ete       = c(6, 9),
    printemps = c(3, 5),
    automne   = c(9, 11),
    annee     = c(1, 12)
  )

  if (is.character(saison) && saison %in% names(saisons)) {
    mois <- saisons[[saison]]
  } else if (is.numeric(saison) && length(saison) == 2) {
    mois <- saison
  } else {
    stop("saison doit etre 'ete', 'printemps', 'automne', 'annee', ",
         "ou un vecteur de 2 mois c(debut, fin)")
  }

  data.frame(
    start_date = sprintf("%d-%02d-01", annees_sentinel, mois[1]),
    end_date   = sprintf("%d-%02d-%02d", annees_sentinel, mois[2],
                          ifelse(mois[2] %in% c(4, 6, 9, 11), 30, 31)),
    annee      = annees_sentinel,
    stringsAsFactors = FALSE
  )
}

# --- Telechargement S2 mono-scene (interne) ---

#' Telecharger une scene Sentinel-2 unique
#'
#' @param sel_feature Feature STAC signe
#' @param aoi_vect SpatVector WGS84
#' @param template SpatRaster template 10m Lambert-93
#' @return SpatRaster 10 bandes, ou NULL
#' @noRd
.download_s2_scene <- function(sel_feature, aoi_vect, template) {
  conf <- .stac_config()
  layers <- list()

  for (band in conf$s2_bands) {
    url <- sel_feature$assets[[band]]$href
    if (is.null(url)) next

    r <- .read_remote_cog(url, aoi_vect)
    if (!is.null(r)) {
      names(r) <- band
      layers[[band]] <- r
    }
  }

  if (length(layers) == 0) return(NULL)

  # Reprojeter et reechantillonner a 10m Lambert-93
  for (i in seq_along(layers)) {
    band_name <- names(layers[[i]])
    layers[[i]] <- tryCatch({
      r <- terra::project(layers[[i]], "EPSG:2154")
      r <- terra::resample(r, template, method = "bilinear")
      names(r) <- band_name
      r
    }, error = function(e) NULL)
  }
  layers <- Filter(Negate(is.null), layers)

  if (length(layers) == 0) return(NULL)

  tryCatch(do.call(c, layers), error = function(e) NULL)
}

# --- Composite median ---

#' Calculer un composite median a partir de plusieurs rasters
#'
#' Calcule la mediane pixel par pixel pour chaque bande a partir
#' d'une liste de rasters multi-bandes. La mediane est robuste aux
#' valeurs aberrantes (nuages residuels, secheresse ponctuelle).
#'
#' @param rasters Liste de SpatRaster (meme nombre de bandes et emprise)
#' @return SpatRaster composite median
#' @export
calculer_composite_median <- function(rasters) {
  if (length(rasters) == 1) return(rasters[[1]])

  message(sprintf("  Calcul composite median a partir de %d scenes...",
                   length(rasters)))

  # Empiler toutes les scenes dans un SpatRasterDataset par bande
  n_bandes <- terra::nlyr(rasters[[1]])
  band_names <- names(rasters[[1]])
  composite_layers <- list()

  for (b in seq_len(n_bandes)) {
    # Extraire la bande b de chaque scene
    band_stack <- terra::rast(lapply(rasters, function(r) r[[b]]))
    # Mediane pixel-wise
    composite_layers[[b]] <- terra::app(band_stack, fun = "median",
                                         na.rm = TRUE)
    names(composite_layers[[b]]) <- band_names[b]
  }

  composite <- do.call(c, composite_layers)
  message(sprintf("  Composite median: %d bandes", terra::nlyr(composite)))
  composite
}

# --- Telechargement S2 ---

#' Telecharger une image Sentinel-2 pour une AOI
#'
#' Telecharge les 10 bandes spectrales Sentinel-2 L2A depuis
#' Microsoft Planetary Computer (COG en acces libre). Utilise rstac
#' pour la recherche et le signing automatique des URLs.
#'
#' Supporte le mode multitemporel : en fournissant `annees_sentinel`,
#' plusieurs scenes sont telechargees sur plusieurs annees/saisons et
#' combinees en un composite median pixel par pixel. Cela permet de
#' gommer les annees seches et de reduire l'impact des nuages.
#'
#' @param aoi sf object en Lambert-93
#' @param output_dir Repertoire de sortie
#' @param date_cible Date cible (format "YYYY-MM-DD", optionnel).
#'   Ignore si `annees_sentinel` est fourni.
#' @param annees_sentinel Vecteur d'annees pour le composite multi-annuel
#'   (ex: `2021:2024`). Si NULL, utilise le comportement mono-date.
#' @param saison Saison cible pour le composite : "ete" (defaut),
#'   "printemps", "automne", "annee", ou vecteur de 2 mois c(debut, fin)
#' @param max_scenes_par_annee Nombre max de scenes a retenir par annee
#'   pour le composite (defaut: 3, les moins nuageuses)
#' @return SpatRaster avec les 10 bandes S2, ou NULL
#' @export
download_s2_for_aoi <- function(aoi, output_dir, date_cible = NULL,
                                 annees_sentinel = NULL, saison = "ete",
                                 max_scenes_par_annee = 3L) {
  message("=== Telechargement Sentinel-2 pour l'AOI ===")

  # Determiner le nom du fichier de sortie
  if (!is.null(annees_sentinel)) {
    s2_path <- file.path(output_dir, sprintf("sentinel2_composite_%s_%s.tif",
                                               paste(range(annees_sentinel),
                                                     collapse = "-"),
                                               saison))
  } else {
    s2_path <- file.path(output_dir, "sentinel2.tif")
  }

  if (file.exists(s2_path)) {
    message("  Cache: ", s2_path)
    return(terra::rast(s2_path))
  }

  aoi_wgs84 <- sf::st_transform(aoi, 4326)
  bbox_wgs84 <- as.numeric(sf::st_bbox(aoi_wgs84))
  aoi_vect <- terra::vect(aoi_wgs84)

  # Template 10m en Lambert-93
  bbox_l93 <- sf::st_bbox(aoi)
  ext_aoi <- terra::ext(bbox_l93["xmin"], bbox_l93["xmax"],
                         bbox_l93["ymin"], bbox_l93["ymax"])
  template <- terra::rast(ext = ext_aoi, res = 10, crs = "EPSG:2154")

  # ---- Mode multitemporel ----
  if (!is.null(annees_sentinel)) {
    message(sprintf("  Mode multitemporel: %d annees, saison = %s",
                     length(annees_sentinel),
                     if (is.character(saison)) saison else
                       paste(saison, collapse = "-")))

    date_ranges <- build_date_ranges(annees_sentinel, saison)
    all_scenes <- list()

    for (i in seq_len(nrow(date_ranges))) {
      yr <- date_ranges$annee[i]
      message(sprintf("  --- Annee %d ---", yr))

      search_result <- search_s2_stac(bbox_wgs84,
                                        date_ranges$start_date[i],
                                        date_ranges$end_date[i],
                                        max_cloud = 30)

      if (is.null(search_result)) {
        message(sprintf("    Aucune scene S2 pour %d, essai annee entiere...",
                         yr))
        search_result <- search_s2_stac(bbox_wgs84,
                                          paste0(yr, "-01-01"),
                                          paste0(yr, "-12-31"),
                                          max_cloud = 30)
      }

      if (is.null(search_result)) {
        message(sprintf("    Aucune scene S2 trouvee pour %d", yr))
        next
      }

      scenes <- search_result$scenes
      items_signed <- search_result$items
      n_sel <- min(nrow(scenes), max_scenes_par_annee)

      for (j in seq_len(n_sel)) {
        sc <- scenes[j, ]
        message(sprintf("    Scene %d/%d: %s (nuages: %.0f%%)",
                         j, n_sel, sc$id, sc$cloud_cover))

        sel_feature <- NULL
        for (feat in items_signed$features) {
          if (feat$id == sc$id) { sel_feature <- feat; break }
        }
        if (is.null(sel_feature)) next

        r <- .download_s2_scene(sel_feature, aoi_vect, template)
        if (!is.null(r)) {
          all_scenes[[length(all_scenes) + 1]] <- r
          message(sprintf("    -> OK (%d bandes)", terra::nlyr(r)))
        }
      }
    }

    if (length(all_scenes) == 0) {
      warning("Aucune scene Sentinel-2 telechargee sur la periode multi-annuelle")
      return(NULL)
    }

    message(sprintf("  Total: %d scenes S2 telechargees", length(all_scenes)))
    s2_raster <- calculer_composite_median(all_scenes)

  } else {
    # ---- Mode mono-date (comportement original) ----
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

    sel_feature <- NULL
    for (feat in items_signed$features) {
      if (feat$id == best$id) { sel_feature <- feat; break }
    }

    if (is.null(sel_feature)) {
      warning("Feature signe non trouve pour la scene selectionnee")
      return(NULL)
    }

    s2_raster <- .download_s2_scene(sel_feature, aoi_vect, template)

    if (is.null(s2_raster)) {
      warning("Aucune bande S2 telechargee")
      return(NULL)
    }

    message(sprintf("  %d bandes S2 telechargees", terra::nlyr(s2_raster)))
  }

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
#' Supporte le mode multitemporel : en fournissant `annees_sentinel`,
#' plusieurs scenes sont telechargees et combinees en composite median.
#'
#' @param aoi sf object en Lambert-93
#' @param output_dir Repertoire de sortie
#' @param date_cible Date cible (optionnel). Ignore si `annees_sentinel`
#'   est fourni.
#' @param annees_sentinel Vecteur d'annees pour le composite multi-annuel
#'   (ex: `2021:2024`). Si NULL, utilise le comportement mono-date.
#' @param saison Saison cible pour le composite : "ete" (defaut),
#'   "printemps", "automne", "annee", ou vecteur de 2 mois
#' @param max_scenes_par_annee Nombre max de scenes par annee/orbite
#'   pour le composite (defaut: 3)
#' @return Liste avec `s1_asc` et `s1_des` (SpatRaster 2 bandes VV+VH chacun),
#'   ou NULL si non disponible
#' @export
download_s1_for_aoi <- function(aoi, output_dir, date_cible = NULL,
                                 annees_sentinel = NULL, saison = "ete",
                                 max_scenes_par_annee = 3L) {
  message("=== Telechargement Sentinel-1 pour l'AOI ===")

  # Determiner les noms de fichiers
  if (!is.null(annees_sentinel)) {
    suffix <- sprintf("_composite_%s_%s",
                       paste(range(annees_sentinel), collapse = "-"),
                       if (is.character(saison)) saison else
                         paste(saison, collapse = "-"))
    s1_asc_path <- file.path(output_dir, paste0("sentinel1_asc", suffix, ".tif"))
    s1_des_path <- file.path(output_dir, paste0("sentinel1_des", suffix, ".tif"))
  } else {
    s1_asc_path <- file.path(output_dir, "sentinel1_asc.tif")
    s1_des_path <- file.path(output_dir, "sentinel1_des.tif")
  }

  if (file.exists(s1_asc_path) && file.exists(s1_des_path)) {
    message("  Cache: S1 ascending + descending")
    return(list(
      s1_asc = terra::rast(s1_asc_path),
      s1_des = terra::rast(s1_des_path)
    ))
  }

  aoi_wgs84 <- sf::st_transform(aoi, 4326)
  bbox_wgs84 <- as.numeric(sf::st_bbox(aoi_wgs84))

  result <- list(s1_asc = NULL, s1_des = NULL)

  # ---- Mode multitemporel ----
  if (!is.null(annees_sentinel)) {
    message(sprintf("  Mode multitemporel S1: %d annees, saison = %s",
                     length(annees_sentinel),
                     if (is.character(saison)) saison else
                       paste(saison, collapse = "-")))

    date_ranges <- build_date_ranges(annees_sentinel, saison)

    for (orbit in c("ascending", "descending")) {
      orbit_key <- ifelse(orbit == "ascending", "s1_asc", "s1_des")
      orbit_path <- ifelse(orbit == "ascending", s1_asc_path, s1_des_path)

      if (file.exists(orbit_path)) {
        result[[orbit_key]] <- terra::rast(orbit_path)
        message(sprintf("  Cache: S1 %s", orbit))
        next
      }

      all_orbit_scenes <- list()

      for (i in seq_len(nrow(date_ranges))) {
        yr <- date_ranges$annee[i]
        message(sprintf("  --- S1 %s, annee %d ---", orbit, yr))

        search_result <- search_s1_stac(bbox_wgs84,
                                          date_ranges$start_date[i],
                                          date_ranges$end_date[i],
                                          orbit_direction = orbit)

        if (is.null(search_result)) {
          message(sprintf("    Aucun produit S1 %s pour %d, essai annee...",
                           orbit, yr))
          search_result <- search_s1_stac(bbox_wgs84,
                                            paste0(yr, "-01-01"),
                                            paste0(yr, "-12-31"),
                                            orbit_direction = orbit)
        }

        if (is.null(search_result)) {
          message(sprintf("    Aucun produit S1 %s pour %d", orbit, yr))
          next
        }

        scenes <- search_result$scenes
        n_sel <- min(nrow(scenes), max_scenes_par_annee)

        for (j in seq_len(n_sel)) {
          sc <- scenes[j, ]
          message(sprintf("    Scene %d/%d: %s", j, n_sel, sc$id))

          s1_orbit <- .download_s1_orbit(
            list(scenes = scenes[j, , drop = FALSE],
                 items = search_result$items),
            aoi, orbit)

          if (!is.null(s1_orbit)) {
            all_orbit_scenes[[length(all_orbit_scenes) + 1]] <- s1_orbit
            message(sprintf("    -> OK (%d bandes)", terra::nlyr(s1_orbit)))
          }
        }
      }

      if (length(all_orbit_scenes) > 0) {
        message(sprintf("  Total S1 %s: %d scenes", orbit,
                         length(all_orbit_scenes)))
        result[[orbit_key]] <- calculer_composite_median(all_orbit_scenes)
        tryCatch({
          terra::writeRaster(result[[orbit_key]], orbit_path, overwrite = TRUE,
                             gdal = c("COMPRESS=LZW"))
          message(sprintf("  S1 %s composite sauvegarde: %s", orbit,
                           orbit_path))
        }, error = function(e) {
          message(sprintf("  [WARN] Echec sauvegarde S1 %s: %s", orbit,
                           e$message))
        })
      }
    }

  } else {
    # ---- Mode mono-date (comportement original) ----
    if (is.null(date_cible)) {
      annee <- format(Sys.Date(), "%Y")
      start_date <- paste0(annee, "-06-01")
      end_date <- paste0(annee, "-09-30")
    } else {
      d <- as.Date(date_cible)
      start_date <- as.character(d - 30)
      end_date <- as.character(d + 30)
    }

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
