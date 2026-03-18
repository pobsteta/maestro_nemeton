# =============================================================================
# Telechargement et rasterisation de la BD Foret V2 (IGN)
# Source : WFS Geoplateforme via happign
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


# --- Telechargement BD Foret V2 via WFS direct ---

#' Telecharger une tuile BD Foret V2 via requete WFS directe
#'
#' Requete WFS GetFeature directe vers la Geoplateforme IGN (sans happign).
#' Plus fiable que happign pour les grandes bbox.
#'
#' @param bbox_vals Vecteur numerique c(xmin, ymin, xmax, ymax) en WGS84
#' @param typename Nom de la couche WFS
#' @param max_features Nombre max de features par requete (defaut: 10000)
#' @return sf data.frame ou NULL
#' @keywords internal
.wfs_get_bdforet <- function(bbox_vals, typename, max_features = 10000) {
  base_url <- "https://data.geopf.fr/wfs/ows"
  bbox_str <- paste(bbox_vals, collapse = ",")

  wfs_url <- paste0(
    base_url,
    "?SERVICE=WFS",
    "&VERSION=2.0.0",
    "&REQUEST=GetFeature",
    "&TYPENAME=", typename,
    "&BBOX=", bbox_str, ",EPSG:4326",
    "&SRSNAME=EPSG:4326",
    "&OUTPUTFORMAT=application/json",
    "&COUNT=", max_features
  )

  h <- curl::new_handle()
  curl::handle_setopt(h, timeout = 120)
  resp <- curl::curl_fetch_memory(wfs_url, handle = h)

  if (resp$status_code != 200) {
    warning(sprintf("WFS HTTP %d pour %s", resp$status_code, typename))
    return(NULL)
  }

  geojson <- rawToChar(resp$content)
  if (nchar(geojson) < 50) return(NULL)

  result <- sf::st_read(geojson, quiet = TRUE)
  if (nrow(result) == 0) return(NULL)
  result
}


#' Telecharger la BD Foret V2 pour une AOI
#'
#' Telecharge les polygones de la BD Foret V2 depuis le service WFS de la
#' Geoplateforme IGN via requete directe. Les polygones sont reprojectes
#' en Lambert-93 et les codes TFV sont convertis en classes NDP0.
#'
#' Pour les grandes AOI (> 0.5 degre), la bbox est decoupee en tuiles.
#'
#' @param aoi sf object (AOI en Lambert-93 ou WGS84)
#' @param output_dir Repertoire de sortie
#' @param layer_name Nom de la couche WFS BD Foret
#' @return sf data.frame avec les polygones BD Foret et la colonne `code_ndp0`
#' @export
download_bdforet_for_aoi <- function(aoi, output_dir,
                                      layer_name = "LANDCOVER.FORESTINVENTORY.V2:formation_vegetale") {
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

  # Extraire les valeurs numeriques sans noms
  bxmin <- unname(bbox_wgs84["xmin"])
  bymin <- unname(bbox_wgs84["ymin"])
  bxmax <- unname(bbox_wgs84["xmax"])
  bymax <- unname(bbox_wgs84["ymax"])

  # Decoupe spatiale : si la bbox depasse ~0.5 degre dans une dimension,
  # on la fractionne en tuiles pour eviter les timeouts du WFS
  bbox_width <- bxmax - bxmin
  bbox_height <- bymax - bymin
  tile_size <- 0.5  # degres

  x_breaks <- seq(bxmin, bxmax, by = tile_size)
  if (tail(x_breaks, 1) < bxmax) x_breaks <- c(x_breaks, bxmax)
  y_breaks <- seq(bymin, bymax, by = tile_size)
  if (tail(y_breaks, 1) < bymax) y_breaks <- c(y_breaks, bymax)

  tiles <- list()
  for (i in seq_len(length(x_breaks) - 1)) {
    for (j in seq_len(length(y_breaks) - 1)) {
      tiles <- c(tiles, list(c(x_breaks[i], y_breaks[j],
                                x_breaks[i + 1], y_breaks[j + 1])))
    }
  }

  n_tiles <- length(tiles)
  if (n_tiles > 1) {
    message(sprintf("  Bbox large (%.2f x %.2f deg) -> decoupe en %d tuiles",
                     bbox_width, bbox_height, n_tiles))
  }

  message(sprintf("  Couche WFS: %s", layer_name))
  message(sprintf("  Bbox: %.4f, %.4f, %.4f, %.4f (WGS84)",
                   bxmin, bymin, bxmax, bymax))

  # Couches a essayer, dans l'ordre
  all_layers <- unique(c(
    layer_name,
    "LANDCOVER.FORESTINVENTORY.V2:formation_vegetale",
    "BDFORET_V2:formation_vegetale"
  ))

  # Telecharger via requete WFS directe avec gestion des tuiles
  bdforet <- NULL
  for (lyr in all_layers) {
    message(sprintf("  Essai couche: %s", lyr))
    tile_results <- list()
    for (k in seq_along(tiles)) {
      tile <- tiles[[k]]
      if (n_tiles > 1) {
        message(sprintf("  Tuile %d/%d [%.4f,%.4f - %.4f,%.4f]",
                         k, n_tiles, tile[1], tile[2], tile[3], tile[4]))
      }
      tile_data <- tryCatch({
        .wfs_get_bdforet(tile, lyr)
      }, error = function(e) {
        message(sprintf("    Tuile %d echec: %s", k, e$message))
        NULL
      })
      if (!is.null(tile_data) && inherits(tile_data, "sf") && nrow(tile_data) > 0) {
        tile_results <- c(tile_results, list(tile_data))
      }
    }
    if (length(tile_results) > 0) {
      bdforet <- tryCatch(do.call(rbind, tile_results), error = function(e) {
        message(sprintf("  Erreur combinaison tuiles: %s", e$message))
        tile_results[[1]]
      })
    }
    if (!is.null(bdforet) && inherits(bdforet, "sf") && nrow(bdforet) > 0) {
      message(sprintf("  %d polygones telecharges via %s", nrow(bdforet), lyr))
      break
    }
    bdforet <- NULL
  }

  if (is.null(bdforet) || (inherits(bdforet, "sf") && nrow(bdforet) == 0)) {
    stop("Echec telechargement BD Foret via WFS.\n",
         "Verifiez votre connexion et le service WFS IGN (https://data.geopf.fr/wfs/ows).")
  }

  if (nrow(bdforet) == 0) {
    warning("Aucun polygone BD Foret V2 trouve pour cette AOI")
    return(NULL)
  }

  # Reprojeter en Lambert-93
  bdforet <- sf::st_transform(bdforet, 2154)

  # Reparer les geometries invalides (coordonnees NA, anneaux non fermes, etc.)
  bdforet <- sf::st_make_valid(bdforet)
  valid_mask <- !sf::st_is_empty(bdforet)
  if (any(!valid_mask)) {
    message(sprintf("  %d geometries invalides supprimees", sum(!valid_mask)))
    bdforet <- bdforet[valid_mask, ]
  }
  if (nrow(bdforet) == 0) {
    warning("Aucun polygone BD Foret valide apres nettoyage")
    return(NULL)
  }

  # Clipper sur l'AOI (s'assurer que les CRS correspondent)
  aoi_clip <- sf::st_transform(aoi, sf::st_crs(bdforet))
  bdforet <- sf::st_intersection(bdforet, sf::st_union(aoi_clip))

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
            "Colonnes disponibles: ", paste(names(bdforet), collapse = ", "),
            "\n  Premiers valeurs par colonne:")
    for (col in setdiff(names(bdforet), "geometry")) {
      vals <- head(unique(as.character(bdforet[[col]])), 5)
      warning(sprintf("    %s: %s", col, paste(vals, collapse = ", ")))
    }
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
#' Fonction combinee : telecharge les polygones via happign puis rasterise
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


# =============================================================================
# Labellisation des patches FLAIR-HUB avec BD Foret V2
# =============================================================================

#' Labelliser les patches FLAIR-HUB avec la BD Foret V2
#'
#' Pour chaque patch aerial FLAIR-HUB, telecharge les polygones BD Foret V2
#' couvrant l'emprise du patch via WFS, et rasterise les codes NDP0 a la
#' resolution du patch (0.2m, 250x250 px). Les labels sont sauvegardes dans
#' un dossier `labels_ndp0/` parallele a `aerial/`.
#'
#' Les requetes WFS sont groupees par domaine (D001_2019, etc.) pour limiter
#' le nombre d'appels au service.
#'
#' @param flair_dir Repertoire racine des donnees FLAIR-HUB
#'   (ex: `"data/flair_hub"`)
#' @param domaines Vecteur de domaines a traiter (`NULL` = tous les domaines
#'   trouves). Ex: `c("D001_2019", "D013_2020")`
#' @param overwrite Recalculer les labels existants (defaut: FALSE)
#' @return data.frame avec les statistiques par domaine (n_patches, n_forest,
#'   pct_forest)
#' @export
labelliser_flair_bdforet <- function(flair_dir = "data/flair_hub",
                                      domaines = NULL,
                                      overwrite = FALSE) {
  message("=== Labellisation FLAIR-HUB avec BD Foret V2 (NDP0) ===")

  aerial_dir <- file.path(flair_dir, "aerial")
  if (!dir.exists(aerial_dir)) {
    stop("Dossier aerial/ introuvable: ", aerial_dir)
  }

  # Detecter les domaines disponibles
  if (is.null(domaines)) {
    domaines <- list.dirs(aerial_dir, recursive = FALSE, full.names = FALSE)
    if (length(domaines) == 0) {
      # Pas de sous-dossiers domaine, les TIF sont directement dans aerial/
      domaines <- ""
    }
  }
  message(sprintf("  %d domaine(s) a traiter", length(domaines)))

  stats <- data.frame(
    domaine = character(0),
    n_patches = integer(0),
    n_labelled = integer(0),
    n_forest = integer(0),
    stringsAsFactors = FALSE
  )

  for (dom in domaines) {
    # Trouver les patches aerial de ce domaine
    if (nchar(dom) > 0) {
      patch_dir <- file.path(aerial_dir, dom)
      label_dir <- file.path(flair_dir, "labels_ndp0", dom)
    } else {
      patch_dir <- aerial_dir
      label_dir <- file.path(flair_dir, "labels_ndp0")
    }

    tif_files <- list.files(patch_dir, pattern = "\\.tif$", full.names = TRUE,
                            recursive = TRUE)
    if (length(tif_files) == 0) next

    dir.create(label_dir, recursive = TRUE, showWarnings = FALSE)

    dom_label <- if (nchar(dom) > 0) dom else "(racine)"
    message(sprintf("\n--- Domaine %s : %d patches ---", dom_label, length(tif_files)))

    # Calculer la bbox englobante de tous les patches du domaine
    # pour faire UNE SEULE requete WFS par domaine
    bbox_all <- NULL
    valid_tifs <- character(0)
    for (tif in tif_files) {
      r <- tryCatch(terra::rast(tif), error = function(e) NULL)
      if (is.null(r)) next
      e <- terra::ext(r)
      ext_vals <- c(e[1], e[2], e[3], e[4])
      if (anyNA(ext_vals) || any(!is.finite(ext_vals))) {
        warning(sprintf("  Patch ignore (extent invalide): %s", basename(tif)))
        next
      }
      valid_tifs <- c(valid_tifs, tif)
      if (is.null(bbox_all)) {
        bbox_all <- e
      } else {
        bbox_all <- terra::union(bbox_all, e)
      }
    }
    tif_files <- valid_tifs
    if (length(tif_files) == 0 || is.null(bbox_all)) {
      warning(sprintf("  Domaine %s: aucun patch valide, ignore", dom_label))
      next
    }

    # Creer un sf pour la bbox englobante du domaine
    # NB: utiliser les accesseurs explicites terra::xmin() etc.
    # pour eviter toute ambiguite d'ordre avec as.vector()
    crs_str <- terra::crs(terra::rast(tif_files[1]))
    bbox_vals <- c(
      xmin = terra::xmin(bbox_all),
      ymin = terra::ymin(bbox_all),
      xmax = terra::xmax(bbox_all),
      ymax = terra::ymax(bbox_all)
    )
    if (anyNA(bbox_vals) || any(!is.finite(bbox_vals))) {
      warning(sprintf("  Domaine %s: bbox invalide apres union, ignore", dom_label))
      next
    }
    message(sprintf("  Bbox domaine: xmin=%.0f ymin=%.0f xmax=%.0f ymax=%.0f",
                     bbox_vals["xmin"], bbox_vals["ymin"],
                     bbox_vals["xmax"], bbox_vals["ymax"]))
    bbox_poly <- sf::st_as_sfc(sf::st_bbox(bbox_vals, crs = sf::st_crs(crs_str)))
    aoi_domaine <- sf::st_sf(geometry = bbox_poly)

    # Telecharger la BD Foret V2 pour tout le domaine
    cache_dir <- file.path(flair_dir, ".cache_bdforet")
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    cache_path <- file.path(cache_dir, paste0("bdforet_", dom, ".gpkg"))

    bdforet <- NULL
    if (file.exists(cache_path)) {
      message("  Cache BD Foret: ", cache_path)
      bdforet <- sf::st_read(cache_path, quiet = TRUE)
      # Rejeter les caches vides ou sans foret (echec precedent)
      if (nrow(bdforet) == 0 || !("code_ndp0" %in% names(bdforet)) ||
          all(bdforet$code_ndp0 == 9L)) {
        warning(sprintf("  Cache invalide (0 polygones forestiers): %s -> re-telechargement",
                         cache_path))
        file.remove(cache_path)
        bdforet <- NULL
      }
    }
    if (is.null(bdforet)) {
      bdforet <- tryCatch({
        download_bdforet_for_aoi(aoi_domaine, cache_dir)
      }, error = function(e) {
        warning(sprintf("  Echec WFS pour domaine %s: %s", dom_label, e$message))
        NULL
      })
      if (!is.null(bdforet)) {
        sf::st_write(bdforet, cache_path, delete_dsn = TRUE, quiet = TRUE)
      }
    }

    # Diagnostic : afficher l'etat de bdforet
    if (is.null(bdforet) || nrow(bdforet) == 0) {
      warning(sprintf("  Domaine %s: BD Foret vide ou NULL -> tous les patches seront non-foret", dom_label))
    } else {
      message(sprintf("  BD Foret chargee: %d polygones, CRS=%s",
                       nrow(bdforet), sf::st_crs(bdforet)$input))
      # Verifier que les CRS correspondent
      patch_crs <- sf::st_crs(sf::st_crs(crs_str))
      bdforet_crs <- sf::st_crs(bdforet)
      if (!identical(patch_crs$wkt, bdforet_crs$wkt)) {
        message("  [DIAG] CRS patches vs bdforet different, transformation automatique dans l'intersection")
        bdforet <- sf::st_transform(bdforet, patch_crs)
      }
    }

    # Rasteriser patch par patch
    n_labelled <- 0L
    n_forest <- 0L
    n_intersect_ok <- 0L

    for (tif in tif_files) {
      patch_name <- tools::file_path_sans_ext(basename(tif))
      label_path <- file.path(label_dir, paste0(patch_name, ".tif"))

      if (file.exists(label_path) && !overwrite) {
        n_labelled <- n_labelled + 1L
        # Compter la foret dans le label existant
        lbl <- terra::rast(label_path)
        vals <- terra::values(lbl)
        if (sum(vals < 9, na.rm = TRUE) / length(vals) > 0.05) {
          n_forest <- n_forest + 1L
        }
        next
      }

      # Lire l'emprise du patch aerial
      ref <- terra::rast(tif)
      ext_patch <- terra::ext(ref)

      # Creer le raster label (250x250 px, 0.2m)
      label_rast <- terra::rast(
        xmin = ext_patch[1], xmax = ext_patch[2],
        ymin = ext_patch[3], ymax = ext_patch[4],
        nrows = terra::nrow(ref), ncols = terra::ncol(ref),
        crs = terra::crs(ref)
      )
      terra::values(label_rast) <- 9L  # Non-foret par defaut

      if (!is.null(bdforet) && nrow(bdforet) > 0) {
        # Clipper les polygones BD Foret sur l'emprise du patch
        patch_bbox <- sf::st_as_sfc(sf::st_bbox(c(
          xmin = ext_patch[1], xmax = ext_patch[2],
          ymin = ext_patch[3], ymax = ext_patch[4]
        ), crs = sf::st_crs(terra::crs(ref))))

        patch_bdforet <- tryCatch({
          sf::st_intersection(bdforet, patch_bbox)
        }, error = function(e) NULL)

        if (!is.null(patch_bdforet) && nrow(patch_bdforet) > 0) {
          n_intersect_ok <- n_intersect_ok + 1L
          # Rasteriser les polygones par code NDP0
          bdforet_vect <- terra::vect(patch_bdforet)
          codes <- sort(unique(patch_bdforet$code_ndp0), decreasing = TRUE)
          for (code in codes) {
            mask_poly <- bdforet_vect[bdforet_vect$code_ndp0 == code, ]
            if (length(mask_poly) == 0) next
            layer <- terra::rasterize(mask_poly, label_rast, field = "code_ndp0")
            valid <- !is.na(terra::values(layer))
            vals <- terra::values(label_rast)
            vals[valid] <- terra::values(layer)[valid]
            terra::values(label_rast) <- vals
          }
        }
      }

      names(label_rast) <- "classe_ndp0"
      terra::writeRaster(label_rast, label_path, overwrite = TRUE,
                         datatype = "INT1U", gdal = c("COMPRESS=LZW"))

      n_labelled <- n_labelled + 1L
      vals <- terra::values(label_rast)
      if (sum(vals < 9, na.rm = TRUE) / length(vals) > 0.05) {
        n_forest <- n_forest + 1L
      }

      if (n_labelled %% 50 == 0) {
        message(sprintf("    %d / %d patches labellises", n_labelled, length(tif_files)))
      }
    }

    pct <- if (n_labelled > 0) round(n_forest / n_labelled * 100, 1) else 0
    message(sprintf("  Domaine %s: %d labellises, %d forestiers (%.1f%%), %d intersections BD Foret non-vides",
                     dom_label, n_labelled, n_forest, pct, n_intersect_ok))

    stats <- rbind(stats, data.frame(
      domaine = dom_label,
      n_patches = length(tif_files),
      n_labelled = n_labelled,
      n_forest = n_forest,
      stringsAsFactors = FALSE
    ))
  }

  message(sprintf("\n=== Total: %d patches labellises, %d forestiers ===",
                   sum(stats$n_labelled), sum(stats$n_forest)))

  invisible(stats)
}
