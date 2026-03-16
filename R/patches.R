#' Creer une grille de patches pour l'inference
#'
#' Genere une grille reguliere de patches carres couvrant l'AOI.
#' Seuls les patches qui intersectent l'AOI sont conserves.
#'
#' @param aoi sf object en Lambert-93
#' @param taille_patch_m Taille des patches en metres (defaut: 50)
#' @return sf data.frame (grille de patches avec colonne `id`)
#' @export
creer_grille_patches <- function(aoi, taille_patch_m = 50) {
  message("=== Creation de la grille de patches ===")

  bbox <- sf::st_bbox(aoi)

  x_coords <- seq(bbox["xmin"], bbox["xmax"], by = taille_patch_m)
  y_coords <- seq(bbox["ymin"], bbox["ymax"], by = taille_patch_m)

  patches <- expand.grid(x = x_coords, y = y_coords)
  patches$xmax <- patches$x + taille_patch_m
  patches$ymax <- patches$y + taille_patch_m

  creer_poly <- function(i) {
    sf::st_polygon(list(matrix(c(
      patches$x[i],    patches$y[i],
      patches$xmax[i], patches$y[i],
      patches$xmax[i], patches$ymax[i],
      patches$x[i],    patches$ymax[i],
      patches$x[i],    patches$y[i]
    ), ncol = 2, byrow = TRUE)))
  }

  geometries <- lapply(seq_len(nrow(patches)), creer_poly)
  grille <- sf::st_sf(
    id = seq_len(nrow(patches)),
    geometry = sf::st_sfc(geometries, crs = sf::st_crs(aoi))
  )

  intersects <- sf::st_intersects(grille, sf::st_union(aoi), sparse = FALSE)[, 1]
  grille <- grille[intersects, ]
  grille$id <- seq_len(nrow(grille))

  message(sprintf("  Patches generes : %d", nrow(grille)))
  message(sprintf("  Taille patch : %.0f m x %.0f m", taille_patch_m, taille_patch_m))

  grille
}

#' Extraire les patches raster depuis un SpatRaster
#'
#' Decoupe un raster multi-bandes selon une grille de patches et
#' retourne les valeurs sous forme de matrices pretes pour Python/NumPy.
#'
#' @param r SpatRaster multi-bandes
#' @param grille sf grille de patches (issue de [creer_grille_patches()])
#' @param taille_pixels Taille cible de chaque patch en pixels (defaut: 250)
#' @return Liste de matrices (H*W x C), une par patch
#' @export
extraire_patches_raster <- function(r, grille, taille_pixels = 250) {
  message("=== Extraction des patches raster ===")
  message(sprintf("  Raster : %d bandes, %d x %d px",
                   terra::nlyr(r), terra::ncol(r), terra::nrow(r)))

  patches_data <- list()
  ext_raster <- terra::ext(r)
  skipped <- 0L

  for (i in seq_len(nrow(grille))) {
    ext_patch <- terra::ext(sf::st_bbox(grille[i, ]))

    # Verifier que le patch chevauche le raster
    overlap <- !(ext_patch$xmin >= ext_raster$xmax ||
                 ext_patch$xmax <= ext_raster$xmin ||
                 ext_patch$ymin >= ext_raster$ymax ||
                 ext_patch$ymax <= ext_raster$ymin)

    if (!overlap) {
      # Patch vide (hors raster) : remplir de NA
      patches_data[[i]] <- matrix(NA_real_,
                                   nrow = taille_pixels * taille_pixels,
                                   ncol = terra::nlyr(r))
      skipped <- skipped + 1L
      next
    }

    patch <- tryCatch(
      terra::crop(r, ext_patch),
      error = function(e) NULL
    )

    if (is.null(patch)) {
      patches_data[[i]] <- matrix(NA_real_,
                                   nrow = taille_pixels * taille_pixels,
                                   ncol = terra::nlyr(r))
      skipped <- skipped + 1L
      next
    }

    if (terra::ncol(patch) != taille_pixels || terra::nrow(patch) != taille_pixels) {
      template <- terra::rast(
        ext = ext_patch,
        nrows = taille_pixels, ncols = taille_pixels,
        crs = terra::crs(r),
        nlyrs = terra::nlyr(r)
      )
      patch <- terra::resample(patch, template, method = "bilinear")
    }

    patches_data[[i]] <- terra::values(patch)

    if (i %% 100 == 0 || i == nrow(grille)) {
      message(sprintf("  Patches extraits : %d / %d", i, nrow(grille)))
    }
  }

  if (skipped > 0L) {
    message(sprintf("  Patches hors emprise raster (ignores) : %d", skipped))
  }
  message(sprintf("  Total patches extraits : %d", length(patches_data)))
  patches_data
}

#' Taille de patch par modalite MAESTRO
#'
#' Retourne la taille de patch en pixels attendue par le modele MAESTRO
#' pour chaque modalite. Les modalites a haute resolution (aerial, dem)
#' utilisent 250 pixels, tandis que les modalites Sentinel (10m) utilisent
#' 5 pixels pour couvrir la meme emprise de 50m.
#'
#' @param mod_name Nom de la modalite
#' @param taille_pixels_ref Taille de reference (defaut: 250 pour aerial)
#' @return Entier : nombre de pixels pour ce patch
#' @export
taille_patch_modalite <- function(mod_name, taille_pixels_ref = 250L) {
  # Modalites a 10m de resolution : 50m / 10m = 5 pixels
  sentinel_mods <- c("s2", "s1_asc", "s1_des")
  if (mod_name %in% sentinel_mods) {
    return(5L)
  }
  # aerial et dem : taille de reference (250px a 0.2m)
  as.integer(taille_pixels_ref)
}

#' Extraire les patches multi-modaux depuis plusieurs SpatRasters
#'
#' Pour chaque patch de la grille, extrait les valeurs de chaque modalite
#' (aerial, dem, s2, s1_asc, s1_des) separement. Chaque modalite est
#' reechantillonnee a sa taille de patch native :
#'   - aerial, dem : 250 x 250 pixels (0.2m)
#'   - s2, s1_asc, s1_des : 5 x 5 pixels (10m)
#'
#' Les donnees sont structurees pour etre passees au modele MAESTRO.
#'
#' @param modalites Liste nommee de SpatRasters (ex: `list(aerial=..., dem=..., s2=...)`)
#' @param grille sf grille de patches (issue de [creer_grille_patches()])
#' @param taille_pixels Taille cible pour aerial/dem en pixels (defaut: 250)
#' @return Liste de listes nommees, chaque element contient les matrices
#'   (H*W x C) pour chaque modalite. Ex: `patches[[i]]$aerial`, `patches[[i]]$s2`
#' @export
extraire_patches_multimodal <- function(modalites, grille, taille_pixels = 250) {
  message("=== Extraction des patches multi-modaux ===")
  message(sprintf("  Modalites: %s", paste(names(modalites), collapse = ", ")))

  # Validation CRS : toutes les modalites doivent avoir le meme CRS
  if (length(modalites) > 1) {
    ref_crs <- terra::crs(modalites[[1]], proj = TRUE)
    for (mod_name in names(modalites)[-1]) {
      mod_crs <- terra::crs(modalites[[mod_name]], proj = TRUE)
      if (mod_crs != ref_crs) {
        stop(sprintf(
          "CRS incoherent: '%s' a CRS='%s' mais '%s' a CRS='%s'. Reprojetez d'abord.",
          names(modalites)[1], ref_crs, mod_name, mod_crs))
      }
    }
  }

  # Afficher la taille de patch pour chaque modalite
  for (mod_name in names(modalites)) {
    tp <- taille_patch_modalite(mod_name, taille_pixels)
    message(sprintf("    %s: %d x %d px (%d bandes)",
                     mod_name, tp, tp, terra::nlyr(modalites[[mod_name]])))
  }

  n_patches <- nrow(grille)
  patches <- vector("list", n_patches)
  skipped <- 0L

  # Utiliser la premiere modalite comme reference pour les emprises
  ref_raster <- modalites[[1]]
  ext_raster <- terra::ext(ref_raster)

  for (i in seq_len(n_patches)) {
    ext_patch <- terra::ext(sf::st_bbox(grille[i, ]))

    # Verifier que le patch chevauche le raster
    overlap <- !(ext_patch$xmin >= ext_raster$xmax ||
                 ext_patch$xmax <= ext_raster$xmin ||
                 ext_patch$ymin >= ext_raster$ymax ||
                 ext_patch$ymax <= ext_raster$ymin)

    if (!overlap) {
      patch_data <- list()
      for (mod_name in names(modalites)) {
        tp <- taille_patch_modalite(mod_name, taille_pixels)
        n_bands <- terra::nlyr(modalites[[mod_name]])
        patch_data[[mod_name]] <- matrix(NA_real_,
                                          nrow = tp * tp,
                                          ncol = n_bands)
      }
      patches[[i]] <- patch_data
      skipped <- skipped + 1L
      next
    }

    patch_data <- list()
    for (mod_name in names(modalites)) {
      r <- modalites[[mod_name]]
      tp <- taille_patch_modalite(mod_name, taille_pixels)

      crop_result <- tryCatch(
        terra::crop(r, ext_patch),
        error = function(e) NULL
      )

      if (is.null(crop_result)) {
        n_bands <- terra::nlyr(r)
        patch_data[[mod_name]] <- matrix(NA_real_,
                                          nrow = tp * tp,
                                          ncol = n_bands)
        next
      }

      # Reechantillonner a la taille de patch de cette modalite
      if (terra::ncol(crop_result) != tp ||
          terra::nrow(crop_result) != tp) {
        template <- terra::rast(
          ext = ext_patch,
          nrows = tp, ncols = tp,
          crs = terra::crs(r),
          nlyrs = terra::nlyr(r)
        )
        crop_result <- terra::resample(crop_result, template, method = "bilinear")
      }

      patch_data[[mod_name]] <- terra::values(crop_result)
    }
    patches[[i]] <- patch_data

    if (i %% 100 == 0 || i == n_patches) {
      message(sprintf("  Patches extraits : %d / %d", i, n_patches))
    }
  }

  if (skipped > 0L) {
    message(sprintf("  Patches hors emprise raster (ignores) : %d", skipped))
  }
  message(sprintf("  Total patches extraits : %d", length(patches)))
  patches
}
