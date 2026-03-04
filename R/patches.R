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

#' Extraire les patches multi-modaux depuis plusieurs SpatRasters
#'
#' Pour chaque patch de la grille, extrait les valeurs de chaque modalite
#' (aerial, dem, etc.) separement. Les donnees sont structurees pour etre
#' passees au modele MAESTRO multi-modal.
#'
#' @param modalites Liste nommee de SpatRasters (ex: `list(aerial=..., dem=...)`)
#' @param grille sf grille de patches (issue de [creer_grille_patches()])
#' @param taille_pixels Taille cible de chaque patch en pixels (defaut: 250)
#' @return Liste de listes nommees, chaque element contient les matrices
#'   (H*W x C) pour chaque modalite. Ex: `patches[[i]]$aerial`, `patches[[i]]$dem`
#' @export
extraire_patches_multimodal <- function(modalites, grille, taille_pixels = 250) {
  message("=== Extraction des patches multi-modaux ===")
  message(sprintf("  Modalites: %s", paste(names(modalites), collapse = ", ")))

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
        n_bands <- terra::nlyr(modalites[[mod_name]])
        patch_data[[mod_name]] <- matrix(NA_real_,
                                          nrow = taille_pixels * taille_pixels,
                                          ncol = n_bands)
      }
      patches[[i]] <- patch_data
      skipped <- skipped + 1L
      next
    }

    patch_data <- list()
    for (mod_name in names(modalites)) {
      r <- modalites[[mod_name]]

      crop_result <- tryCatch(
        terra::crop(r, ext_patch),
        error = function(e) NULL
      )

      if (is.null(crop_result)) {
        n_bands <- terra::nlyr(r)
        patch_data[[mod_name]] <- matrix(NA_real_,
                                          nrow = taille_pixels * taille_pixels,
                                          ncol = n_bands)
        next
      }

      if (terra::ncol(crop_result) != taille_pixels ||
          terra::nrow(crop_result) != taille_pixels) {
        template <- terra::rast(
          ext = ext_patch,
          nrows = taille_pixels, ncols = taille_pixels,
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
