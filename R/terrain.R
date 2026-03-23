#' Calculer les derives morphologiques du MNT
#'
#' A partir d'un DTM (Modele Numerique de Terrain) a 1m de resolution,
#' calcule les derives topographiques utiles pour la segmentation forestiere :
#' pente, orientation (aspect), TPI (Topographic Position Index) et
#' TWI (Topographic Wetness Index).
#'
#' @param dtm SpatRaster mono-bande (DTM/MNT a 1m)
#' @return Liste nommee de SpatRasters mono-bande :
#'   `SLOPE`, `ASPECT`, `TPI`, `TWI`
#' @export
calculer_derives_terrain <- function(dtm) {
  message("=== Calcul des derives morphologiques du MNT ===")

  # --- Pente (degres) ---
  message("  Pente...")
  slope <- terra::terrain(dtm, v = "slope", unit = "degrees")
  names(slope) <- "SLOPE"


  # --- Orientation (aspect, degres 0-360, -1 = plat) ---
  message("  Orientation (aspect)...")
  aspect <- terra::terrain(dtm, v = "aspect", unit = "degrees")
  # Remplacer -1 (zones plates) par 0
  aspect[aspect < 0] <- 0
  names(aspect) <- "ASPECT"

  # --- TPI (Topographic Position Index) ---
  message("  TPI (Topographic Position Index)...")
  tpi <- terra::terrain(dtm, v = "TPI")
  names(tpi) <- "TPI"

  # --- TWI (Topographic Wetness Index) ---
  # TWI = ln(a / tan(slope)), ou 'a' est l'aire drainee amont

  # Approximation via pente locale : TWI ~ ln(cellsize / tan(slope_rad))
  # C'est une approximation car le vrai TWI necessite le flow accumulation,
  # mais c'est suffisant pour la segmentation et beaucoup plus rapide.
  message("  TWI (Topographic Wetness Index)...")
  slope_rad <- terra::terrain(dtm, v = "slope", unit = "radians")
  # Eviter tan(0) = 0 -> log(Inf)
  tan_slope <- tan(slope_rad)
  tan_slope[tan_slope < 0.001] <- 0.001
  res_m <- terra::res(dtm)[1]
  twi <- log(res_m / tan_slope)
  names(twi) <- "TWI"
  # Borner les valeurs extremes
  twi[twi < 0] <- 0
  twi[twi > 30] <- 30

  derives <- list(SLOPE = slope, ASPECT = aspect, TPI = tpi, TWI = twi)

  for (nm in names(derives)) {
    vals <- terra::values(derives[[nm]], na.rm = TRUE)
    if (length(vals) > 0 && any(is.finite(vals))) {
      message(sprintf("    %s: %.2f - %.2f", nm,
                       min(vals, na.rm = TRUE), max(vals, na.rm = TRUE)))
    }
  }

  derives
}


#' Assembler un DEM 2 bandes a partir des canaux choisis
#'
#' Selectionne 2 canaux parmi les sources disponibles (DSM, DTM, SLOPE,
#' ASPECT, TPI, TWI) et les empile en un SpatRaster 2 bandes pour MAESTRO.
#'
#' @param dsm SpatRaster mono-bande (DSM)
#' @param dtm SpatRaster mono-bande (DTM)
#' @param derives Liste de derives terrain (issue de [calculer_derives_terrain()])
#' @param dem_channels Vecteur de 2 noms de canaux parmi
#'   `c("DSM", "DTM", "SLOPE", "ASPECT", "TPI", "TWI")`
#' @return SpatRaster 2 bandes avec les canaux selectionnes
#' @export
assembler_dem_channels <- function(dsm, dtm, derives, dem_channels) {
  valid_channels <- c("DSM", "DTM", "SLOPE", "ASPECT", "TPI", "TWI")
  dem_channels <- toupper(dem_channels)

  if (length(dem_channels) != 2) {
    stop("dem_channels doit contenir exactement 2 noms de canaux, ex: c('SLOPE', 'TWI')")
  }
  unknown <- setdiff(dem_channels, valid_channels)
  if (length(unknown) > 0) {
    stop(sprintf("Canal(aux) DEM inconnu(s): %s. Valides: %s",
                 paste(unknown, collapse = ", "),
                 paste(valid_channels, collapse = ", ")))
  }

  # Construire la banque de canaux disponibles
  banque <- list(DSM = dsm, DTM = dtm)
  banque <- c(banque, derives)

  band1 <- banque[[dem_channels[1]]]
  band2 <- banque[[dem_channels[2]]]

  dem <- c(band1, band2)
  names(dem) <- dem_channels

  message(sprintf("  DEM assemble: %s + %s", dem_channels[1], dem_channels[2]))
  dem
}
