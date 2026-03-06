# =============================================================================
# Indices spectraux pour l'analyse de la vegetation
# =============================================================================

#' Calculer le NDVI (Normalized Difference Vegetation Index)
#'
#' Calcule le NDVI a partir d'un raster RGBI ou de bandes PIR et Rouge separees.
#' NDVI = (PIR - Rouge) / (PIR + Rouge)
#'
#' @param rgbi SpatRaster avec bandes R, G, B, NIR (ou 2 bandes PIR, Rouge)
#' @param nir_band Indice de la bande PIR (defaut: 4 pour RGBI)
#' @param red_band Indice de la bande Rouge (defaut: 1 pour RGBI)
#' @return SpatRaster mono-bande avec valeurs NDVI [-1, 1]
#' @export
#' @examples
#' \dontrun{
#' rgbi <- terra::rast("ortho_rgbi.tif")
#' ndvi <- compute_ndvi(rgbi)
#' terra::plot(ndvi)
#' }
compute_ndvi <- function(rgbi, nir_band = 4L, red_band = 1L) {
  pir <- rgbi[[nir_band]]
  rouge <- rgbi[[red_band]]
  ndvi <- (pir - rouge) / (pir + rouge + 1e-8)
  names(ndvi) <- "NDVI"
  ndvi
}

#' Calculer le GNDVI (Green Normalized Difference Vegetation Index)
#'
#' GNDVI = (PIR - Vert) / (PIR + Vert)
#'
#' @param rgbi SpatRaster avec bandes R, G, B, NIR
#' @param nir_band Indice de la bande PIR (defaut: 4)
#' @param green_band Indice de la bande Verte (defaut: 2)
#' @return SpatRaster mono-bande avec valeurs GNDVI [-1, 1]
#' @export
compute_gndvi <- function(rgbi, nir_band = 4L, green_band = 2L) {
  pir <- rgbi[[nir_band]]
  vert <- rgbi[[green_band]]
  gndvi <- (pir - vert) / (pir + vert + 1e-8)
  names(gndvi) <- "GNDVI"
  gndvi
}

#' Calculer le SAVI (Soil Adjusted Vegetation Index)
#'
#' SAVI = ((PIR - Rouge) / (PIR + Rouge + L)) * (1 + L)
#'
#' @param rgbi SpatRaster avec bandes R, G, B, NIR
#' @param nir_band Indice de la bande PIR (defaut: 4)
#' @param red_band Indice de la bande Rouge (defaut: 1)
#' @param L Facteur d'ajustement du sol (defaut: 0.5)
#' @return SpatRaster mono-bande avec valeurs SAVI
#' @export
compute_savi <- function(rgbi, nir_band = 4L, red_band = 1L, L = 0.5) {
  pir <- rgbi[[nir_band]]
  rouge <- rgbi[[red_band]]
  savi <- ((pir - rouge) / (pir + rouge + L + 1e-8)) * (1 + L)
  names(savi) <- "SAVI"
  savi
}

#' Creer un masque de vegetation par seuillage NDVI
#'
#' @param rgbi SpatRaster avec bandes R, G, B, NIR
#' @param seuil Seuil NDVI pour la vegetation (defaut: 0.3)
#' @param nir_band Indice de la bande PIR (defaut: 4)
#' @param red_band Indice de la bande Rouge (defaut: 1)
#' @return SpatRaster binaire (1 = vegetation, 0 = non-vegetation)
#' @export
mask_vegetation <- function(rgbi, seuil = 0.3, nir_band = 4L, red_band = 1L) {
  ndvi <- compute_ndvi(rgbi, nir_band, red_band)
  mask <- ndvi >= seuil
  names(mask) <- "vegetation"
  mask
}
