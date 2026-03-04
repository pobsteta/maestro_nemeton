#' Combiner les ortho RVB et IRC en image 4 bandes RGBI
#'
#' Les modeles MAESTRO/FLAIR attendent 4 canaux optiques : Rouge, Vert, Bleu,
#' PIR. L'IRC IGN fournit le PIR en premiere bande.
#'
#' @param rvb SpatRaster ortho RVB (3 bandes : Rouge, Vert, Bleu)
#' @param irc SpatRaster ortho IRC (3 bandes : PIR, Rouge, Vert)
#' @return SpatRaster 4 bandes (Rouge, Vert, Bleu, PIR)
#' @export
combine_rvb_irc <- function(rvb, irc) {
  message("Combinaison RVB + PIR en image 4 bandes RGBI...")

  if (!terra::compareGeom(rvb, irc, stopOnError = FALSE)) {
    message("  Reechantillonnage IRC sur la grille RVB...")
    irc <- terra::resample(irc, rvb, method = "bilinear")
  }

  pir <- irc[[1]]
  names(pir) <- "PIR"

  rgbi <- c(rvb[[1]], rvb[[2]], rvb[[3]], pir)
  names(rgbi) <- c("Rouge", "Vert", "Bleu", "PIR")

  message(sprintf("  Image RGBI: %d x %d px, %d bandes",
                   terra::ncol(rgbi), terra::nrow(rgbi), terra::nlyr(rgbi)))
  rgbi
}

#' Combiner RGBI + MNT en image 5 bandes
#'
#' Ajoute le MNT comme 5eme bande a l'image RGBI. Le MNT est reechantillonne
#' sur la grille RGBI si necessaire.
#'
#' @param rgbi SpatRaster 4 bandes (Rouge, Vert, Bleu, PIR)
#' @param mnt SpatRaster 1 bande (MNT)
#' @return SpatRaster 5 bandes (Rouge, Vert, Bleu, PIR, MNT)
#' @export
combine_rgbi_mnt <- function(rgbi, mnt) {
  message("Ajout du MNT comme 5eme bande...")

  if (!terra::compareGeom(rgbi, mnt, stopOnError = FALSE)) {
    message("  Reechantillonnage MNT sur la grille RGBI...")
    mnt <- terra::resample(mnt, rgbi, method = "bilinear")
  }

  rgbi_mnt <- c(rgbi, mnt)
  names(rgbi_mnt) <- c("Rouge", "Vert", "Bleu", "PIR", "MNT")

  message(sprintf("  Image RGBI+MNT: %d x %d px, %d bandes",
                   terra::ncol(rgbi_mnt), terra::nrow(rgbi_mnt),
                   terra::nlyr(rgbi_mnt)))
  rgbi_mnt
}

#' Aligner le DEM 2 bandes (DSM+DTM) sur la grille RGBI
#'
#' Reechantillonne le DEM (2 bandes : DSM, DTM) sur la grille de l'image
#' RGBI aerienne. Utilisee pour preparer les entrees multi-modales MAESTRO.
#'
#' @param dem SpatRaster 2 bandes (DSM, DTM)
#' @param rgbi SpatRaster de reference (meme emprise/resolution)
#' @return SpatRaster 2 bandes (DSM, DTM) alignees sur la grille RGBI
#' @export
aligner_dem_sur_rgbi <- function(dem, rgbi) {
  if (!terra::compareGeom(dem, rgbi, stopOnError = FALSE)) {
    message("  Reechantillonnage DEM sur la grille RGBI...")
    dem <- terra::resample(dem, rgbi, method = "bilinear")
  }
  dem
}
