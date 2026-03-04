# =============================================================================
# Package-level configuration and constants
# =============================================================================

# --- IGN Geoplateforme WMS-R ---
.ign_config <- new.env(parent = emptyenv())
.ign_config$WMS_URL      <- "https://data.geopf.fr/wms-r"
.ign_config$LAYER_ORTHO  <- "ORTHOIMAGERY.ORTHOPHOTOS"
.ign_config$LAYER_IRC    <- "ORTHOIMAGERY.ORTHOPHOTOS.IRC"
.ign_config$LAYER_MNT    <- "ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES"
.ign_config$LAYER_MNS    <- "IGNF_LIDAR-HD_MNS_ELEVATION.ELEVATIONGRIDCOVERAGE.LAMB93"
.ign_config$WMS_MAX_PX   <- 4096L
.ign_config$RES_IGN      <- 0.2
.ign_config$RES_DEM      <- 1.0

#' @importFrom utils read.csv write.csv
NULL

.onLoad <- function(libname, pkgname) {
  # Eviter le crash OpenMP sur Windows : torch et numpy livrent chacun

  # libiomp5md.dll, le conflit tue le process si la variable n'est pas
  # positionnee AVANT le chargement des DLL.
  Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE")
}
