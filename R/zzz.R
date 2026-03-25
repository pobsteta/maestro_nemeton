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

#' @importFrom utils write.csv
NULL

.onLoad <- function(libname, pkgname) {
  # Eviter le crash OpenMP sur Windows : torch et numpy livrent chacun
  # libiomp5md.dll, le conflit tue le process si la variable n'est pas
  # positionnee AVANT le chargement des DLL.
  Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE")

  # Configurer RETICULATE_PYTHON AVANT que reticulate ne s'initialise,

  # sinon il s'accroche au premier Python trouve (uv/reticulate) et on
  # ne peut plus basculer vers l'env conda maestro.
  if (nchar(Sys.getenv("RETICULATE_PYTHON")) == 0) {
    py_path <- .find_maestro_python()
    if (!is.null(py_path)) {
      Sys.setenv(RETICULATE_PYTHON = py_path)
    }
  }
}

# Chercher le Python de l'env conda maestro sans charger reticulate
.find_maestro_python <- function(envname = "maestro") {
  conda_dirs <- if (.Platform$OS.type == "windows") {
    home <- Sys.getenv("USERPROFILE", Sys.getenv("HOME"))
    c(file.path(home, "miniforge3"),
      file.path(home, "mambaforge"),
      file.path(home, "miniconda3"),
      file.path(home, "anaconda3"),
      file.path(Sys.getenv("LOCALAPPDATA"), "miniforge3"),
      file.path(Sys.getenv("PROGRAMDATA"), "miniforge3"))
  } else {
    home <- Sys.getenv("HOME")
    c(file.path(home, "miniforge3"),
      file.path(home, "mambaforge"),
      file.path(home, "miniconda3"),
      file.path(home, "anaconda3"),
      "/opt/miniforge3",
      "/opt/miniconda3")
  }

  for (conda_root in conda_dirs) {
    py_path <- if (.Platform$OS.type == "windows") {
      file.path(conda_root, "envs", envname, "python.exe")
    } else {
      file.path(conda_root, "envs", envname, "bin", "python")
    }
    if (file.exists(py_path)) return(py_path)
  }
  NULL
}
