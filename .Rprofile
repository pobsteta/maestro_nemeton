# =============================================================================
# .Rprofile - Configuration automatique de l'environnement MAESTRO
# Charge au demarrage de RStudio dans ce projet
# =============================================================================

.First <- function() {
  # Eviter le conflit OpenMP sur Windows
  Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE")

  # Detecter le Python de l'environnement conda maestro
  envname <- "maestro"
  conda_dirs <- if (.Platform$OS.type == "windows") {
    home <- Sys.getenv("USERPROFILE", Sys.getenv("HOME"))
    c(file.path(home, "miniforge3"),
      file.path(home, "mambaforge"),
      file.path(home, "miniconda3"),
      file.path(home, "anaconda3"))
  } else {
    home <- Sys.getenv("HOME")
    c(file.path(home, "miniforge3"),
      file.path(home, "mambaforge"),
      file.path(home, "miniconda3"),
      file.path(home, "anaconda3"),
      "/opt/miniforge3", "/opt/miniconda3")
  }

  py_suffix <- if (.Platform$OS.type == "windows") {
    file.path("envs", envname, "python.exe")
  } else {
    file.path("envs", envname, "bin", "python")
  }

  for (d in conda_dirs) {
    py <- file.path(d, py_suffix)
    if (file.exists(py)) {
      Sys.setenv(RETICULATE_PYTHON = py)
      message("[maestro] Python: ", py)
      break
    }
  }

  # Verification au chargement de reticulate
  setHook(packageEvent("reticulate", "onLoad"), function(...) {
    tryCatch({
      cfg <- reticulate::py_config()
      message("[maestro] Python actif: ", cfg$python)
      message("[maestro] NumPy: ", if (reticulate::py_module_available("numpy")) "OK" else "MANQUANT")
      message("[maestro] Torch: ", if (reticulate::py_module_available("torch")) "OK" else "MANQUANT")
      message("[maestro] Safetensors: ", if (reticulate::py_module_available("safetensors")) "OK" else "MANQUANT")
    }, error = function(e) {
      message("[maestro] Attention: impossible de verifier Python - ", e$message)
    })
  })

  message("[maestro] Environnement configure. Token HF: ",
          if (nchar(Sys.getenv("HUGGING_FACE_HUB_TOKEN")) > 0) "OK" else "MANQUANT")
}
