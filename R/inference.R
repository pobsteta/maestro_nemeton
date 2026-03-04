# ==============================================================================
# Environnement conda MAESTRO
# ==============================================================================

#' Nom de l'environnement conda pour MAESTRO
#' @export
CONDA_ENV <- "maestro"

#' Configurer l'environnement Python pour MAESTRO
#'
#' Configure reticulate pour utiliser l'environnement conda `maestro`
#' et verifie que tous les modules requis sont disponibles.
#' Pattern identique a `setup_python()` de flair_hub_nemeton.
#'
#' @param envname Nom de l'environnement conda (defaut: "maestro")
#' @return Invisible NULL
#' @export
configurer_python <- function(envname = CONDA_ENV) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Le package 'reticulate' est requis. Installez-le avec : ",
         "install.packages('reticulate')")
  }
  library(reticulate)

  use_condaenv(envname, required = TRUE)
  message("Environnement conda configure: ", envname)

  # Verifier les modules disponibles
  modules <- c("torch", "numpy", "safetensors")
  ok <- TRUE
  for (mod in modules) {
    avail <- py_module_available(mod)
    message(sprintf("  Python %s: %s", mod, ifelse(avail, "OK", "MANQUANT")))
    if (!avail) ok <- FALSE
  }

  if (!ok) {
    stop("Modules Python manquants. Installez-les dans l'env '", envname, "':\n",
         "  conda activate ", envname, "\n",
         "  pip install torch numpy safetensors")
  }

  message("  Python configure.")
  invisible(NULL)
}

#' Chemin vers le module Python d'inference MAESTRO
#'
#' Retourne le chemin du repertoire contenant `maestro_inference.py`,
#' installe avec le package dans `inst/python/`.
#'
#' @return Chemin du repertoire Python (character)
#' @export
python_module_path <- function() {
  system.file("python", package = "maestro", mustWork = TRUE)
}

#' Executer l'inference MAESTRO sur des patches
#'
#' Charge le modele MAESTRO via le module Python et predit la classe
#' d'essence forestiere pour chaque patch.
#'
#' @param patches_data Liste de matrices de patches (issue de
#'   [extraire_patches_raster()])
#' @param fichiers_modele Liste avec `config` et `weights` (issue de
#'   [telecharger_modele()])
#' @param n_classes Nombre de classes de sortie (defaut: 13 pour PureForest)
#' @param n_bands Nombre de bandes d'entree (4 = RGBI, 5 = RGBI+MNT)
#' @param utiliser_gpu Utiliser le GPU CUDA (defaut: FALSE)
#' @param batch_size Taille des batchs pour l'inference (defaut: 16)
#' @return Liste de predictions (codes de classes 0-12)
#' @export
executer_inference <- function(patches_data, fichiers_modele, n_classes = 13L,
                                n_bands = 5L, utiliser_gpu = FALSE,
                                batch_size = 16L) {
  message("=== Inference MAESTRO ===")

  py_path <- python_module_path()
  maestro <- reticulate::import_from_path("maestro_inference", path = py_path)

  torch <- reticulate::import("torch")
  np <- reticulate::import("numpy")

  device_str <- if (utiliser_gpu && torch$cuda$is_available()) {
    message("  Utilisation du GPU (CUDA)")
    "cuda"
  } else {
    message("  Utilisation du CPU")
    "cpu"
  }

  chemin_poids <- fichiers_modele$weights %||% fichiers_modele$snapshot
  if (is.null(chemin_poids)) {
    stop("Impossible de trouver les poids du modele.")
  }

  modele <- maestro$charger_modele(
    chemin_poids = chemin_poids,
    n_classes = as.integer(n_classes),
    device = device_str,
    in_channels = as.integer(n_bands)
  )

  message("  Lancement de l'inference...")
  n_patches <- length(patches_data)
  predictions <- vector("list", n_patches)
  n_batches <- ceiling(n_patches / batch_size)
  patch_size <- as.integer(sqrt(nrow(patches_data[[1]])))

  for (b in seq_len(n_batches)) {
    debut <- (b - 1L) * batch_size + 1L
    fin <- min(b * batch_size, n_patches)
    indices <- debut:fin

    batch_arrays <- lapply(patches_data[indices], function(p) {
      np$array(p, dtype = np$float32)
    })
    batch_np <- np$stack(batch_arrays)

    preds <- maestro$predire_batch_from_values(
      modele, batch_np,
      patch_h = patch_size, patch_w = patch_size,
      device = device_str
    )

    for (j in seq_along(indices)) {
      predictions[[indices[j]]] <- preds[j]
    }

    if (b %% 10 == 0 || b == n_batches) {
      message(sprintf("  Batch %d / %d traite", b, n_batches))
    }
  }

  predictions
}
