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

  # Eviter le conflit OpenMP sur Windows (torch + numpy livrent chacun libiomp5md.dll)
  Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE")

  # Auto-detection de conda (Miniforge, Miniconda, Anaconda) si reticulate

  # ne le trouve pas tout seul
  if (is.null(tryCatch(conda_binary(), error = function(e) NULL))) {
    conda_candidates <- if (.Platform$OS.type == "windows") {
      home <- Sys.getenv("USERPROFILE", Sys.getenv("HOME"))
      c(file.path(home, "miniforge3", "condabin", "conda.bat"),
        file.path(home, "mambaforge", "condabin", "conda.bat"),
        file.path(home, "miniconda3", "condabin", "conda.bat"),
        file.path(home, "anaconda3", "condabin", "conda.bat"),
        file.path(Sys.getenv("LOCALAPPDATA"), "miniforge3", "condabin", "conda.bat"),
        file.path(Sys.getenv("PROGRAMDATA"), "miniforge3", "condabin", "conda.bat"))
    } else {
      home <- Sys.getenv("HOME")
      c(file.path(home, "miniforge3", "bin", "conda"),
        file.path(home, "mambaforge", "bin", "conda"),
        file.path(home, "miniconda3", "bin", "conda"),
        file.path(home, "anaconda3", "bin", "conda"),
        "/opt/miniforge3/bin/conda",
        "/opt/miniconda3/bin/conda")
    }
    found <- Filter(file.exists, conda_candidates)
    if (length(found) > 0) {
      Sys.setenv(RETICULATE_CONDA = found[[1]])
      message("Conda detecte automatiquement: ", found[[1]])
    }
  }

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

#' Executer l'inference MAESTRO sur des patches (single-modal, legacy)
#'
#' Charge le modele MAESTRO via le module Python et predit la classe
#' d'essence forestiere pour chaque patch. Version mono-raster (legacy).
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

  # Determiner les modalites selon le nombre de bandes
  if (n_bands <= 4L) {
    mod_list <- list("aerial")
  } else {
    mod_list <- list("aerial", "dem")
  }

  modele <- maestro$charger_modele(
    chemin_poids = chemin_poids,
    n_classes = as.integer(n_classes),
    device = device_str,
    modalites = mod_list
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

#' Executer l'inference MAESTRO multi-modale sur des patches
#'
#' Charge le modele MAESTRO multi-modal et predit la classe d'essence
#' forestiere pour chaque patch en utilisant toutes les modalites disponibles.
#'
#' @param patches_multimodal Liste de listes nommees (issue de
#'   [extraire_patches_multimodal()]). Chaque element contient les matrices
#'   (H*W x C) par modalite : `patches[[i]]$aerial`, `patches[[i]]$dem`
#' @param fichiers_modele Liste avec `config` et `weights` (issue de
#'   [telecharger_modele()])
#' @param n_classes Nombre de classes de sortie (defaut: 13 pour PureForest)
#' @param modalites Vecteur des noms de modalites a utiliser
#'   (defaut: `c("aerial", "dem")`)
#' @param utiliser_gpu Utiliser le GPU CUDA (defaut: FALSE)
#' @param batch_size Taille des batchs pour l'inference (defaut: 16)
#' @return Liste de predictions (codes de classes 0-12)
#' @export
executer_inference_multimodal <- function(patches_multimodal, fichiers_modele,
                                           n_classes = 13L,
                                           modalites = c("aerial", "dem"),
                                           utiliser_gpu = FALSE,
                                           batch_size = 16L) {
  message("=== Inference MAESTRO multi-modale ===")
  message(sprintf("  Modalites: %s", paste(modalites, collapse = " + ")))

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
    modalites = as.list(modalites)
  )

  message("  Lancement de l'inference multi-modale...")
  n_patches <- length(patches_multimodal)
  predictions <- vector("list", n_patches)
  n_batches <- ceiling(n_patches / batch_size)

  # Determiner la taille des patches depuis la premiere modalite
  first_patch <- patches_multimodal[[1]]
  first_mod <- names(first_patch)[1]
  patch_size <- as.integer(sqrt(nrow(first_patch[[first_mod]])))

  for (b in seq_len(n_batches)) {
    debut <- (b - 1L) * batch_size + 1L
    fin <- min(b * batch_size, n_patches)
    indices <- debut:fin
    batch_patches <- patches_multimodal[indices]

    # Construire un dict numpy par modalite
    batch_dict <- list()
    for (mod_name in modalites) {
      # Empiler les matrices (H*W, C) du batch pour cette modalite
      mod_arrays <- lapply(batch_patches, function(p) {
        mat <- p[[mod_name]]
        # Reshape (H*W, C) -> (C, H, W)
        C <- ncol(mat)
        arr <- array(mat, dim = c(patch_size, patch_size, C))
        arr <- aperm(arr, c(3, 1, 2))  # (C, H, W)
        arr
      })
      # Stack en (B, C, H, W)
      batch_4d <- array(
        unlist(mod_arrays),
        dim = c(dim(mod_arrays[[1]]), length(mod_arrays))
      )
      # Reordonner: actuellement (C, H, W, B) -> (B, C, H, W)
      batch_4d <- aperm(batch_4d, c(4, 1, 2, 3))

      batch_dict[[mod_name]] <- np$array(batch_4d, dtype = np$float32)
    }

    # Appeler predire_multimodal
    result <- maestro$predire_multimodal(
      modele, batch_dict,
      device = device_str
    )

    classes <- result$classes
    for (j in seq_along(indices)) {
      predictions[[indices[j]]] <- classes[[j]]
    }

    if (b %% 10 == 0 || b == n_batches) {
      message(sprintf("  Batch %d / %d traite", b, n_batches))
    }
  }

  predictions
}
