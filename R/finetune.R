# ==============================================================================
# Fine-tuning MAESTRO sur TreeSatAI
# ==============================================================================

#' Telecharger le dataset TreeSatAI
#'
#' Telecharge et extrait le dataset TreeSatAI Benchmark Archive.
#' Essaie d'abord Hugging Face (IGNF/TreeSatAI-Time-Series), puis
#' Zenodo en fallback (per-espece). Le dataset contient des patches
#' aerial (CIR 0.2m), Sentinel-1 et Sentinel-2 avec des labels de
#' 20 especes d'arbres europeens.
#'
#' Structure de sortie attendue par le fine-tuning :
#' \preformatted{
#' output_dir/
#'   aerial/
#'     train/
#'       Abies/ Acer/ ... Tilia/
#'         *.tif
#'     test/
#'       Abies/ Acer/ ... Tilia/
#'         *.tif
#' }
#'
#' @param output_dir Repertoire de destination (defaut: `"data/TreeSatAI"`)
#' @param modalities Vecteur de modalites a telecharger : `"aerial"`, `"s1"`,
#'   `"s2"` (defaut: toutes)
#' @param source Source de telechargement : `"auto"` (HF puis Zenodo),
#'   `"huggingface"`, ou `"zenodo"` (defaut: `"auto"`)
#' @return Chemin vers le dossier extrait (invisible)
#' @export
#' @examples
#' \dontrun{
#' data_dir <- download_treesatai("data/TreeSatAI")
#' data_dir <- download_treesatai("data/TreeSatAI", modalities = "aerial")
#' data_dir <- download_treesatai("data/TreeSatAI", source = "zenodo")
#' }
download_treesatai <- function(output_dir = "data/TreeSatAI",
                                modalities = c("aerial", "s1", "s2"),
                                source = "auto") {
  message("=== Telechargement du dataset TreeSatAI ===")

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Verifier si les donnees existent deja
  aerial_ok <- .treesatai_check_structure(output_dir, "aerial")
  if ("aerial" %in% modalities && aerial_ok) {
    message("  Donnees aerial deja presentes dans: ", output_dir)
    return(invisible(output_dir))
  }

  # Essayer HuggingFace d'abord
  if (source %in% c("auto", "huggingface")) {
    hf_ok <- tryCatch({
      .treesatai_download_hf(output_dir, modalities)
      TRUE
    }, error = function(e) {
      message(sprintf("  HuggingFace echoue: %s", e$message))
      FALSE
    })
    if (hf_ok) {
      message(sprintf("\n  Dataset TreeSatAI dans: %s", output_dir))
      return(invisible(output_dir))
    }
  }

  # Fallback Zenodo (per-espece)
  if (source %in% c("auto", "zenodo")) {
    tryCatch({
      .treesatai_download_zenodo(output_dir, modalities)
    }, error = function(e) {
      message(sprintf("  Zenodo echoue: %s", e$message))
    })
  }

  message(sprintf("\n  Dataset TreeSatAI dans: %s", output_dir))
  invisible(output_dir)
}


#' Verifier la structure du dataset TreeSatAI
#' @keywords internal
.treesatai_check_structure <- function(data_dir, modality = "aerial") {
  train_dir <- file.path(data_dir, modality, "train")
  if (!dir.exists(train_dir)) return(FALSE)
  # Verifier qu'il y a au moins un sous-dossier avec des .tif
  subdirs <- list.dirs(train_dir, recursive = FALSE)
  if (length(subdirs) == 0) return(FALSE)
  tifs <- list.files(subdirs[1], pattern = "\\.tif$", recursive = FALSE)
  length(tifs) > 0
}


#' Telecharger TreeSatAI depuis Hugging Face
#' @keywords internal
.treesatai_download_hf <- function(output_dir, modalities) {
  if (!requireNamespace("hfhub", quietly = TRUE)) {
    stop("Package 'hfhub' requis: install.packages('hfhub')")
  }

  message("  Source: HuggingFace (IGNF/TreeSatAI-Time-Series)")

  # Telecharger le snapshot du dataset
  snapshot_path <- hfhub::hub_snapshot(
    repo_id = "IGNF/TreeSatAI-Time-Series",
    repo_type = "dataset"
  )
  message(sprintf("  Snapshot HF: %s", snapshot_path))

  # Scanner le snapshot pour trouver les .tif aeriens
  # Structure HF possible: data/aerial/60m/<species>/*.tif
  # ou: aerial/<split>/<species>/*.tif
  # ou: <species>/*.tif dans un sous-dossier aerial
  .treesatai_organize_from_snapshot(snapshot_path, output_dir, modalities)
}


#' Organiser les fichiers telecharges en structure train/test/classe
#' @keywords internal
.treesatai_organize_from_snapshot <- function(snapshot_path, output_dir,
                                               modalities) {
  # Les 20 genres TreeSatAI
  species <- c("Abies", "Acer", "Alnus", "Betula", "Carpinus",
               "Castanea", "Fagus", "Fraxinus", "Larix", "Picea",
               "Pinus", "Populus", "Prunus", "Pseudotsuga", "Quercus",
               "Robinia", "Salix", "Sorbus", "Taxus", "Tilia")

  for (mod in modalities) {
    # Chercher les dossiers aerial dans le snapshot
    # Patterns possibles dans le snapshot HF
    search_dirs <- c(
      file.path(snapshot_path, mod),
      file.path(snapshot_path, "data", mod),
      file.path(snapshot_path, mod, "60m"),
      file.path(snapshot_path, "data", mod, "60m")
    )

    src_dir <- NULL
    for (d in search_dirs) {
      if (dir.exists(d)) {
        src_dir <- d
        break
      }
    }

    if (is.null(src_dir)) {
      message(sprintf("  [WARN] Dossier %s introuvable dans le snapshot HF", mod))
      next
    }

    # Determiner si train/test existe deja dans le source
    has_train <- dir.exists(file.path(src_dir, "train"))
    has_test <- dir.exists(file.path(src_dir, "test"))

    if (has_train || has_test) {
      # Structure avec train/test : copier tel quel
      for (split in c("train", "test")) {
        split_src <- file.path(src_dir, split)
        if (!dir.exists(split_src)) next
        split_dst <- file.path(output_dir, mod, split)
        if (!dir.exists(split_dst)) {
          dir.create(split_dst, recursive = TRUE)
        }
        .treesatai_copy_species(split_src, split_dst, species)
      }
    } else {
      # Structure plate par espece : split 90/10 (comme l'original)
      message(sprintf("  Organisation %s en train/test (90/10)...", mod))
      all_tifs <- list.files(src_dir, pattern = "\\.tif$",
                              recursive = TRUE, full.names = TRUE)
      if (length(all_tifs) == 0) {
        message(sprintf("  [WARN] Aucun .tif trouve dans %s", src_dir))
        next
      }

      # Organiser par espece
      for (sp in species) {
        sp_tifs <- all_tifs[grepl(paste0("/", sp, "/"), all_tifs,
                                   ignore.case = TRUE)]
        if (length(sp_tifs) == 0) next

        # Split 90/10
        set.seed(42)
        n <- length(sp_tifs)
        train_idx <- sort(sample.int(n, size = round(n * 0.9)))
        test_idx <- setdiff(seq_len(n), train_idx)

        for (split in c("train", "test")) {
          idx <- if (split == "train") train_idx else test_idx
          if (length(idx) == 0) next
          dst <- file.path(output_dir, mod, split, sp)
          if (!dir.exists(dst)) dir.create(dst, recursive = TRUE)
          file.copy(sp_tifs[idx], dst, overwrite = FALSE)
        }
      }
    }

    n_train <- length(list.files(file.path(output_dir, mod, "train"),
                                  pattern = "\\.tif$", recursive = TRUE))
    n_test <- length(list.files(file.path(output_dir, mod, "test"),
                                 pattern = "\\.tif$", recursive = TRUE))
    message(sprintf("  %s: %d train + %d test", mod, n_train, n_test))
  }
}


#' Copier les dossiers d'especes d'un split
#' @keywords internal
.treesatai_copy_species <- function(src_split, dst_split, species) {
  src_dirs <- list.dirs(src_split, recursive = FALSE)
  for (src_d in src_dirs) {
    sp_name <- basename(src_d)
    # Verifier que c'est un genre connu (ou variante)
    sp_match <- species[tolower(species) == tolower(sp_name)]
    if (length(sp_match) == 0) {
      # Essayer correspondance partielle (ex: "quercus_robur" -> "Quercus")
      for (sp in species) {
        if (grepl(tolower(sp), tolower(sp_name))) {
          sp_match <- sp
          break
        }
      }
    }
    if (length(sp_match) == 0) next
    sp_name_out <- sp_match[1]

    dst_d <- file.path(dst_split, sp_name_out)
    if (!dir.exists(dst_d)) dir.create(dst_d, recursive = TRUE)
    tifs <- list.files(src_d, pattern = "\\.tif$", full.names = TRUE)
    if (length(tifs) > 0) {
      file.copy(tifs, dst_d, overwrite = FALSE)
    }
  }
}


#' Telecharger TreeSatAI depuis Zenodo (per-espece)
#' @keywords internal
.treesatai_download_zenodo <- function(output_dir, modalities) {
  message("  Source: Zenodo (per-espece)")

  # Les 20 genres TreeSatAI
  species <- c("Abies", "Acer", "Alnus", "Betula", "Carpinus",
               "Castanea", "Fagus", "Fraxinus", "Larix", "Picea",
               "Pinus", "Populus", "Prunus", "Pseudotsuga", "Quercus",
               "Robinia", "Salix", "Sorbus", "Taxus", "Tilia")

  # DOI: 10.5281/zenodo.6598391
  # Les fichiers aerial sont par espece, pas par split train/test
  zenodo_base <- "https://zenodo.org/records/6598391/files"

  for (mod in modalities) {
    if (mod == "aerial") {
      # Aerial : 20 zips par espece
      for (sp in species) {
        fname <- sprintf("%s_60m_%s.zip", mod, sp)
        .treesatai_download_one(zenodo_base, fname, output_dir, mod)
      }
    } else {
      # S1/S2 : un seul zip par modalite (train/test separes ou combines)
      for (suffix in c("train", "test", "")) {
        if (suffix == "") {
          fname <- sprintf("%s_60m.zip", mod)
        } else {
          fname <- sprintf("%s_60m_%s.zip", mod, suffix)
        }
        .treesatai_download_one(zenodo_base, fname, output_dir, mod)
      }
    }

    # Extraire et organiser
    .treesatai_extract_zenodo(output_dir, mod)
  }
}


#' Telecharger un fichier depuis Zenodo
#' @keywords internal
.treesatai_download_one <- function(zenodo_base, fname, output_dir, mod) {
  url <- paste0(zenodo_base, "/", fname, "?download=1")
  dest <- file.path(output_dir, fname)

  if (file.exists(dest)) {
    message(sprintf("  Deja telecharge: %s", fname))
    return(invisible(TRUE))
  }

  message(sprintf("  Telechargement: %s ...", fname))
  tryCatch({
    h <- curl::new_handle()
    curl::handle_setopt(h, followlocation = TRUE, timeout = 600L)
    curl::curl_download(url, dest, handle = h)
    message(sprintf("  OK: %s (%.1f Mo)", fname, file.size(dest) / 1e6))
    invisible(TRUE)
  }, error = function(e) {
    message(sprintf("  ERREUR: %s - %s", fname, e$message))
    if (file.exists(dest)) unlink(dest)  # supprimer fichier partiel
    invisible(FALSE)
  })
}


#' Extraire et organiser les zips Zenodo per-espece
#' @keywords internal
.treesatai_extract_zenodo <- function(output_dir, mod) {
  zips <- list.files(output_dir, pattern = sprintf("^%s_60m.*\\.zip$", mod),
                      full.names = TRUE)
  if (length(zips) == 0) return(invisible(NULL))

  message(sprintf("\n  Extraction de %d archives %s...", length(zips), mod))
  tmp_extract <- file.path(output_dir, paste0(".tmp_", mod))
  dir.create(tmp_extract, recursive = TRUE, showWarnings = FALSE)

  for (zp in zips) {
    tryCatch({
      utils::unzip(zp, exdir = tmp_extract)
    }, error = function(e) {
      message(sprintf("  ERREUR extraction: %s - %s", basename(zp), e$message))
    })
  }

  # Reorganiser en train/test si necessaire
  all_tifs <- list.files(tmp_extract, pattern = "\\.tif$",
                          recursive = TRUE, full.names = TRUE)
  if (length(all_tifs) == 0) {
    unlink(tmp_extract, recursive = TRUE)
    return(invisible(NULL))
  }

  # Determiner si la structure train/test existe deja
  has_splits <- any(grepl("/(train|test)/", all_tifs))

  if (has_splits) {
    # Copier en preservant la structure
    for (tif in all_tifs) {
      rel <- sub(paste0("^", normalizePath(tmp_extract, winslash = "/"), "/?"),
                 "", normalizePath(tif, winslash = "/"))
      dst <- file.path(output_dir, mod, rel)
      dst_dir <- dirname(dst)
      if (!dir.exists(dst_dir)) dir.create(dst_dir, recursive = TRUE)
      file.copy(tif, dst, overwrite = FALSE)
    }
  } else {
    # Pas de splits : creer train/test 90/10
    message(sprintf("  Organisation %s en train/test (90/10)...", mod))

    # Grouper par dossier parent (= espece)
    parents <- unique(dirname(all_tifs))
    for (parent_dir in parents) {
      sp_name <- basename(parent_dir)
      sp_tifs <- list.files(parent_dir, pattern = "\\.tif$",
                             full.names = TRUE)
      set.seed(42)
      n <- length(sp_tifs)
      train_idx <- sort(sample.int(n, size = round(n * 0.9)))
      test_idx <- setdiff(seq_len(n), train_idx)

      for (split in c("train", "test")) {
        idx <- if (split == "train") train_idx else test_idx
        if (length(idx) == 0) next
        dst <- file.path(output_dir, mod, split, sp_name)
        if (!dir.exists(dst)) dir.create(dst, recursive = TRUE)
        file.copy(sp_tifs[idx], dst, overwrite = FALSE)
      }
    }
  }

  n_train <- length(list.files(file.path(output_dir, mod, "train"),
                                pattern = "\\.tif$", recursive = TRUE))
  n_test <- length(list.files(file.path(output_dir, mod, "test"),
                               pattern = "\\.tif$", recursive = TRUE))
  message(sprintf("  %s: %d train + %d test", mod, n_train, n_test))

  # Nettoyer le dossier temporaire
  unlink(tmp_extract, recursive = TRUE)
}


#' Telecharger le dataset TreeSatAI Time-Series depuis Hugging Face
#'
#' Raccourci vers [download_treesatai()] avec `source = "huggingface"`.
#' Telecharge la version etendue du dataset TreeSatAI avec des series
#' temporelles Sentinel-1 et Sentinel-2, hebergee par l'IGNF sur
#' Hugging Face, et organise les fichiers en structure train/test/classe.
#'
#' @param output_dir Repertoire de destination
#' @param modalities Modalites a telecharger (defaut: `c("aerial")`)
#' @return Chemin vers le dossier (invisible)
#' @export
#' @examples
#' \dontrun{
#' data_dir <- download_treesatai_hf("data/TreeSatAI")
#' }
download_treesatai_hf <- function(output_dir = "data/TreeSatAI",
                                    modalities = c("aerial")) {
  download_treesatai(output_dir, modalities = modalities,
                      source = "huggingface")
}


#' Fine-tuner MAESTRO sur le dataset TreeSatAI
#'
#' Charge les encodeurs pre-entraines MAESTRO et entraine la tete de
#' classification sur les donnees TreeSatAI. Les 20 especes TreeSatAI sont
#' regroupees en 8 classes (schema simplifie). Les 13 classes PureForest
#' seront utilisees quand le LiDAR sera integre.
#'
#' @param checkpoint_path Chemin vers le checkpoint pre-entraine MAESTRO (.ckpt).
#'   Peut etre obtenu via [telecharger_modele()].
#' @param data_dir Chemin vers le dossier TreeSatAI (structure attendue :
#'   `aerial/train/<classe>/*.tif`)
#' @param output_path Chemin de sortie pour le checkpoint fine-tune
#'   (defaut: `"outputs/maestro_7classes_treesatai.pt"`)
#' @param epochs Nombre d'epoques d'entrainement (defaut: 30)
#' @param lr Learning rate pour la tete de classification (defaut: 1e-3)
#' @param lr_encoder Learning rate pour les encodeurs si non geles (defaut: 1e-5)
#' @param batch_size Taille du batch (defaut: 16)
#' @param freeze_encoder Geler les encodeurs et n'entrainer que la tete ?
#'   (defaut: TRUE)
#' @param modalities Modalites a utiliser pour le fine-tuning
#'   (defaut: `c("aerial")`)
#' @param gpu Utiliser CUDA (defaut: FALSE)
#' @param patience Early stopping patience (defaut: 5)
#' @return Liste avec historique d'entrainement et chemin du checkpoint
#' @export
#' @examples
#' \dontrun{
#' # Telecharger le modele pre-entraine
#' fichiers_modele <- telecharger_modele()
#'
#' # Telecharger TreeSatAI
#' download_treesatai("data/TreeSatAI", modalities = "aerial")
#'
#' # Fine-tuner (tete seulement, ~30 min sur CPU)
#' result <- finetune_maestro(
#'   checkpoint_path = fichiers_modele$weights,
#'   data_dir = "data/TreeSatAI",
#'   output_path = "outputs/maestro_7classes_treesatai.pt",
#'   epochs = 30, freeze_encoder = TRUE
#' )
#'
#' # Utiliser le modele fine-tune dans le pipeline
#' maestro_pipeline("data/aoi.gpkg",
#'                   model_id = "outputs/maestro_7classes_treesatai.pt")
#' }
finetune_maestro <- function(checkpoint_path, data_dir,
                               output_path = "outputs/maestro_7classes_treesatai.pt",
                               epochs = 30L, lr = 1e-3, lr_encoder = 1e-5,
                               batch_size = 16L, freeze_encoder = TRUE,
                               modalities = c("aerial"),
                               gpu = FALSE, patience = 5L) {
  message("=== Fine-tuning MAESTRO sur TreeSatAI ===")

  # Configurer Python
  configurer_python()

  py_path <- system.file("python", package = "maestro", mustWork = TRUE)
  finetune_module <- reticulate::import_from_path("maestro_finetune",
                                                    path = py_path)

  torch <- reticulate::import("torch")

  device_str <- if (gpu && torch$cuda$is_available()) {
    message("  Utilisation du GPU (CUDA)")
    "cuda"
  } else {
    message("  Utilisation du CPU")
    "cpu"
  }

  # Creer le dossier de sortie si necessaire
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # Lancer le fine-tuning
  result <- finetune_module$finetuner(
    checkpoint_path = checkpoint_path,
    data_dir = data_dir,
    output_path = output_path,
    epochs = as.integer(epochs),
    lr = lr,
    lr_encoder = lr_encoder,
    batch_size = as.integer(batch_size),
    freeze_encoder = freeze_encoder,
    modalities = as.list(modalities),
    n_classes = 7L,
    device = device_str,
    patience = as.integer(patience)
  )

  message(sprintf("\n  Checkpoint fine-tune: %s", output_path))
  message(sprintf("  Meilleure precision: %.1f%% (epoch %d)",
                  result$best_val_acc, result$best_epoch))

  invisible(result)
}
