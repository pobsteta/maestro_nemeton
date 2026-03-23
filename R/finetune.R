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

  # Verifier quelles modalites sont deja presentes
  missing <- character(0)
  for (mod in modalities) {
    if (.treesatai_check_structure(output_dir, mod)) {
      message(sprintf("  Donnees %s deja presentes, skip.", mod))
    } else {
      missing <- c(missing, mod)
    }
  }
  if (length(missing) == 0) {
    message("  Toutes les modalites sont deja presentes dans: ", output_dir)
    return(invisible(output_dir))
  }
  modalities <- missing

  # Essayer HuggingFace d'abord
  if (source %in% c("auto", "huggingface")) {
    hf_ok <- tryCatch({
      .treesatai_download_hf(output_dir, modalities)
      # Verifier que les donnees sont bien la
      .treesatai_check_structure(output_dir, modalities[1])
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
#'
#' Le dataset HF contient des fichiers zip (aerial.zip, sentinel.zip, etc.)
#' et un fichier split.zip avec les listes train/test.
#' Sur Windows, les symlinks echouent souvent -> on resout les blobs.
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

  # Le repo HF contient: aerial.zip, sentinel.zip, sentinel-ts.zip,
  # geojson.zip, labels.zip, split.zip
  # Sur Windows les symlinks echouent -> resoudre les blobs reels

  # Mapping modalite -> nom du zip dans le repo HF
  mod_zip_map <- list(
    aerial = "aerial.zip",
    s1 = "sentinel.zip",
    s2 = "sentinel.zip"
  )

  for (mod in modalities) {
    zip_name <- mod_zip_map[[mod]]
    if (is.null(zip_name)) {
      message(sprintf("  [WARN] Modalite '%s' non supportee pour HF", mod))
      next
    }

    # Trouver le fichier zip (snapshot ou blob)
    zip_path <- .treesatai_resolve_hf_file(snapshot_path, zip_name)
    if (is.null(zip_path)) {
      stop(sprintf("Fichier %s introuvable dans le snapshot HF", zip_name))
    }
    message(sprintf("  Zip %s: %s (%.0f Mo)",
                    zip_name, basename(zip_path),
                    file.size(zip_path) / 1e6))

    # Extraire le zip dans un dossier temporaire
    tmp_extract <- file.path(output_dir, ".tmp_extract")
    if (dir.exists(tmp_extract)) unlink(tmp_extract, recursive = TRUE)
    dir.create(tmp_extract, recursive = TRUE)
    message(sprintf("  Extraction de %s...", zip_name))
    utils::unzip(zip_path, exdir = tmp_extract)

    # Trouver le dossier racine des tifs
    src_dir <- .treesatai_find_tif_root(tmp_extract, mod)
    if (is.null(src_dir)) {
      unlink(tmp_extract, recursive = TRUE)
      stop(sprintf("Aucun .tif trouve apres extraction de %s", zip_name))
    }

    # Telecharger split.zip pour connaitre les listes train/test
    split_info <- .treesatai_get_split_info(snapshot_path, output_dir)

    # Organiser en train/test/espece
    .treesatai_organize_extracted(src_dir, output_dir, mod, split_info)

    # Nettoyage
    unlink(tmp_extract, recursive = TRUE)

    n_train <- length(list.files(file.path(output_dir, mod, "train"),
                                  pattern = "\\.tif$", recursive = TRUE))
    n_test <- length(list.files(file.path(output_dir, mod, "test"),
                                 pattern = "\\.tif$", recursive = TRUE))
    message(sprintf("  %s: %d train + %d test", mod, n_train, n_test))
  }
}


#' Resoudre un fichier HF (gere les symlinks casses sur Windows)
#'
#' Sur Windows sans privileges admin, hfhub cree des symlinks qui echouent.
#' On lit le pointer file pour trouver le blob reel dans le cache HF.
#' @keywords internal
.treesatai_resolve_hf_file <- function(snapshot_path, filename) {
  fpath <- file.path(snapshot_path, filename)

  # Cas 1: Le fichier existe et est lisible (symlink OK ou Linux/Mac)
  if (file.exists(fpath) && file.size(fpath) > 1000) {
    return(fpath)
  }

  # Cas 2: Windows - le fichier est un pointer/symlink casse
  # Chercher le blob dans le cache HF
  # Le cache est structure: .cache/huggingface/hub/datasets--<org>--<name>/blobs/
  cache_root <- dirname(dirname(snapshot_path))  # remonte de snapshots/<hash>
  blobs_dir <- file.path(cache_root, "blobs")

  if (!dir.exists(blobs_dir)) {
    message(sprintf("  [WARN] Dossier blobs introuvable: %s", blobs_dir))
    return(NULL)
  }

  # Methode 1: Lire le pointer file (format git-lfs)
  if (file.exists(fpath) && file.size(fpath) < 1000) {
    pointer_content <- tryCatch(readLines(fpath, warn = FALSE),
                                 error = function(e) NULL)
    if (!is.null(pointer_content)) {
      sha_line <- grep("^oid sha256:", pointer_content, value = TRUE)
      if (length(sha_line) > 0) {
        sha <- sub("^oid sha256:", "", sha_line[1])
        blob_path <- file.path(blobs_dir, sha)
        if (file.exists(blob_path) && file.size(blob_path) > 1000) {
          message(sprintf("  [Windows] Blob resolu pour %s", filename))
          return(blob_path)
        }
      }
    }
  }

  # Methode 2: Chercher le plus gros blob (heuristique pour aerial.zip)
  blobs <- list.files(blobs_dir, full.names = TRUE)
  if (length(blobs) == 0) return(NULL)

  # Pour aerial.zip on cherche un blob > 100 Mo
  blob_sizes <- file.size(blobs)
  # Trier par taille decroissante
  big_blobs <- blobs[order(blob_sizes, decreasing = TRUE)]
  big_sizes <- sort(blob_sizes, decreasing = TRUE)

  # Essayer de verifier que c'est un zip valide
  for (i in seq_along(big_blobs)) {
    if (big_sizes[i] < 1e6) break  # Moins de 1 Mo, pas un zip de donnees
    # Verifier le magic number ZIP (PK\x03\x04)
    magic <- tryCatch({
      con <- file(big_blobs[i], "rb")
      bytes <- readBin(con, "raw", 4)
      close(con)
      bytes
    }, error = function(e) NULL)
    if (!is.null(magic) && length(magic) >= 4 &&
        magic[1] == as.raw(0x50) && magic[2] == as.raw(0x4b)) {
      # C'est un ZIP, verifier si c'est le bon en regardant le contenu
      zip_contents <- tryCatch(utils::unzip(big_blobs[i], list = TRUE),
                                error = function(e) NULL)
      if (!is.null(zip_contents)) {
        # Pour aerial.zip, on cherche des .tif
        if (grepl("aerial", filename, ignore.case = TRUE)) {
          if (any(grepl("\\.tif$", zip_contents$Name, ignore.case = TRUE))) {
            message(sprintf("  [Windows] Blob identifie pour %s: %s (%.0f Mo)",
                            filename, basename(big_blobs[i]),
                            big_sizes[i] / 1e6))
            return(big_blobs[i])
          }
        } else {
          # Pour d'autres zips, retourner le premier zip valide du bon type
          return(big_blobs[i])
        }
      }
    }
  }

  message(sprintf("  [WARN] Impossible de resoudre %s dans les blobs HF", filename))
  NULL
}


#' Trouver le dossier racine contenant les .tif apres extraction
#' @keywords internal
.treesatai_find_tif_root <- function(extract_dir, modality) {
  # Chercher tous les .tif
  tifs <- list.files(extract_dir, pattern = "\\.tif$",
                      recursive = TRUE, full.names = TRUE)
  if (length(tifs) == 0) return(NULL)

  # Remonter au dossier parent le plus haut qui contient des sous-dossiers especes
  # Structure typique: extract/aerial/Abies/*.tif ou extract/Abies/*.tif
  first_tif_dir <- dirname(tifs[1])
  parent <- dirname(first_tif_dir)

  # Verifier les structures possibles
  # 1. extract_dir/aerial/<species>/*.tif
  mod_dir <- file.path(extract_dir, modality)
  if (dir.exists(mod_dir) &&
      length(list.files(mod_dir, pattern = "\\.tif$", recursive = TRUE)) > 0) {
    return(mod_dir)
  }

  # 2. extract_dir/<species>/*.tif (directement les especes)
  subdirs <- list.dirs(extract_dir, recursive = FALSE)
  for (d in subdirs) {
    if (length(list.files(d, pattern = "\\.tif$")) > 0) {
      return(extract_dir)
    }
  }

  # 3. Remonter depuis le premier tif
  return(parent)
}


#' Obtenir les informations de split train/test depuis split.zip
#' @keywords internal
.treesatai_get_split_info <- function(snapshot_path, output_dir) {
  split_path <- .treesatai_resolve_hf_file(snapshot_path, "split.zip")
  if (is.null(split_path)) {
    message("  [INFO] split.zip non trouve, utilisation du split 90/10")
    return(NULL)
  }

  tmp_split <- file.path(output_dir, ".tmp_split")
  if (dir.exists(tmp_split)) unlink(tmp_split, recursive = TRUE)
  dir.create(tmp_split, recursive = TRUE)

  tryCatch({
    utils::unzip(split_path, exdir = tmp_split)

    # Chercher les fichiers csv/txt de split
    split_files <- list.files(tmp_split, pattern = "\\.(csv|txt)$",
                               recursive = TRUE, full.names = TRUE)
    if (length(split_files) == 0) {
      unlink(tmp_split, recursive = TRUE)
      return(NULL)
    }

    # Lire les fichiers de split
    # Format attendu: un fichier train.csv et test.csv, ou un fichier
    # avec une colonne "split"
    train_files <- split_files[grepl("train", basename(split_files),
                                      ignore.case = TRUE)]
    test_files <- split_files[grepl("test", basename(split_files),
                                     ignore.case = TRUE)]

    result <- list()
    if (length(train_files) > 0) {
      train_data <- tryCatch(
        utils::read.csv(train_files[1], header = FALSE, stringsAsFactors = FALSE),
        error = function(e) {
          # Essayer sans header avec sep differents
          tryCatch(
            utils::read.table(train_files[1], header = FALSE,
                               stringsAsFactors = FALSE),
            error = function(e2) NULL
          )
        }
      )
      if (!is.null(train_data)) {
        result$train <- trimws(train_data[[1]])
      }
    }
    if (length(test_files) > 0) {
      test_data <- tryCatch(
        utils::read.csv(test_files[1], header = FALSE, stringsAsFactors = FALSE),
        error = function(e) {
          tryCatch(
            utils::read.table(test_files[1], header = FALSE,
                               stringsAsFactors = FALSE),
            error = function(e2) NULL
          )
        }
      )
      if (!is.null(test_data)) {
        result$test <- trimws(test_data[[1]])
      }
    }

    unlink(tmp_split, recursive = TRUE)

    if (length(result) > 0) {
      message(sprintf("  Split info: %d train, %d test",
                      length(if (!is.null(result$train)) result$train else character(0)),
                      length(if (!is.null(result$test)) result$test else character(0))))
      return(result)
    }
    NULL
  }, error = function(e) {
    message(sprintf("  [WARN] Erreur lecture split.zip: %s", e$message))
    unlink(tmp_split, recursive = TRUE)
    NULL
  })
}


#' Organiser les fichiers extraits en structure train/test/espece
#'
#' Utilise file.rename() (deplacer) au lieu de file.copy() pour eviter
#' de recopier les donnees sur le meme filesystem. C'est ~100x plus rapide
#' sur 50k fichiers car c'est une simple operation d'inode.
#' @keywords internal
.treesatai_organize_extracted <- function(src_dir, output_dir, mod, split_info) {
  species <- treesatai_species()

  # Verifier si la structure contient deja train/test
  has_train <- dir.exists(file.path(src_dir, "train"))
  has_test <- dir.exists(file.path(src_dir, "test"))

  if (has_train || has_test) {
    message(sprintf("  Structure train/test detectee pour %s", mod))
    for (split in c("train", "test")) {
      split_src <- file.path(src_dir, split)
      if (!dir.exists(split_src)) next
      split_dst <- file.path(output_dir, mod, split)
      if (!dir.exists(split_dst)) dir.create(split_dst, recursive = TRUE)
      .treesatai_move_species(split_src, split_dst, species)
    }
    return(invisible(NULL))
  }

  # Structure plate par espece -> utiliser split_info ou split 90/10
  all_tifs <- list.files(src_dir, pattern = "\\.tif$",
                          recursive = TRUE, full.names = TRUE)
  if (length(all_tifs) == 0) {
    stop(sprintf("Aucun .tif dans %s", src_dir))
  }

  message(sprintf("  Organisation %s: %d fichiers en train/test...",
                  mod, length(all_tifs)))

  for (sp in species) {
    # Matcher par dossier parent ou par prefixe du fichier
    sp_pattern <- paste0("(^|/|\\\\)", sp, "(/|\\\\|_)")
    sp_tifs <- all_tifs[grepl(sp_pattern, all_tifs, ignore.case = TRUE)]
    if (length(sp_tifs) == 0) next

    if (!is.null(split_info)) {
      # Utiliser les listes de split officielles
      tif_basenames <- tools::file_path_sans_ext(basename(sp_tifs))
      train_mask <- tif_basenames %in% split_info$train |
                    basename(sp_tifs) %in% split_info$train
      test_mask <- tif_basenames %in% split_info$test |
                   basename(sp_tifs) %in% split_info$test
      # Les fichiers non classes vont dans train
      unclassed <- !train_mask & !test_mask
      train_mask <- train_mask | unclassed

      train_tifs <- sp_tifs[train_mask]
      test_tifs <- sp_tifs[test_mask]
    } else {
      # Split aleatoire 90/10
      set.seed(42)
      n <- length(sp_tifs)
      train_idx <- sort(sample.int(n, size = round(n * 0.9)))
      test_idx <- setdiff(seq_len(n), train_idx)
      train_tifs <- sp_tifs[train_idx]
      test_tifs <- sp_tifs[test_idx]
    }

    for (split in c("train", "test")) {
      tifs <- if (split == "train") train_tifs else test_tifs
      if (length(tifs) == 0) next
      dst <- file.path(output_dir, mod, split, sp)
      if (!dir.exists(dst)) dir.create(dst, recursive = TRUE)
      .treesatai_move_files(tifs, dst)
    }
  }
}


#' Deplacer des fichiers (file.rename avec fallback file.copy)
#'
#' file.rename() est quasi-instantane sur le meme filesystem (simple
#' operation d'inode). En cas d'echec (cross-filesystem), on tombe
#' sur file.copy() + suppression.
#' @keywords internal
.treesatai_move_files <- function(files, dest_dir) {
  dest_paths <- file.path(dest_dir, basename(files))
  # file.rename est vectorise et retourne TRUE/FALSE par fichier
  moved <- file.rename(files, dest_paths)
  # Fallback pour les fichiers qui n'ont pas pu etre renommes
  # (cross-filesystem, permissions, etc.)
  failed <- which(!moved)
  if (length(failed) > 0) {
    file.copy(files[failed], dest_paths[failed], overwrite = FALSE)
    file.remove(files[failed])
  }
}


#' Deplacer les dossiers d'especes d'un split (move au lieu de copy)
#' @keywords internal
.treesatai_move_species <- function(src_split, dst_split, species) {
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
      .treesatai_move_files(tifs, dst_d)
    }
  }
}


#' Telecharger TreeSatAI depuis Zenodo (per-espece)
#' @keywords internal
.treesatai_download_zenodo <- function(output_dir, modalities) {
  message("  Source: Zenodo (per-espece)")

  species <- treesatai_species()

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
      # Supprimer le ZIP apres extraction pour liberer l'espace disque
      zip_size <- file.size(zp) / 1e6
      unlink(zp)
      message(sprintf("  ZIP supprime: %s (%.0f Mo liberes)", basename(zp), zip_size))
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
    # Deplacer en preservant la structure
    for (tif in all_tifs) {
      rel <- sub(paste0("^", normalizePath(tmp_extract, winslash = "/"), "/?"),
                 "", normalizePath(tif, winslash = "/"))
      dst <- file.path(output_dir, mod, rel)
      dst_dir <- dirname(dst)
      if (!dir.exists(dst_dir)) dir.create(dst_dir, recursive = TRUE)
      .treesatai_move_files(tif, dst_dir)
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
        .treesatai_move_files(sp_tifs[idx], dst)
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
