# =============================================================================
# Telechargement du dataset FLAIR-HUB depuis HuggingFace
# =============================================================================

#' Lister les fichiers d'un dataset HuggingFace
#'
#' Interroge l'API HuggingFace pour lister les fichiers disponibles dans un
#' dataset (ou sous-ensemble).
#'
#' @param repo_id Identifiant du repository HuggingFace (ex: "IGNF/FLAIR-HUB")
#' @param path Sous-chemin optionnel pour filtrer
#' @param token Token HuggingFace (optionnel, pour les repos prives)
#' @return Vecteur de noms de fichiers
#' @export
hf_list_files <- function(repo_id, path = NULL, token = NULL) {
  url <- sprintf("https://huggingface.co/api/datasets/%s/tree/main", repo_id)
  if (!is.null(path)) {
    url <- paste0(url, "/", path)
  }

  h <- curl::new_handle()
  if (!is.null(token)) {
    curl::handle_setheaders(h, Authorization = paste("Bearer", token))
  }

  resp <- tryCatch(
    curl::curl_fetch_memory(url, handle = h),
    error = function(e) {
      warning("Erreur API HuggingFace: ", e$message)
      return(NULL)
    }
  )

  if (is.null(resp) || resp$status_code != 200) {
    warning("Impossible de lister les fichiers (HTTP ", resp$status_code, ")")
    return(character(0))
  }

  items <- jsonlite::fromJSON(rawToChar(resp$content))
  if (is.data.frame(items) && "path" %in% names(items)) {
    return(items$path)
  }
  character(0)
}

#' Telecharger un fichier depuis un dataset HuggingFace
#'
#' @param repo_id Identifiant du repository (ex: "IGNF/FLAIR-HUB")
#' @param filename Nom du fichier dans le repo
#' @param dest_dir Repertoire de destination local
#' @param token Token HuggingFace (optionnel)
#' @return Chemin du fichier telecharge
#' @export
hf_download_file <- function(repo_id, filename, dest_dir, token = NULL) {
  fs::dir_create(dest_dir)

  url <- sprintf("https://huggingface.co/datasets/%s/resolve/main/%s",
                 repo_id, filename)
  dest_file <- file.path(dest_dir, basename(filename))

  if (file.exists(dest_file)) {
    message("  Deja telecharge: ", basename(filename))
    return(dest_file)
  }

  h <- curl::new_handle()
  curl::handle_setopt(h, followlocation = TRUE)
  if (!is.null(token)) {
    curl::handle_setheaders(h, Authorization = paste("Bearer", token))
  }

  message("  Telechargement: ", filename)
  tryCatch(
    curl::curl_download(url, dest_file, handle = h, quiet = TRUE),
    error = function(e) {
      warning("Echec telechargement ", filename, ": ", e$message)
      return(NULL)
    }
  )

  dest_file
}

#' Telecharger un sous-ensemble du dataset FLAIR-HUB
#'
#' Telecharge les fichiers d'une modalite et d'un domaine specifiques
#' du dataset FLAIR-HUB depuis HuggingFace.
#'
#' @param modalite Modalite a telecharger: "aerial", "dem", "spot", "s2", "s1",
#'   "labels_cosia", "labels_lpis"
#' @param domaine Domaine geographique (ex: "D001_2019")
#' @param data_dir Repertoire de base pour les donnees
#' @param repo_id Identifiant du repository (defaut: "IGNF/FLAIR-HUB")
#' @param token Token HuggingFace (optionnel)
#' @return Vecteur de chemins de fichiers telecharges
#' @export
download_flair_subset <- function(modalite, domaine = NULL,
                                   data_dir = "data/flair_hub",
                                   repo_id = "IGNF/FLAIR-HUB",
                                   token = NULL) {
  message(sprintf("=== Telechargement FLAIR-HUB: %s ===", modalite))

  path <- modalite
  if (!is.null(domaine)) {
    path <- file.path(modalite, domaine)
  }

  files <- hf_list_files(repo_id, path = path, token = token)
  if (length(files) == 0) {
    warning("Aucun fichier trouve pour ", path)
    return(character(0))
  }

  # Filtrer les fichiers .tif
  tif_files <- files[grepl("\\.tif$", files, ignore.case = TRUE)]
  message(sprintf("  %d fichiers TIF trouves", length(tif_files)))

  downloaded <- vapply(tif_files, function(f) {
    dest_dir <- file.path(data_dir, dirname(f))
    hf_download_file(repo_id, f, dest_dir, token)
  }, character(1))

  downloaded
}

#' Telecharger le toy dataset FLAIR-HUB
#'
#' Telecharge un petit jeu de donnees d'exemple pour tester le pipeline.
#'
#' @param data_dir Repertoire de destination
#' @param token Token HuggingFace (optionnel)
#' @return Chemin du repertoire contenant les donnees
#' @export
download_flair_toy <- function(data_dir = "data/flair_hub_toy",
                                token = NULL) {
  message("=== Telechargement du toy dataset FLAIR-HUB ===")
  fs::dir_create(data_dir)

  repo_id <- "IGNF/FLAIR-HUB"

  # Telecharger un petit domaine d'exemple
  download_flair_subset("aerial", domaine = "D001_2019",
                         data_dir = data_dir, repo_id = repo_id, token = token)
  download_flair_subset("labels_cosia", domaine = "D001_2019",
                         data_dir = data_dir, repo_id = repo_id, token = token)

  message("Toy dataset telecharge dans: ", data_dir)
  data_dir
}

#' Charger un patch FLAIR-HUB depuis un fichier GeoTIFF
#'
#' @param tif_path Chemin vers le fichier GeoTIFF du patch
#' @return SpatRaster
#' @export
load_flair_patch <- function(tif_path) {
  if (!file.exists(tif_path)) {
    stop("Fichier introuvable: ", tif_path)
  }
  terra::rast(tif_path)
}

#' Scanner les fichiers FLAIR-HUB disponibles localement
#'
#' Explore un repertoire pour detecter les patches FLAIR-HUB
#' et leurs modalites.
#'
#' @param data_dir Repertoire contenant les donnees FLAIR-HUB
#' @return Liste nommee par modalite, chacune contenant les chemins des fichiers
#' @export
scan_flair_files <- function(data_dir = "data/flair_hub") {
  if (!dir.exists(data_dir)) {
    message("Repertoire introuvable: ", data_dir)
    return(list())
  }

  modalites <- c("aerial", "dem", "spot", "s2", "s1",
                 "labels_cosia", "labels_lpis")

  result <- list()
  for (mod in modalites) {
    mod_dir <- file.path(data_dir, mod)
    if (dir.exists(mod_dir)) {
      files <- list.files(mod_dir, pattern = "\\.tif$",
                          recursive = TRUE, full.names = TRUE)
      if (length(files) > 0) {
        result[[mod]] <- files
        message(sprintf("  %s: %d fichiers", mod, length(files)))
      }
    }
  }

  if (length(result) == 0) {
    message("Aucun fichier FLAIR-HUB trouve dans: ", data_dir)
  }

  result
}
