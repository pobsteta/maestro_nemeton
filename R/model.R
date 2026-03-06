#' Trouver le nom du fichier checkpoint dans un depot Hugging Face
#'
#' Interroge l'API Hugging Face pour lister les fichiers du depot et
#' retourne le fichier de poids le plus probable.
#'
#' @param hf_repo Identifiant du depot (ex: `"IGNF/MAESTRO_FLAIR-HUB_base"`)
#' @return Nom du fichier checkpoint, ou NULL
#' @export
find_checkpoint_name <- function(hf_repo) {
  url <- paste0("https://huggingface.co/api/models/", hf_repo)
  resp <- tryCatch({
    tmp <- tempfile()
    h <- curl::new_handle()
    curl::curl_download(url, tmp, handle = h)
    info <- jsonlite::fromJSON(tmp)
    unlink(tmp)
    info
  }, error = function(e) NULL)
  if (is.null(resp)) return(NULL)

  files <- resp$siblings$rfilename
  ckpt_files <- files[grepl("\\.(ckpt|pth|pt|bin|safetensors)$", files)]
  if (length(ckpt_files) == 0) return(NULL)

  ext_priority <- c("\\.ckpt$", "\\.pth$", "\\.pt$", "\\.bin$", "\\.safetensors$")
  for (pat in ext_priority) {
    matches <- ckpt_files[grepl(pat, ckpt_files)]
    if (length(matches) > 0) return(matches[1])
  }
  ckpt_files[1]
}

#' Reparer un symlink casse du cache HuggingFace (Windows)
#'
#' Sur Windows sans privileges developpeur, hfhub ne peut pas creer de
#' symlinks. Le fichier retourne par hub_download() pointe vers un snapshot
#' inexistant. Cette fonction detecte ce cas et copie le blob reel a la
#' place du symlink casse.
#'
#' @param chemin Chemin retourne par `hfhub::hub_download()`
#' @return Chemin valide (le meme si OK, ou apres copie du blob)
#' @keywords internal
reparer_symlink_hf <- function(chemin) {
  if (file.exists(chemin)) return(chemin)

  # Le chemin est sous .../snapshots/<hash>/...
  # Le blob correspondant est sous .../blobs/<sha256>
  # On cherche dans le dossier blobs le fichier le plus gros (= le checkpoint)
  cache_root <- chemin
  while (!grepl("snapshots$", basename(dirname(cache_root))) &&
         nchar(cache_root) > 3) {
    cache_root <- dirname(cache_root)
  }
  model_dir <- dirname(dirname(cache_root))  # remonte au-dessus de snapshots/
  blobs_dir <- file.path(model_dir, "blobs")

  if (!dir.exists(blobs_dir)) {
    warning("Dossier blobs introuvable: ", blobs_dir)
    return(chemin)
  }

  blobs <- list.files(blobs_dir, full.names = TRUE)
  if (length(blobs) == 0) {
    warning("Aucun blob dans: ", blobs_dir)
    return(chemin)
  }

  # Prendre le plus gros blob (c'est le checkpoint)
  sizes <- file.size(blobs)
  blob_path <- blobs[which.max(sizes)]
  blob_size <- max(sizes, na.rm = TRUE)

  message(sprintf("  [Windows] Symlink casse, copie du blob (%.0f Mo)...",
                  blob_size / 1e6))

  # Creer les dossiers parents si necessaire
  dir.create(dirname(chemin), recursive = TRUE, showWarnings = FALSE)
  file.copy(blob_path, chemin, overwrite = TRUE)

  if (file.exists(chemin)) {
    message("  [Windows] Checkpoint copie avec succes")
  } else {
    warning("Impossible de copier le blob vers: ", chemin)
  }

  chemin
}

#' Telecharger le modele MAESTRO depuis Hugging Face
#'
#' Utilise le package R [hfhub](https://cran.r-project.org/package=hfhub)
#' pour telecharger les poids et la configuration du modele. Les fichiers
#' sont mis en cache par hfhub et reutilises automatiquement.
#'
#' @param repo_id Identifiant du depot HF (ex: `"IGNF/MAESTRO_FLAIR-HUB_base"`)
#' @param token Token Hugging Face (optionnel, ou via `HUGGING_FACE_HUB_TOKEN`)
#' @return Liste avec `config` et `weights` (chemins locaux)
#' @export
telecharger_modele <- function(repo_id = "IGNF/MAESTRO_FLAIR-HUB_base",
                                token = NULL) {
  if (!requireNamespace("hfhub", quietly = TRUE)) {
    stop("Le package 'hfhub' est requis. Installez-le avec : ",
         "install.packages('hfhub')")
  }

  message("=== Telechargement du modele MAESTRO depuis Hugging Face ===")
  message(sprintf("Repository : %s", repo_id))

  if (!is.null(token)) {
    Sys.setenv(HUGGING_FACE_HUB_TOKEN = token)
  }

  fichiers_modele <- list()

  # Config
  tryCatch({
    fichiers_modele$config <- hfhub::hub_download(repo_id, "config.json")
    message(sprintf("  Config : %s", fichiers_modele$config))
  }, error = function(e) message("  Pas de config.json"))

  # Poids : detection auto
  ckpt_name <- find_checkpoint_name(repo_id)
  if (!is.null(ckpt_name)) {
    message(sprintf("  Telechargement via hfhub : %s", ckpt_name))
    tryCatch({
      chemin_poids <- hfhub::hub_download(repo_id, ckpt_name)
      fichiers_modele$weights <- reparer_symlink_hf(chemin_poids)
      message(sprintf("  Poids : %s", fichiers_modele$weights))
    }, error = function(e) message("  hfhub echoue: ", e$message))
  }

  # Fallback noms standards
  if (is.null(fichiers_modele$weights)) {
    noms_poids <- c("model.safetensors", "pytorch_model.bin",
                     "model.pt", "checkpoint.pth")
    for (nom in noms_poids) {
      tryCatch({
        chemin_poids <- hfhub::hub_download(repo_id, nom)
        fichiers_modele$weights <- reparer_symlink_hf(chemin_poids)
        message(sprintf("  Poids : %s", fichiers_modele$weights))
        break
      }, error = function(e) NULL)
    }
  }

  # Dernier fallback : snapshot
  if (is.null(fichiers_modele$weights)) {
    message("  Tentative de snapshot complet...")
    tryCatch({
      fichiers_modele$snapshot <- hfhub::hub_snapshot(repo_id = repo_id)
      message(sprintf("  Snapshot : %s", fichiers_modele$snapshot))
    }, error = function(e) {
      stop("Impossible de telecharger le modele : ", e$message)
    })
  }

  fichiers_modele
}
