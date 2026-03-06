# ==============================================================================
# Fine-tuning MAESTRO sur TreeSatAI
# ==============================================================================

#' Telecharger le dataset TreeSatAI depuis Zenodo
#'
#' Telecharge et extrait le dataset TreeSatAI Benchmark Archive depuis Zenodo.
#' Le dataset contient des patches aerial (CIR 0.2m), Sentinel-1 et Sentinel-2
#' avec des labels de 20 especes d'arbres europeens.
#'
#' @param output_dir Repertoire de destination (defaut: `"data/TreeSatAI"`)
#' @param modalities Vecteur de modalites a telecharger : `"aerial"`, `"s1"`,
#'   `"s2"` (defaut: toutes)
#' @return Chemin vers le dossier extrait (invisible)
#' @export
#' @examples
#' \dontrun{
#' data_dir <- download_treesatai("data/TreeSatAI")
#' }
download_treesatai <- function(output_dir = "data/TreeSatAI",
                                modalities = c("aerial", "s1", "s2")) {
  message("=== Telechargement du dataset TreeSatAI ===")

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # URLs Zenodo pour TreeSatAI
  # DOI: 10.5281/zenodo.6598391
  zenodo_base <- "https://zenodo.org/records/6598391/files"

  files_to_download <- c()
  if ("aerial" %in% modalities) {
    files_to_download <- c(files_to_download,
                            "aerial_60m_train.zip",
                            "aerial_60m_test.zip")
  }
  if ("s1" %in% modalities) {
    files_to_download <- c(files_to_download,
                            "s1_60m_train.zip",
                            "s1_60m_test.zip")
  }
  if ("s2" %in% modalities) {
    files_to_download <- c(files_to_download,
                            "s2_60m_train.zip",
                            "s2_60m_test.zip")
  }

  for (fname in files_to_download) {
    url <- paste0(zenodo_base, "/", fname, "?download=1")
    dest <- file.path(output_dir, fname)

    if (file.exists(dest)) {
      message(sprintf("  Deja telecharge: %s", fname))
      next
    }

    message(sprintf("  Telechargement: %s ...", fname))
    tryCatch({
      h <- curl::new_handle()
      curl::handle_setopt(h, followlocation = TRUE, timeout = 600L)
      curl::curl_download(url, dest, handle = h)
      message(sprintf("  OK: %s (%.1f Mo)", fname, file.size(dest) / 1e6))
    }, error = function(e) {
      message(sprintf("  ERREUR: %s - %s", fname, e$message))
    })
  }

  # Extraire les zips
  message("\n  Extraction des archives...")
  for (fname in files_to_download) {
    zip_path <- file.path(output_dir, fname)
    if (!file.exists(zip_path)) next

    # Determiner le dossier de sortie
    mod_name <- sub("_60m_(train|test)\\.zip$", "", fname)
    split_name <- sub(".*_60m_(train|test)\\.zip$", "\\1", fname)
    extract_dir <- file.path(output_dir, mod_name, split_name)

    if (dir.exists(extract_dir) && length(list.files(extract_dir)) > 0) {
      message(sprintf("  Deja extrait: %s/%s", mod_name, split_name))
      next
    }

    message(sprintf("  Extraction: %s -> %s/%s", fname, mod_name, split_name))
    dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
    tryCatch({
      utils::unzip(zip_path, exdir = extract_dir)
      message(sprintf("  OK: %d fichiers", length(list.files(extract_dir,
                                                              recursive = TRUE))))
    }, error = function(e) {
      message(sprintf("  ERREUR extraction: %s", e$message))
    })
  }

  message(sprintf("\n  Dataset TreeSatAI dans: %s", output_dir))
  invisible(output_dir)
}


#' Telecharger le dataset TreeSatAI Time-Series depuis Hugging Face
#'
#' Version etendue du dataset TreeSatAI avec des series temporelles
#' Sentinel-1 et Sentinel-2, hebergee par l'IGNF sur Hugging Face.
#'
#' @param output_dir Repertoire de destination
#' @return Chemin vers le dossier (invisible)
#' @export
#' @examples
#' \dontrun{
#' data_dir <- download_treesatai_hf("data/TreeSatAI-TS")
#' }
download_treesatai_hf <- function(output_dir = "data/TreeSatAI-TS") {
  if (!requireNamespace("hfhub", quietly = TRUE)) {
    stop("Le package 'hfhub' est requis: install.packages('hfhub')")
  }

  message("=== Telechargement TreeSatAI Time-Series (IGNF/HuggingFace) ===")
  message("  Repository: IGNF/TreeSatAI-Time-Series")

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  tryCatch({
    snapshot_path <- hfhub::hub_snapshot(
      repo_id = "IGNF/TreeSatAI-Time-Series",
      repo_type = "dataset"
    )
    message(sprintf("  Telecharge dans: %s", snapshot_path))
    invisible(snapshot_path)
  }, error = function(e) {
    message(sprintf("  ERREUR: %s", e$message))
    message("  Essayez le telechargement Zenodo: download_treesatai()")
    invisible(NULL)
  })
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
#'   (defaut: `"outputs/finetuned_maestro.pt"`)
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
#'   output_path = "outputs/finetuned_maestro.pt",
#'   epochs = 30, freeze_encoder = TRUE
#' )
#'
#' # Utiliser le modele fine-tune dans le pipeline
#' maestro_pipeline("data/aoi.gpkg",
#'                   model_id = "outputs/finetuned_maestro.pt")
#' }
finetune_maestro <- function(checkpoint_path, data_dir,
                               output_path = "outputs/finetuned_maestro.pt",
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
    n_classes = 8L,
    device = device_str,
    patience = as.integer(patience)
  )

  message(sprintf("\n  Checkpoint fine-tune: %s", output_path))
  message(sprintf("  Meilleure precision: %.1f%% (epoch %d)",
                  result$best_val_acc, result$best_epoch))

  invisible(result)
}
