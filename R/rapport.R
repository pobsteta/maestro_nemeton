#' Generer un rapport HTML ou PDF du pipeline MAESTRO
#'
#' Compile le template Rmd fourni avec le package pour produire un rapport
#' complet du pipeline : telechargement des donnees, combinaison, creation
#' des patches, indices spectraux et optionnellement inference.
#'
#' @param aoi_path Chemin vers le fichier GeoPackage de l'AOI.
#' @param output_dir Repertoire de sortie pour les resultats et le rapport
#'   (par defaut `"outputs"`).
#' @param format Format de sortie : `"html"` (par defaut) ou `"pdf"`.
#'   Le format PDF necessite une installation LaTeX (voir
#'   [tinytex::install_tinytex()]).
#' @param output_file Nom du fichier de sortie (optionnel). Par defaut
#'   `"rapport_pipeline_aoi.html"` ou `"rapport_pipeline_aoi.pdf"`.
#' @param millesime Millesime des orthophotos IGN (`NULL` = le plus recent).
#' @param inference Logical. Lancer l'inference MAESTRO ? (defaut `FALSE`).
#' @param gpu Logical. Utiliser le GPU pour l'inference ? (defaut `FALSE`).
#' @param model_id Identifiant du modele sur Hugging Face
#'   (defaut `"IGNF/MAESTRO_FLAIR-HUB_base"`).
#' @param use_s2 Logical. Telecharger et integrer les donnees Sentinel-2 ?
#'   (defaut `FALSE`).
#' @param use_s1 Logical. Telecharger et integrer les donnees Sentinel-1 ?
#'   (defaut `FALSE`).
#' @param date_sentinel Date cible pour les donnees Sentinel (format
#'   `"YYYY-MM-DD"` ou `NULL`).
#' @param open Logical. Ouvrir le rapport dans le navigateur apres generation ?
#'   (defaut `TRUE` en session interactive).
#' @return Le chemin du fichier rapport genere (invisiblement).
#' @export
#' @examples
#' \dontrun{
#' # Rapport HTML simple
#' generer_rapport("data/aoi.gpkg")
#'
#' # Rapport PDF avec inference
#' generer_rapport("data/aoi.gpkg", format = "pdf", inference = TRUE)
#'
#' # Rapport complet avec Sentinel-2 et inference GPU
#' generer_rapport(
#'   aoi_path   = "data/aoi.gpkg",
#'   format     = "html",
#'   inference  = TRUE,
#'   gpu        = TRUE,
#'   use_s2     = TRUE,
#'   date_sentinel = "2024-06-15"
#' )
#' }
generer_rapport <- function(aoi_path,
                            output_dir = "outputs",
                            format = c("html", "pdf"),
                            output_file = NULL,
                            millesime = NULL,
                            inference = FALSE,
                            gpu = FALSE,
                            model_id = "IGNF/MAESTRO_FLAIR-HUB_base",
                            use_s2 = FALSE,
                            use_s1 = FALSE,
                            date_sentinel = NULL,
                            open = interactive()) {

  format <- match.arg(format)
  .check_rmarkdown(format)

  rmd_path <- system.file("scripts/rapport_pipeline_aoi.Rmd",
                           package = "maestro")
  if (!nzchar(rmd_path)) {
    stop("Template Rmd introuvable. Verifiez l'installation du package.",
         call. = FALSE)
  }

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  if (is.null(output_file)) {
    ext <- if (format == "html") ".html" else ".pdf"
    output_file <- paste0("rapport_pipeline_aoi", ext)
  }

  params <- list(
    aoi_path      = normalizePath(aoi_path, mustWork = TRUE),
    output_dir    = normalizePath(output_dir, mustWork = FALSE),
    millesime     = millesime,
    inference     = inference,
    gpu           = gpu,
    model_id      = model_id,
    use_s2        = use_s2,
    use_s1        = use_s1,
    date_sentinel = date_sentinel
  )

  message("Generation du rapport pipeline ", format, " ...")
  message("  AOI      : ", params$aoi_path)
  message("  Sortie   : ", file.path(output_dir, output_file))
  message("  Inference: ", ifelse(inference, "oui", "non"))
  if (use_s2) message("  Sentinel-2: oui")
  if (use_s1) message("  Sentinel-1: oui")

  .render_rapport(rmd_path, format, output_file, output_dir, params, open)
}


#' Generer un rapport de segmentation MAESTRO (HTML ou PDF)
#'
#' Compile le template Rmd de segmentation pour produire un rapport complet :
#' donnees d'entree (RGBI, DEM avec SLOPE/TWI/TPI/ASPECT, Sentinel),
#' indices spectraux (NDVI), carte de segmentation NDP0 a 0.2m,
#' distribution des essences, et analyses topographiques croisees.
#'
#' @param aoi_path Chemin vers le fichier GeoPackage de l'AOI.
#' @param backbone_path Chemin vers le checkpoint MAESTRO (.ckpt).
#' @param decoder_path Chemin vers le decodeur de segmentation (.pt).
#' @param output_dir Repertoire de sortie (defaut `"outputs"`).
#' @param format Format de sortie : `"html"` (par defaut) ou `"pdf"`.
#' @param output_file Nom du fichier de sortie (optionnel).
#' @param millesime_ortho Millesime de l'ortho RVB (`NULL` = plus recent).
#' @param millesime_irc Millesime de l'ortho IRC (`NULL` = plus recent).
#' @param dem_channels Vecteur de 2 canaux DEM parmi `"DSM"`, `"DTM"`,
#'   `"SLOPE"`, `"ASPECT"`, `"TPI"`, `"TWI"` (defaut `c("SLOPE", "TWI")`).
#' @param use_s2 Logical. Inclure Sentinel-2 ? (defaut `FALSE`).
#' @param use_s1 Logical. Inclure Sentinel-1 ? (defaut `FALSE`).
#' @param date_sentinel Date cible pour Sentinel (`NULL`).
#' @param gpu Logical. Utiliser le GPU ? (defaut `FALSE`).
#' @param run_segmentation Logical. Executer la segmentation ? Si `FALSE`,
#'   charge un resultat existant depuis `output_dir` (defaut `TRUE`).
#' @param use_flair Logical. Appliquer la contrainte FLAIR feuillus/resineux
#'   apres la segmentation ? (defaut `FALSE`).
#' @param model_flair Identifiant du modele FLAIR HuggingFace
#'   (defaut `"IGNF/FLAIR-HUB_RGBI_19cl"`).
#' @param open Logical. Ouvrir le rapport apres generation ?
#'   (defaut `TRUE` en session interactive).
#' @return Le chemin du fichier rapport genere (invisiblement).
#' @export
#' @examples
#' \dontrun{
#' # Rapport HTML de segmentation
#' generer_rapport_segmentation(
#'   aoi_path      = "data/aoi.gpkg",
#'   backbone_path = modele$weights,
#'   decoder_path  = "segmenter_ndp0_best.pt"
#' )
#'
#' # Rapport PDF avec DEM DSM+DTM et Sentinel-2
#' generer_rapport_segmentation(
#'   aoi_path      = "data/aoi.gpkg",
#'   backbone_path = modele$weights,
#'   decoder_path  = "segmenter_ndp0_best.pt",
#'   format        = "pdf",
#'   dem_channels  = c("DSM", "DTM"),
#'   use_s2        = TRUE
#' )
#'
#' # Rapport depuis resultats existants (sans re-executer)
#' generer_rapport_segmentation(
#'   aoi_path         = "data/aoi.gpkg",
#'   backbone_path    = NULL,
#'   decoder_path     = "segmenter_ndp0_best.pt",
#'   run_segmentation = FALSE
#' )
#' }
generer_rapport_segmentation <- function(aoi_path,
                                          backbone_path,
                                          decoder_path = "segmenter_ndp0_best.pt",
                                          output_dir = "outputs",
                                          format = c("html", "pdf"),
                                          output_file = NULL,
                                          millesime_ortho = NULL,
                                          millesime_irc = NULL,
                                          dem_channels = c("SLOPE", "TWI"),
                                          use_s2 = FALSE,
                                          use_s1 = FALSE,
                                          date_sentinel = NULL,
                                          gpu = FALSE,
                                          run_segmentation = TRUE,
                                          use_flair = FALSE,
                                          model_flair = "IGNF/FLAIR-INC_rgbi_15cl_resnet34-unet",
                                          open = interactive()) {

  format <- match.arg(format)
  .check_rmarkdown(format)

  rmd_path <- system.file("scripts/rapport_segmentation.Rmd",
                           package = "maestro")
  if (!nzchar(rmd_path)) {
    stop("Template rapport_segmentation.Rmd introuvable. ",
         "Verifiez l'installation du package.", call. = FALSE)
  }

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  if (is.null(output_file)) {
    ext <- if (format == "html") ".html" else ".pdf"
    output_file <- paste0("rapport_segmentation", ext)
  }

  params <- list(
    aoi_path         = normalizePath(aoi_path, mustWork = TRUE),
    output_dir       = normalizePath(output_dir, mustWork = FALSE),
    backbone_path    = if (!is.null(backbone_path)) normalizePath(backbone_path, mustWork = TRUE) else NULL,
    decoder_path     = normalizePath(decoder_path, mustWork = FALSE),
    millesime_ortho  = millesime_ortho,
    millesime_irc    = millesime_irc,
    dem_channels     = dem_channels,
    use_s2           = use_s2,
    use_s1           = use_s1,
    date_sentinel    = date_sentinel,
    gpu              = gpu,
    run_segmentation = run_segmentation,
    use_flair        = use_flair,
    model_flair      = model_flair
  )

  message("Generation du rapport segmentation ", format, " ...")
  message("  AOI         : ", params$aoi_path)
  message("  Sortie      : ", file.path(output_dir, output_file))
  message("  DEM         : ", paste(dem_channels, collapse = " + "))
  message("  Segmentation: ", ifelse(run_segmentation, "oui (execution)", "non (resultats existants)"))
  if (use_flair) message("  FLAIR       : oui (contrainte feuillus/resineux)")
  if (use_s2) message("  Sentinel-2  : oui")
  if (use_s1) message("  Sentinel-1  : oui")

  .render_rapport(rmd_path, format, output_file, output_dir, params, open)
}


# --- Helpers internes ---

# Verifier rmarkdown et tinytex
# @keywords internal
.check_rmarkdown <- function(format) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Le package 'rmarkdown' est requis. Installez-le avec :\n",
         "  install.packages('rmarkdown')", call. = FALSE)
  }
  if (format == "pdf" && !requireNamespace("tinytex", quietly = TRUE)) {
    message("Note : le format PDF necessite LaTeX. ",
            "Installez tinytex avec :\n  install.packages('tinytex')\n",
            "  tinytex::install_tinytex()")
  }
}

# Compiler un template Rmd
# @keywords internal
.render_rapport <- function(rmd_path, format, output_file, output_dir,
                             params, open) {
  output_format <- switch(format,
    html = "html_document",
    pdf  = "pdf_document"
  )

  result_path <- rmarkdown::render(
    input         = rmd_path,
    output_format = output_format,
    output_file   = output_file,
    output_dir    = normalizePath(output_dir, mustWork = FALSE),
    params        = params,
    envir         = new.env(parent = globalenv()),
    quiet         = FALSE
  )

  message("Rapport genere : ", result_path)

  if (open && interactive()) {
    utils::browseURL(result_path)
  }

  invisible(result_path)
}
