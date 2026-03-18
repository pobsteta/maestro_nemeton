#!/usr/bin/env Rscript
# =============================================================================
# test_bdforet_local.R
# Script de diagnostic local : telecharge 1 domaine FLAIR + BD Foret,
# labellise, et verifie que le pourcentage de foret est > 0.
#
# Usage :
#   Rscript inst/scripts/test_bdforet_local.R [domaine] [data_dir]
#
# Exemples :
#   Rscript inst/scripts/test_bdforet_local.R D033-2018
#   Rscript inst/scripts/test_bdforet_local.R D063-2019 /tmp/flair_test
#
# Le domaine par defaut est D033-2018 (Gironde), riche en forets.
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
DOMAINE   <- if (length(args) >= 1) args[1] else "D033-2018"
DATA_DIR  <- if (length(args) >= 2) args[2] else file.path(path.expand("~/dev/maestro_nemeton"), "data")

message("=========================================================")
message(" MAESTRO - Diagnostic BD Foret / FLAIR-HUB")
message(sprintf(" Domaine  : %s", DOMAINE))
message(sprintf(" Dossier  : %s", DATA_DIR))
message("=========================================================")
message("")

# --- Charger les fonctions du package ---
pkg_root <- normalizePath(file.path(dirname(sys.frame(1)$ofile %||% "."), "../.."),
                          mustWork = FALSE)
if (!file.exists(file.path(pkg_root, "DESCRIPTION"))) {
  # Fallback : on est peut-etre lance depuis la racine du projet
  pkg_root <- getwd()
}
message(sprintf("Racine du package : %s", pkg_root))

r_files <- list.files(file.path(pkg_root, "R"), pattern = "\\.R$", full.names = TRUE)
if (length(r_files) == 0) {
  stop("Aucun fichier R/ trouve. Lancez ce script depuis la racine du projet maestro_nemeton/")
}
for (f in r_files) {
  tryCatch(source(f, local = FALSE), error = function(e) {
    message(sprintf("  [WARN] Erreur au chargement de %s: %s", basename(f), e$message))
  })
}

# --- Verifier les packages requis ---
required_pkgs <- c("sf", "terra", "curl", "jsonlite")
missing <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
  stop(sprintf("Packages manquants : %s\nInstallez-les avec : install.packages(c('%s'))",
               paste(missing, collapse = ", "),
               paste(missing, collapse = "', '")))
}

# Verifier happign (necessaire pour WFS BD Foret)
if (!requireNamespace("happign", quietly = TRUE)) {
  message("[INFO] Installation du package happign (acces WFS IGN)...")
  install.packages("happign", repos = "https://cloud.r-project.org")
}

# =====================================================================
# ETAPE 1 : Telecharger les patches aeriens FLAIR-HUB
# =====================================================================
message("")
message("=== ETAPE 1/4 : Telechargement patches aeriens FLAIR-HUB ===")

flair_dir <- DATA_DIR
download_flair_subset("aerial", domaine = DOMAINE, data_dir = flair_dir)

aerial_dir <- file.path(flair_dir, "aerial", DOMAINE)
tif_files <- list.files(aerial_dir, pattern = "\\.tif$",
                        full.names = TRUE, recursive = TRUE)
message(sprintf("  -> %d patches aeriens telecharges dans %s", length(tif_files), aerial_dir))

if (length(tif_files) == 0) {
  stop("Aucun patch telecharge. Verifiez le domaine et votre connexion.")
}

# Examiner un patch de reference
ref <- terra::rast(tif_files[1])
message(sprintf("  Patch exemple : %s", basename(tif_files[1])))
message(sprintf("  Dimensions    : %d x %d px, %d bandes", terra::ncol(ref), terra::nrow(ref), terra::nlyr(ref)))
message(sprintf("  Resolution    : %.2f m", terra::res(ref)[1]))
message(sprintf("  CRS           : %s", terra::crs(ref, describe = TRUE)$name))

# =====================================================================
# ETAPE 2 : Calculer la bbox du domaine et inspecter
# =====================================================================
message("")
message("=== ETAPE 2/4 : Calcul de la bbox du domaine ===")

bbox_all <- NULL
for (tif in tif_files) {
  r <- tryCatch(terra::rast(tif), error = function(e) NULL)
  if (is.null(r)) next
  e <- terra::ext(r)
  if (is.null(bbox_all)) {
    bbox_all <- e
  } else {
    bbox_all <- terra::union(bbox_all, e)
  }
}

message(sprintf("  Bbox Lambert-93 : xmin=%.0f ymin=%.0f xmax=%.0f ymax=%.0f",
                terra::xmin(bbox_all), terra::ymin(bbox_all),
                terra::xmax(bbox_all), terra::ymax(bbox_all)))

# Convertir en WGS84 pour verifier visuellement
crs_str <- terra::crs(ref)
bbox_sf <- sf::st_as_sfc(sf::st_bbox(c(
  xmin = terra::xmin(bbox_all), ymin = terra::ymin(bbox_all),
  xmax = terra::xmax(bbox_all), ymax = terra::ymax(bbox_all)
), crs = sf::st_crs(crs_str)))
bbox_wgs84 <- sf::st_transform(bbox_sf, 4326)
bb84 <- sf::st_bbox(bbox_wgs84)
message(sprintf("  Bbox WGS84     : lon=[%.4f, %.4f] lat=[%.4f, %.4f]",
                bb84["xmin"], bb84["xmax"], bb84["ymin"], bb84["ymax"]))

# Superficie couverte
width_km <- (terra::xmax(bbox_all) - terra::xmin(bbox_all)) / 1000
height_km <- (terra::ymax(bbox_all) - terra::ymin(bbox_all)) / 1000
message(sprintf("  Superficie      : ~%.0f x %.0f km", width_km, height_km))

# =====================================================================
# ETAPE 3 : Telecharger la BD Foret V2 et diagnostiquer
# =====================================================================
message("")
message("=== ETAPE 3/4 : Telechargement BD Foret V2 via WFS ===")

aoi_domaine <- sf::st_sf(geometry = bbox_sf)

cache_dir <- file.path(flair_dir, ".cache_bdforet")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

# Supprimer le cache existant pour forcer un re-telechargement
cache_path <- file.path(cache_dir, paste0("bdforet_", DOMAINE, ".gpkg"))
if (file.exists(cache_path)) {
  message(sprintf("  Suppression du cache existant : %s", cache_path))
  file.remove(cache_path)
}

bdforet <- tryCatch({
  download_bdforet_for_aoi(aoi_domaine, cache_dir)
}, error = function(e) {
  message(sprintf("  ERREUR WFS : %s", e$message))
  NULL
})

if (is.null(bdforet) || nrow(bdforet) == 0) {
  message("")
  message("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  message(" PROBLEME : BD Foret V2 vide ou echec WFS")
  message(" -> Tous les labels seront 'non-foret' (classe 9)")
  message(" -> C'est probablement la cause du 0% de foret")
  message("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  message("")
  message("Diagnostics :")
  message("  1. Verifiez que le service WFS IGN est accessible :")
  message("     https://data.geopf.fr/wfs/ows?service=WFS&request=GetCapabilities")
  message("  2. Testez manuellement :")
  message("     library(happign)")
  message(sprintf("     bbox <- sf::st_as_sfc(sf::st_bbox(c(xmin=%.4f, ymin=%.4f, xmax=%.4f, ymax=%.4f), crs=4326))",
                   bb84["xmin"], bb84["ymin"], bb84["xmax"], bb84["ymax"]))
  message("     happign::get_wfs(bbox, 'LANDCOVER.FORESTINVENTORY.V2:formation_vegetale')")
  stop("Arret : impossible de continuer sans BD Foret")
}

message("")
message("--- Diagnostic BD Foret ---")
message(sprintf("  Nombre de polygones : %d", nrow(bdforet)))
message(sprintf("  CRS                 : %s", sf::st_crs(bdforet)$input))
message(sprintf("  Colonnes            : %s", paste(names(bdforet), collapse = ", ")))

# Verifier la colonne code_ndp0
if ("code_ndp0" %in% names(bdforet)) {
  freq <- table(bdforet$code_ndp0)
  total <- sum(freq)
  cls <- classes_ndp0()
  message("")
  message("  Distribution des classes NDP0 (polygones) :")
  for (nm in sort(names(freq))) {
    code <- as.integer(nm)
    n <- as.integer(freq[nm])
    pct <- round(n / total * 100, 1)
    label <- cls$classe[cls$code == code]
    indicateur <- if (code < 9) " <-- FORET" else ""
    message(sprintf("    %d - %-18s : %5d polygones (%5.1f%%)%s",
                     code, label, n, pct, indicateur))
  }

  n_foret <- sum(freq[names(freq) != "9"])
  pct_foret <- round(n_foret / total * 100, 1)
  message(sprintf("\n  TOTAL FORET : %d / %d polygones (%.1f%%)", n_foret, total, pct_foret))

  if (pct_foret == 0) {
    message("")
    message("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    message(" PROBLEME : 0% de polygones forestiers !")
    message(" Causes possibles :")
    message("   - La colonne TFV n'a pas ete correctement identifiee")
    message("   - Les codes TFV ne matchent pas les patterns attendus")
    message("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")

    # Chercher la colonne TFV
    for (col in setdiff(names(bdforet), c("geometry", "code_ndp0"))) {
      vals <- head(unique(as.character(bdforet[[col]])), 10)
      message(sprintf("  Colonne '%s' : %s", col, paste(vals, collapse = ", ")))
    }
  }
} else {
  message("  ATTENTION : colonne code_ndp0 absente !")
  for (col in setdiff(names(bdforet), "geometry")) {
    vals <- head(unique(as.character(bdforet[[col]])), 10)
    message(sprintf("  Colonne '%s' : %s", col, paste(vals, collapse = ", ")))
  }
}

# =====================================================================
# ETAPE 4 : Labelliser quelques patches et verifier
# =====================================================================
message("")
message("=== ETAPE 4/4 : Labellisation de 10 patches (test) ===")

# Prendre un echantillon de 10 patches
n_test <- min(10, length(tif_files))
test_tifs <- tif_files[1:n_test]

label_dir <- file.path(flair_dir, "labels_ndp0", DOMAINE)
dir.create(label_dir, recursive = TRUE, showWarnings = FALSE)

# Nettoyer les geometries problematiques avant toute operation spatiale
# Tester chaque geometrie individuellement car st_make_valid/st_transform
# en bloc echoue si UNE SEULE geometrie contient des coordonnees NA
message("  Nettoyage des geometries...")
n_before <- nrow(bdforet)
keep <- vapply(seq_len(nrow(bdforet)), function(i) {
  tryCatch({
    g <- sf::st_geometry(bdforet[i, ])
    # Tester si st_make_valid + st_transform passe sans erreur
    g_valid <- sf::st_make_valid(g)
    if (sf::st_is_empty(g_valid)) return(FALSE)
    TRUE
  }, error = function(e) FALSE)
}, logical(1))
if (any(!keep)) {
  message(sprintf("  %d geometries problematiques supprimees sur %d",
                   sum(!keep), n_before))
  bdforet <- bdforet[keep, ]
}

# Maintenant st_make_valid et st_transform sont surs
bdforet <- sf::st_make_valid(bdforet)
bdforet <- bdforet[!sf::st_is_empty(bdforet), ]
message(sprintf("  %d geometries valides", nrow(bdforet)))

# Transformer bdforet dans le CRS des patches si necessaire
patch_crs <- sf::st_crs(crs_str)
if (!identical(sf::st_crs(bdforet)$wkt, patch_crs$wkt)) {
  message("  Transformation CRS BD Foret -> CRS patches")
  bdforet <- sf::st_transform(bdforet, patch_crs)
}

results <- data.frame(
  patch = character(0),
  n_pixels = integer(0),
  n_foret = integer(0),
  pct_foret = numeric(0),
  n_intersect = integer(0),
  classes = character(0),
  stringsAsFactors = FALSE
)

for (i in seq_along(test_tifs)) {
  tif <- test_tifs[i]
  patch_name <- tools::file_path_sans_ext(basename(tif))
  label_path <- file.path(label_dir, paste0(patch_name, ".tif"))

  # Lire l'emprise du patch
  ref_patch <- terra::rast(tif)
  ext_patch <- terra::ext(ref_patch)

  # Creer le raster label
  label_rast <- terra::rast(
    xmin = ext_patch[1], xmax = ext_patch[2],
    ymin = ext_patch[3], ymax = ext_patch[4],
    nrows = terra::nrow(ref_patch), ncols = terra::ncol(ref_patch),
    crs = terra::crs(ref_patch)
  )
  terra::values(label_rast) <- 9L  # Non-foret par defaut

  # Intersection avec BD Foret
  patch_bbox <- sf::st_as_sfc(sf::st_bbox(c(
    xmin = ext_patch[1], xmax = ext_patch[2],
    ymin = ext_patch[3], ymax = ext_patch[4]
  ), crs = sf::st_crs(terra::crs(ref_patch))))

  patch_bdforet <- tryCatch({
    sf::st_intersection(bdforet, patch_bbox)
  }, error = function(e) NULL)

  n_intersect <- 0L
  classes_found <- character(0)
  if (!is.null(patch_bdforet) && nrow(patch_bdforet) > 0) {
    n_intersect <- nrow(patch_bdforet)
    # Rasteriser
    bdforet_vect <- terra::vect(patch_bdforet)
    codes <- sort(unique(patch_bdforet$code_ndp0), decreasing = TRUE)
    classes_found <- as.character(codes)
    for (code in codes) {
      mask_poly <- bdforet_vect[bdforet_vect$code_ndp0 == code, ]
      if (length(mask_poly) == 0) next
      layer <- terra::rasterize(mask_poly, label_rast, field = "code_ndp0")
      valid <- !is.na(terra::values(layer))
      vals <- terra::values(label_rast)
      vals[valid] <- terra::values(layer)[valid]
      terra::values(label_rast) <- vals
    }
  }

  # Sauvegarder le label
  names(label_rast) <- "classe_ndp0"
  terra::writeRaster(label_rast, label_path, overwrite = TRUE,
                     datatype = "INT1U", gdal = c("COMPRESS=LZW"))

  # Stats
  vals <- terra::values(label_rast)
  n_pixels <- length(vals)
  n_foret <- sum(vals < 9, na.rm = TRUE)
  pct_foret <- round(n_foret / n_pixels * 100, 1)

  results <- rbind(results, data.frame(
    patch = patch_name,
    n_pixels = n_pixels,
    n_foret = n_foret,
    pct_foret = pct_foret,
    n_intersect = n_intersect,
    classes = paste(classes_found, collapse = ","),
    stringsAsFactors = FALSE
  ))

  status <- if (pct_foret > 5) "OK" else if (pct_foret > 0) "faible" else "VIDE"
  message(sprintf("  [%d/%d] %s : %.1f%% foret (%d polygones BD Foret, classes: %s) [%s]",
                   i, n_test, patch_name, pct_foret, n_intersect,
                   paste(classes_found, collapse = ","), status))
}

# =====================================================================
# RESUME FINAL
# =====================================================================
message("")
message("=========================================================")
message(" RESUME DIAGNOSTIC")
message("=========================================================")
message(sprintf("  Domaine         : %s", DOMAINE))
message(sprintf("  Patches testes  : %d / %d", n_test, length(tif_files)))
message(sprintf("  BD Foret        : %d polygones", nrow(bdforet)))
message("")

n_ok <- sum(results$pct_foret > 5)
n_faible <- sum(results$pct_foret > 0 & results$pct_foret <= 5)
n_vide <- sum(results$pct_foret == 0)

message(sprintf("  Patches avec foret (>5%%)  : %d", n_ok))
message(sprintf("  Patches faibles (0-5%%)    : %d", n_faible))
message(sprintf("  Patches sans foret (0%%)   : %d", n_vide))
message(sprintf("  Foret moyenne             : %.1f%%", mean(results$pct_foret)))
message("")

if (mean(results$pct_foret) > 5) {
  message("  -> RESULTAT : La labellisation fonctionne correctement !")
  message("     Le probleme de 0% foret vient probablement d'ailleurs")
  message("     (pipeline cloud, CRLF, ou organisation des fichiers)")
} else if (mean(results$pct_foret) > 0) {
  message("  -> RESULTAT : La labellisation fonctionne mais peu de foret")
  message("     Essayez un domaine plus forestier (D063-2019 Puy-de-Dome)")
} else {
  message("  -> RESULTAT : PROBLEME CONFIRME - 0% de foret !")
  message("     La labellisation BD Foret ne fonctionne pas.")
  message("     Verifiez les messages d'erreur ci-dessus.")
}

message("")
message(sprintf("  Fichiers de sortie dans : %s", DATA_DIR))
message("=========================================================")
