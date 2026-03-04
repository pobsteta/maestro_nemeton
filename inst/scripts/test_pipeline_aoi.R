#!/usr/bin/env Rscript
# =============================================================================
# test_pipeline_aoi.R
# Script de test du pipeline MAESTRO sur une AOI de demonstration
#
# Ce script teste chaque etape du pipeline independamment :
#   1. Creation d'une AOI de test (petit rectangle en foret)
#   2. Telechargement des ortho RVB et IRC depuis la Geoplateforme IGN
#   3. Telechargement du MNT RGE ALTI 1m
#   4. Combinaison des bandes (RGBI + MNT → 5 bandes)
#   5. Decoupage en patches et verification des dimensions
#
# L'inference du modele n'est PAS executee (necessite PyTorch).
# Ce script verifie uniquement la partie R (telechargement + preparation).
#
# Utilisation :
#   Rscript inst/scripts/test_pipeline_aoi.R
#   Rscript inst/scripts/test_pipeline_aoi.R --millesime 2023
#   Rscript inst/scripts/test_pipeline_aoi.R --petite_zone
# =============================================================================

# --- Packages ---
for (pkg in c("sf", "terra", "curl", "fs")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

library(sf)
library(terra)
library(curl)
library(fs)

# --- Arguments simples ---
args <- commandArgs(trailingOnly = TRUE)
millesime <- NULL
petite_zone <- FALSE

for (i in seq_along(args)) {
  if (args[i] == "--millesime" && i < length(args)) {
    millesime <- as.integer(args[i + 1])
  }
  if (args[i] == "--petite_zone") {
    petite_zone <- TRUE
  }
}

# =============================================================================
# Sourcer les fonctions du script principal
# =============================================================================

# Detecter le repertoire racine du projet
script_dir <- tryCatch(
  dirname(sys.frame(1)$ofile),
  error = function(e) "."
)
racine <- normalizePath(file.path(script_dir, "..", ".."), mustWork = FALSE)
if (!file.exists(file.path(racine, "maestro_essences.R"))) {
  racine <- "."
}
if (!file.exists(file.path(racine, "maestro_essences.R"))) {
  stop("Impossible de trouver maestro_essences.R. ",
       "Lancez le script depuis la racine du projet.")
}

# Sourcer uniquement les constantes et fonctions (pas le main())
# On redefinit opt pour eviter l'execution de parse_args
message("Chargement des fonctions depuis maestro_essences.R...")

# Extraire les fonctions sans executer le pipeline
env <- new.env(parent = globalenv())

# Definir les constantes IGN directement
env$IGN_WMS_URL      <- "https://data.geopf.fr/wms-r"
env$IGN_LAYER_ORTHO  <- "ORTHOIMAGERY.ORTHOPHOTOS"
env$IGN_LAYER_IRC    <- "ORTHOIMAGERY.ORTHOPHOTOS.IRC"
env$IGN_LAYER_MNT    <- "ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES"
env$WMS_MAX_PX       <- 4096
env$RES_IGN          <- 0.2

# Parser les fonctions du script principal (sans l'executer)
lignes <- readLines(file.path(racine, "maestro_essences.R"))
# Trouver les blocs de fonctions
code_src <- paste(lignes, collapse = "\n")
# Evaluer les fonctions dans l'environnement dedie
# On wrappe dans un tryCatch pour ignorer l'erreur de parse_args/main()
tryCatch(
  source(file.path(racine, "maestro_essences.R"), local = env),
  error = function(e) {
    # Attendu : erreur de parse_args ou main() a la fin du fichier
    message("  (Ignore erreur attendue du source : ", conditionMessage(e), ")")
  }
)

# Recuperer les fonctions
ign_layer_name      <- env$ign_layer_name
download_wms_tile   <- env$download_wms_tile
download_ign_tiled  <- env$download_ign_tiled
validate_wms_data   <- env$validate_wms_data
download_ortho_for_aoi <- env$download_ortho_for_aoi
download_mnt_for_aoi   <- env$download_mnt_for_aoi
combine_rvb_irc     <- env$combine_rvb_irc
combine_rgbi_mnt    <- env$combine_rgbi_mnt
load_aoi            <- env$load_aoi
creer_grille_patches <- env$creer_grille_patches
extraire_patches_raster <- env$extraire_patches_raster

# =============================================================================
# Compteurs de tests
# =============================================================================
n_tests <- 0L
n_pass  <- 0L
n_fail  <- 0L

test_ok <- function(description) {
  n_tests <<- n_tests + 1L
  n_pass  <<- n_pass + 1L
  message(sprintf("  [PASS] %s", description))
}

test_fail <- function(description, detail = "") {
  n_tests <<- n_tests + 1L
  n_fail  <<- n_fail + 1L
  msg <- sprintf("  [FAIL] %s", description)
  if (nchar(detail) > 0) msg <- paste0(msg, " : ", detail)
  message(msg)
}

test_check <- function(condition, description, detail_fail = "") {
  if (isTRUE(condition)) {
    test_ok(description)
  } else {
    test_fail(description, detail_fail)
  }
}

# =============================================================================
# Configuration de la zone de test
# =============================================================================

message("\n========================================================")
message(" TEST DU PIPELINE MAESTRO")
message("========================================================\n")

dossier_test <- file.path(tempdir(), "maestro_test")
dir.create(dossier_test, showWarnings = FALSE, recursive = TRUE)
message(sprintf("Repertoire de test : %s", dossier_test))

# Zone de test : petit carre en foret de Fontainebleau
# En mode --petite_zone : 200m x 200m (rapide)
# Sinon : 500m x 500m
if (petite_zone) {
  largeur <- 200
  message("Mode petite zone : 200m x 200m")
} else {
  largeur <- 500
  message("Zone de test : 500m x 500m")
}

# Foret de Fontainebleau (chenes, hetres, pins)
xmin <- 652400
ymin <- 6812400
xmax <- xmin + largeur
ymax <- ymin + largeur

coords <- matrix(c(
  xmin, ymin,
  xmax, ymin,
  xmax, ymax,
  xmin, ymax,
  xmin, ymin
), ncol = 2, byrow = TRUE)

poly <- st_polygon(list(coords))
aoi <- st_sf(
  nom = "Zone test Fontainebleau",
  geometry = st_sfc(poly, crs = 2154)
)

# Sauvegarder l'AOI de test
aoi_path <- file.path(dossier_test, "aoi_test.gpkg")
st_write(aoi, aoi_path, delete_dsn = TRUE, quiet = TRUE)

# =============================================================================
# TEST 1 : Chargement de l'AOI
# =============================================================================

message("\n--- Test 1 : Chargement de l'AOI ---")

aoi_loaded <- tryCatch(
  load_aoi(aoi_path),
  error = function(e) {
    test_fail("Chargement AOI", e$message)
    NULL
  }
)

if (!is.null(aoi_loaded)) {
  test_check(nrow(aoi_loaded) == 1, "AOI contient 1 entite")
  test_check(st_crs(aoi_loaded)$epsg == 2154, "AOI en Lambert-93")
  bbox <- st_bbox(aoi_loaded)
  test_check(
    abs(bbox["xmax"] - bbox["xmin"] - largeur) < 1,
    sprintf("Largeur AOI = %dm", largeur)
  )
}

# =============================================================================
# TEST 2 : Noms de couches WMS
# =============================================================================

message("\n--- Test 2 : Construction des noms de couches WMS ---")

test_check(
  ign_layer_name("ortho", NULL) == "ORTHOIMAGERY.ORTHOPHOTOS",
  "Ortho sans millesime"
)
test_check(
  ign_layer_name("irc", NULL) == "ORTHOIMAGERY.ORTHOPHOTOS.IRC",
  "IRC sans millesime"
)
test_check(
  ign_layer_name("ortho", 2023) == "ORTHOIMAGERY.ORTHOPHOTOS2023",
  "Ortho millesime 2023"
)
test_check(
  ign_layer_name("irc", 2023) == "ORTHOIMAGERY.ORTHOPHOTOS.IRC.2023",
  "IRC millesime 2023"
)

# =============================================================================
# TEST 3 : Telechargement ortho RVB et IRC
# =============================================================================

message("\n--- Test 3 : Telechargement ortho RVB + IRC ---")
message(sprintf("  Millesime : %s",
                if (is.null(millesime)) "plus recent (defaut)" else millesime))

ortho <- tryCatch(
  download_ortho_for_aoi(aoi_loaded, dossier_test,
                          millesime_ortho = millesime,
                          millesime_irc = millesime),
  error = function(e) {
    test_fail("Telechargement ortho", e$message)
    NULL
  }
)

if (!is.null(ortho)) {
  test_ok("Telechargement ortho reussi")

  # Verifier RVB
  test_check(!is.null(ortho$rvb), "Raster RVB present")
  test_check(nlyr(ortho$rvb) == 3, sprintf("RVB = 3 bandes (obtenu: %d)", nlyr(ortho$rvb)))
  test_check(file.exists(ortho$rvb_path), "Fichier ortho_rvb.tif existe")

  # Verifier IRC
  test_check(!is.null(ortho$irc), "Raster IRC present")
  test_check(nlyr(ortho$irc) == 3, sprintf("IRC = 3 bandes (obtenu: %d)", nlyr(ortho$irc)))
  test_check(file.exists(ortho$irc_path), "Fichier ortho_irc.tif existe")

  # Verifier les donnees (pas toutes a 0/NA)
  test_check(validate_wms_data(ortho$rvb), "RVB contient des donnees valides")
  test_check(validate_wms_data(ortho$irc), "IRC contient des donnees valides")

  # Verifier la resolution
  res_rvb <- res(ortho$rvb)[1]
  test_check(
    abs(res_rvb - 0.2) < 0.01,
    sprintf("Resolution RVB = 0.2m (obtenu: %.3f)", res_rvb)
  )

  # Verifier les dimensions coherentes
  test_check(
    ncol(ortho$rvb) > 0 && nrow(ortho$rvb) > 0,
    sprintf("Dimensions RVB: %d x %d px", ncol(ortho$rvb), nrow(ortho$rvb))
  )
}

# =============================================================================
# TEST 4 : Telechargement MNT
# =============================================================================

message("\n--- Test 4 : Telechargement MNT (RGE ALTI 1m) ---")

# Creer le RGBI d'abord pour le reechantillonnage
rgbi <- NULL
if (!is.null(ortho)) {
  rgbi <- tryCatch(
    combine_rvb_irc(ortho$rvb, ortho$irc),
    error = function(e) {
      test_fail("Combinaison RVB+IRC", e$message)
      NULL
    }
  )
  if (!is.null(rgbi)) {
    test_ok("Combinaison RVB + IRC reussie")
    test_check(nlyr(rgbi) == 4, sprintf("RGBI = 4 bandes (obtenu: %d)", nlyr(rgbi)))
  }
}

mnt_data <- tryCatch(
  download_mnt_for_aoi(aoi_loaded, dossier_test, rgbi = rgbi),
  error = function(e) {
    test_fail("Telechargement MNT", e$message)
    NULL
  }
)

if (!is.null(mnt_data)) {
  test_ok("Telechargement MNT reussi")
  test_check(!is.null(mnt_data$mnt), "Raster MNT present")
  test_check(nlyr(mnt_data$mnt) == 1, sprintf("MNT = 1 bande (obtenu: %d)", nlyr(mnt_data$mnt)))
  test_check(file.exists(mnt_data$mnt_path), "Fichier mnt_1m.tif existe")

  # Verifier que les altitudes sont raisonnables (Fontainebleau ~80-150m)
  mnt_vals <- values(mnt_data$mnt, na.rm = TRUE)
  mnt_vals <- mnt_vals[is.finite(mnt_vals)]
  if (length(mnt_vals) > 0) {
    alt_min <- min(mnt_vals)
    alt_max <- max(mnt_vals)
    test_check(
      alt_min > 0 && alt_max < 500,
      sprintf("Altitudes plausibles : %.0f - %.0f m", alt_min, alt_max)
    )
  }

  # Si RGBI est dispo, verifier l'alignement
  if (!is.null(rgbi)) {
    same_ext <- abs(ext(mnt_data$mnt)[1] - ext(rgbi)[1]) < 1 &&
                abs(ext(mnt_data$mnt)[2] - ext(rgbi)[2]) < 1
    test_check(same_ext, "MNT aligne sur la grille RGBI")
  }
}

# =============================================================================
# TEST 5 : Combinaison RGBI + MNT → 5 bandes
# =============================================================================

message("\n--- Test 5 : Image finale 5 bandes ---")

image_finale <- NULL
if (!is.null(rgbi) && !is.null(mnt_data)) {
  image_finale <- tryCatch(
    combine_rgbi_mnt(rgbi, mnt_data$mnt),
    error = function(e) {
      test_fail("Combinaison RGBI+MNT", e$message)
      NULL
    }
  )

  if (!is.null(image_finale)) {
    test_ok("Combinaison RGBI + MNT reussie")
    test_check(
      nlyr(image_finale) == 5,
      sprintf("Image finale = 5 bandes (obtenu: %d)", nlyr(image_finale))
    )
    test_check(
      all(names(image_finale) == c("Rouge", "Vert", "Bleu", "PIR", "MNT")),
      sprintf("Noms des bandes : %s", paste(names(image_finale), collapse=", "))
    )

    # Sauvegarder
    finale_path <- file.path(dossier_test, "image_finale.tif")
    writeRaster(image_finale, finale_path, overwrite = TRUE, gdal = c("COMPRESS=LZW"))
    test_check(file.exists(finale_path), "Fichier image_finale.tif sauvegarde")

    fi <- file.info(finale_path)
    message(sprintf("    Taille fichier : %.1f Mo", fi$size / 1e6))
  }
}

# =============================================================================
# TEST 6 : Decoupage en patches
# =============================================================================

message("\n--- Test 6 : Grille de patches ---")

taille_patch_m <- 250 * 0.2  # 50m
grille <- tryCatch(
  creer_grille_patches(aoi_loaded, taille_patch_m),
  error = function(e) {
    test_fail("Creation grille", e$message)
    NULL
  }
)

if (!is.null(grille)) {
  test_ok("Grille de patches creee")
  n_attendu <- ceiling(largeur / taille_patch_m)^2
  test_check(
    nrow(grille) > 0,
    sprintf("Nombre de patches : %d", nrow(grille))
  )

  # Verifier l'extraction des patches si l'image finale est disponible
  if (!is.null(image_finale) && nrow(grille) > 0) {
    message("\n--- Test 6b : Extraction des patches raster ---")

    # Tester sur les 3 premiers patches seulement (rapidite)
    grille_sub <- grille[1:min(3, nrow(grille)), ]
    patches <- tryCatch(
      extraire_patches_raster(image_finale, grille_sub, 250),
      error = function(e) {
        test_fail("Extraction patches", e$message)
        NULL
      }
    )

    if (!is.null(patches)) {
      test_ok("Extraction des patches reussie")
      test_check(
        length(patches) == nrow(grille_sub),
        sprintf("Nombre de patches extraits : %d", length(patches))
      )
      # Chaque patch : matrice 250*250 lignes x 5 colonnes
      dims <- dim(patches[[1]])
      test_check(
        dims[1] == 250 * 250 && dims[2] == 5,
        sprintf("Dimensions patch : %d x %d (attendu: %d x %d)",
                dims[1], dims[2], 250*250, 5)
      )
    }
  }
}

# =============================================================================
# TEST 7 : Cache (re-telechargement evite)
# =============================================================================

message("\n--- Test 7 : Cache des fichiers ---")

test_check(file.exists(file.path(dossier_test, "ortho_rvb.tif")), "Cache ortho_rvb.tif")
test_check(file.exists(file.path(dossier_test, "ortho_irc.tif")), "Cache ortho_irc.tif")
test_check(file.exists(file.path(dossier_test, "mnt_1m.tif")), "Cache mnt_1m.tif")

# Verifier que le 2e appel utilise le cache (pas de re-telechargement)
if (!is.null(ortho)) {
  t0 <- proc.time()
  ortho2 <- tryCatch(
    download_ortho_for_aoi(aoi_loaded, dossier_test,
                            millesime_ortho = millesime,
                            millesime_irc = millesime),
    error = function(e) NULL
  )
  dt <- (proc.time() - t0)["elapsed"]
  test_check(
    dt < 2,
    sprintf("Cache ortho : 2e appel en %.1fs (< 2s)", dt)
  )
}

# =============================================================================
# Resume
# =============================================================================

message("\n========================================================")
message(sprintf(" RESULTATS : %d tests, %d PASS, %d FAIL",
                n_tests, n_pass, n_fail))
message("========================================================")

# Lister les fichiers generes
fichiers <- dir_ls(dossier_test, type = "file")
message("\nFichiers generes :")
for (f in fichiers) {
  fi <- file.info(f)
  message(sprintf("  %-30s  %s",
                  basename(f),
                  format(fi$size, big.mark = " ")))
}

message(sprintf("\nRepertoire de test : %s", dossier_test))

if (n_fail > 0) {
  message("\n[ATTENTION] Certains tests ont echoue.")
  quit(status = 1)
} else {
  message("\nTous les tests sont passes !")
  quit(status = 0)
}
