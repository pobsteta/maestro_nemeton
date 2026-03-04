#!/usr/bin/env Rscript
# =============================================================================
# test_pipeline_aoi.R
# Test du pipeline MAESTRO sur une AOI de demonstration
#
# Teste chaque etape independamment :
#   1. Chargement de l'AOI depuis data/aoi.gpkg
#   2. Telechargement des ortho RVB et IRC depuis la Geoplateforme IGN
#   3. Telechargement du MNT RGE ALTI 1m
#   4. Combinaison des bandes (RGBI + MNT -> 5 bandes)
#   5. Decoupage en patches et verification des dimensions
#
# L'inference du modele n'est PAS executee (necessite PyTorch).
# Les resultats sont ecrits dans le repertoire outputs/
#
# Utilisation :
#   Rscript inst/scripts/test_pipeline_aoi.R
#   Rscript inst/scripts/test_pipeline_aoi.R --millesime 2023
# =============================================================================

# --- Charger le package ---
if (requireNamespace("maestro", quietly = TRUE)) {
  library(maestro)
} else {
  # Fallback : charger via devtools si en developpement
  if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".")
  } else {
    stop("Le package 'maestro' n'est pas installe.\n",
         "Installez-le avec : devtools::install() ou R CMD INSTALL .")
  }
}

library(sf)
library(terra)
library(fs)

# --- Arguments simples ---
args <- commandArgs(trailingOnly = TRUE)
millesime <- NULL

for (i in seq_along(args)) {
  if (args[i] == "--millesime" && i < length(args)) {
    millesime <- as.integer(args[i + 1])
  }
}

# =============================================================================
# Compteurs de tests
# =============================================================================
n_tests <- 0L
n_pass  <- 0L
n_fail  <- 0L

test_check <- function(condition, description, detail_fail = "") {
  n_tests <<- n_tests + 1L
  if (isTRUE(condition)) {
    n_pass <<- n_pass + 1L
    message(sprintf("  [PASS] %s", description))
  } else {
    n_fail <<- n_fail + 1L
    msg <- sprintf("  [FAIL] %s", description)
    if (nchar(detail_fail) > 0) msg <- paste0(msg, " : ", detail_fail)
    message(msg)
  }
}

# =============================================================================
# Configuration de la zone de test
# =============================================================================

message("\n========================================================")
message(" TEST DU PIPELINE MAESTRO (package)")
message("========================================================\n")

dossier_test <- "outputs"
dir.create(dossier_test, showWarnings = FALSE, recursive = TRUE)
message(sprintf("Repertoire de test : %s", dossier_test))

aoi_path <- file.path("data", "aoi.gpkg")
if (!file.exists(aoi_path)) {
  stop("Fichier AOI introuvable : ", aoi_path,
       "\nPlacez votre fichier aoi.gpkg dans le repertoire data/")
}
message(sprintf("AOI : %s", aoi_path))

# =============================================================================
# TEST 1 : Chargement de l'AOI
# =============================================================================

message("\n--- Test 1 : Chargement de l'AOI ---")

aoi_loaded <- tryCatch(load_aoi(aoi_path), error = function(e) {
  test_check(FALSE, "Chargement AOI", e$message); NULL
})

if (!is.null(aoi_loaded)) {
  test_check(nrow(aoi_loaded) >= 1, sprintf("AOI contient %d entite(s)", nrow(aoi_loaded)))
  test_check(st_crs(aoi_loaded)$epsg == 2154, "AOI en Lambert-93")
  bbox <- st_bbox(aoi_loaded)
  largeur_aoi <- bbox["xmax"] - bbox["xmin"]
  hauteur_aoi <- bbox["ymax"] - bbox["ymin"]
  message(sprintf("  Emprise AOI : %.0f x %.0f m", largeur_aoi, hauteur_aoi))
  test_check(largeur_aoi > 0 && hauteur_aoi > 0, "Emprise AOI valide")
}

# =============================================================================
# TEST 2 : Noms de couches WMS
# =============================================================================

message("\n--- Test 2 : Noms de couches WMS ---")

test_check(ign_layer_name("ortho", NULL) == "ORTHOIMAGERY.ORTHOPHOTOS",
           "Ortho sans millesime")
test_check(ign_layer_name("irc", NULL) == "ORTHOIMAGERY.ORTHOPHOTOS.IRC",
           "IRC sans millesime")
test_check(ign_layer_name("ortho", 2023) == "ORTHOIMAGERY.ORTHOPHOTOS2023",
           "Ortho millesime 2023")
test_check(ign_layer_name("irc", 2023) == "ORTHOIMAGERY.ORTHOPHOTOS.IRC.2023",
           "IRC millesime 2023")

# =============================================================================
# TEST 3 : Telechargement ortho RVB + IRC
# =============================================================================

message("\n--- Test 3 : Telechargement ortho RVB + IRC ---")
message(sprintf("  Millesime : %s",
                if (is.null(millesime)) "plus recent (defaut)" else millesime))

ortho <- tryCatch(
  download_ortho_for_aoi(aoi_loaded, dossier_test,
                          millesime_ortho = millesime,
                          millesime_irc = millesime),
  error = function(e) {
    test_check(FALSE, "Telechargement ortho", e$message); NULL
  }
)

if (!is.null(ortho)) {
  test_check(TRUE, "Telechargement ortho reussi")
  test_check(nlyr(ortho$rvb) == 3,
             sprintf("RVB = 3 bandes (obtenu: %d)", nlyr(ortho$rvb)))
  test_check(nlyr(ortho$irc) == 3,
             sprintf("IRC = 3 bandes (obtenu: %d)", nlyr(ortho$irc)))
  test_check(file.exists(ortho$rvb_path), "Fichier ortho_rvb.tif existe")
  test_check(file.exists(ortho$irc_path), "Fichier ortho_irc.tif existe")
  test_check(validate_wms_data(ortho$rvb), "RVB contient des donnees valides")
  test_check(validate_wms_data(ortho$irc), "IRC contient des donnees valides")

  res_rvb <- res(ortho$rvb)[1]
  test_check(abs(res_rvb - 0.2) < 0.01,
             sprintf("Resolution RVB = 0.2m (obtenu: %.3f)", res_rvb))
}

# =============================================================================
# TEST 4 : Combinaison RVB + IRC -> RGBI
# =============================================================================

message("\n--- Test 4 : Combinaison RVB + IRC ---")

rgbi <- NULL
if (!is.null(ortho)) {
  rgbi <- tryCatch(combine_rvb_irc(ortho$rvb, ortho$irc),
                    error = function(e) {
                      test_check(FALSE, "Combinaison RVB+IRC", e$message); NULL
                    })
  if (!is.null(rgbi)) {
    test_check(TRUE, "Combinaison RVB + IRC reussie")
    test_check(nlyr(rgbi) == 4,
               sprintf("RGBI = 4 bandes (obtenu: %d)", nlyr(rgbi)))
    test_check(all(names(rgbi) == c("Rouge", "Vert", "Bleu", "PIR")),
               sprintf("Noms: %s", paste(names(rgbi), collapse = ", ")))
  }
}

# =============================================================================
# TEST 5 : Telechargement MNT
# =============================================================================

message("\n--- Test 5 : Telechargement MNT ---")

mnt_data <- tryCatch(
  download_mnt_for_aoi(aoi_loaded, dossier_test, rgbi = rgbi),
  error = function(e) {
    test_check(FALSE, "Telechargement MNT", e$message); NULL
  }
)

if (!is.null(mnt_data)) {
  test_check(TRUE, "Telechargement MNT reussi")
  test_check(nlyr(mnt_data$mnt) == 1,
             sprintf("MNT = 1 bande (obtenu: %d)", nlyr(mnt_data$mnt)))
  test_check(file.exists(mnt_data$mnt_path), "Fichier mnt_1m.tif existe")

  mnt_vals <- values(mnt_data$mnt, na.rm = TRUE)
  mnt_vals <- mnt_vals[is.finite(mnt_vals)]
  if (length(mnt_vals) > 0) {
    alt_min <- min(mnt_vals); alt_max <- max(mnt_vals)
    test_check(alt_min > 0 && alt_max < 500,
               sprintf("Altitudes plausibles : %.0f - %.0f m", alt_min, alt_max))
  }
}

# =============================================================================
# TEST 6 : Image finale 5 bandes
# =============================================================================

message("\n--- Test 6 : Image finale 5 bandes ---")

image_finale <- NULL
if (!is.null(rgbi) && !is.null(mnt_data)) {
  image_finale <- tryCatch(combine_rgbi_mnt(rgbi, mnt_data$mnt),
                            error = function(e) {
                              test_check(FALSE, "RGBI+MNT", e$message); NULL
                            })
  if (!is.null(image_finale)) {
    test_check(TRUE, "Combinaison RGBI + MNT reussie")
    test_check(nlyr(image_finale) == 5,
               sprintf("5 bandes (obtenu: %d)", nlyr(image_finale)))
    test_check(
      all(names(image_finale) == c("Rouge", "Vert", "Bleu", "PIR", "MNT")),
      sprintf("Noms: %s", paste(names(image_finale), collapse = ", "))
    )

    finale_path <- file.path(dossier_test, "image_finale.tif")
    writeRaster(image_finale, finale_path, overwrite = TRUE,
                gdal = c("COMPRESS=LZW"))
    test_check(file.exists(finale_path), "Fichier image_finale.tif sauvegarde")
    fi <- file.info(finale_path)
    message(sprintf("    Taille : %.1f Mo", fi$size / 1e6))
  }
}

# =============================================================================
# TEST 7 : Grille de patches + extraction
# =============================================================================

message("\n--- Test 7 : Grille de patches ---")

taille_patch_m <- 250 * 0.2  # 50m
grille <- tryCatch(creer_grille_patches(aoi_loaded, taille_patch_m),
                    error = function(e) {
                      test_check(FALSE, "Creation grille", e$message); NULL
                    })

if (!is.null(grille)) {
  test_check(TRUE, "Grille de patches creee")
  test_check(nrow(grille) > 0,
             sprintf("Nombre de patches : %d", nrow(grille)))

  if (!is.null(image_finale) && nrow(grille) > 0) {
    message("\n--- Test 7b : Extraction des patches ---")
    grille_sub <- grille[1:min(3, nrow(grille)), ]
    patches <- tryCatch(
      extraire_patches_raster(image_finale, grille_sub, 250),
      error = function(e) {
        test_check(FALSE, "Extraction patches", e$message); NULL
      }
    )

    if (!is.null(patches)) {
      test_check(TRUE, "Extraction reussie")
      test_check(length(patches) == nrow(grille_sub),
                 sprintf("%d patches extraits", length(patches)))
      dims <- dim(patches[[1]])
      test_check(dims[1] == 250 * 250 && dims[2] == 5,
                 sprintf("Dimensions: %d x %d (attendu: %d x %d)",
                         dims[1], dims[2], 250 * 250, 5))
    }
  }
}

# =============================================================================
# TEST 8 : Table des essences
# =============================================================================

message("\n--- Test 8 : Table des essences ---")

ess <- essences_pureforest()
test_check(nrow(ess) == 13, sprintf("13 essences (obtenu: %d)", nrow(ess)))
test_check(all(c("code", "classe", "nom_latin", "type") %in% names(ess)),
           "Colonnes attendues presentes")

# =============================================================================
# TEST 9 : Cache
# =============================================================================

message("\n--- Test 9 : Cache des fichiers ---")

test_check(file.exists(file.path(dossier_test, "ortho_rvb.tif")), "Cache ortho_rvb.tif")
test_check(file.exists(file.path(dossier_test, "ortho_irc.tif")), "Cache ortho_irc.tif")
test_check(file.exists(file.path(dossier_test, "mnt_1m.tif")), "Cache mnt_1m.tif")

if (!is.null(ortho)) {
  t0 <- proc.time()
  ortho2 <- tryCatch(
    download_ortho_for_aoi(aoi_loaded, dossier_test,
                            millesime_ortho = millesime,
                            millesime_irc = millesime),
    error = function(e) NULL
  )
  dt <- (proc.time() - t0)["elapsed"]
  test_check(dt < 2, sprintf("Cache ortho: 2e appel en %.1fs (< 2s)", dt))
}

# =============================================================================
# TEST 10 : Chemin Python du package
# =============================================================================

message("\n--- Test 10 : Module Python ---")

py_path <- tryCatch(python_module_path(), error = function(e) NULL)
if (!is.null(py_path)) {
  test_check(file.exists(file.path(py_path, "maestro_inference.py")),
             "maestro_inference.py trouve dans inst/python/")
} else {
  test_check(FALSE, "python_module_path()",
             "inst/python introuvable (normal en mode devtools::load_all)")
}

# =============================================================================
# Resume
# =============================================================================

message("\n========================================================")
message(sprintf(" RESULTATS : %d tests, %d PASS, %d FAIL",
                n_tests, n_pass, n_fail))
message("========================================================")

fichiers <- dir_ls(dossier_test, type = "file")
message("\nFichiers generes :")
for (f in fichiers) {
  fi <- file.info(f)
  message(sprintf("  %-30s  %s", basename(f), format(fi$size, big.mark = " ")))
}

message(sprintf("\nRepertoire de test : %s", dossier_test))

if (n_fail > 0) {
  message("\n[ATTENTION] Certains tests ont echoue.")
} else {
  message("\nTous les tests sont passes !")
}
