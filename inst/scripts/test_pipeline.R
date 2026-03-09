# =============================================================================
# Test du pipeline MAESTRO multi-modal
# =============================================================================
# Prerequis:
#   - Environnement conda "maestro" avec torch, numpy, safetensors
#   - Package maestro installe: devtools::install()
#   - Un fichier AOI GeoPackage dans data/aoi.gpkg
#
# Usage:
#   Rscript test_pipeline.R
#   ou depuis RStudio: source("test_pipeline.R")
# =============================================================================

library(maestro)

# --- Test 1: Verifier la configuration ---
message("\n=== TEST 1: Configuration Python ===")
configurer_python()

# --- Test 2: Charger le modele via Python ---
message("\n=== TEST 2: Chargement du modele MAESTRO ===")
# En mode dev, utiliser inst/python/ local ; sinon le package installe
py_path <- if (file.exists("inst/python/maestro_inference.py")) {
  normalizePath("inst/python")
} else {
  python_module_path()
}
maestro_py <- reticulate::import_from_path("maestro_inference", path = py_path)
np <- reticulate::import("numpy")

# Telecharger les poids si pas deja fait
fichiers <- telecharger_modele("IGNF/MAESTRO_FLAIR-HUB_base")
chemin_poids <- fichiers$weights %||% fichiers$snapshot

# Test aerial seul
message("\n--- Test aerial seul ---")
modele_aerial <- maestro_py$charger_modele(
  chemin_poids = chemin_poids,
  n_classes = 13L,
  device = "cpu",
  modalites = list("aerial")
)

# Forward pass avec image aleatoire
dummy_aerial <- np$array(np$random$randn(1L, 4L, 256L, 256L), dtype = np$float32)
result <- maestro_py$predire_patch(modele_aerial, dummy_aerial, device = "cpu")
message(sprintf("  Prediction: classe %d (%s)", result$classe, result$essence))
message("  OK!")

# Test aerial + DEM
message("\n--- Test aerial + DEM ---")
modele_multi <- maestro_py$charger_modele(
  chemin_poids = chemin_poids,
  n_classes = 13L,
  device = "cpu",
  modalites = list("aerial", "dem")
)

donnees <- list(
  aerial = np$array(np$random$randn(1L, 4L, 256L, 256L), dtype = np$float32),
  dem = np$array(np$random$randn(1L, 2L, 256L, 256L), dtype = np$float32)
)
result2 <- maestro_py$predire_multimodal(modele_multi, donnees, device = "cpu")
message(sprintf("  Prediction: classe %d (%s)",
                result2$classes[[1]], result2$essences[[1]]))
message("  OK!")

# --- Test 3: Pipeline complet (si AOI disponible) ---
aoi_path <- "data/aoi.gpkg"
if (file.exists(aoi_path)) {
  message("\n=== TEST 3: Pipeline complet ===")
  maestro_pipeline(aoi_path, output_dir = "outputs_test")
} else {
  message("\n=== TEST 3: IGNORE (pas de fichier data/aoi.gpkg) ===")
  message("  Pour tester le pipeline complet, placez un GeoPackage dans data/aoi.gpkg")
  message("  puis relancez ce script.")
}

message("\n=== TOUS LES TESTS PASSES ===")
