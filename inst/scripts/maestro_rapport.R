#!/usr/bin/env Rscript
# =============================================================================
# maestro_rapport.R
# Pipeline complet MAESTRO : telechargement, inference, carte des essences
# et generation du rapport graphique PDF avec patchwork.
#
# Utilisation dans RStudio :
#   source("inst/scripts/maestro_rapport.R")
#
# Ou en ligne de commande :
#   Rscript inst/scripts/maestro_rapport.R
#   Rscript inst/scripts/maestro_rapport.R --millesime 2023
#   Rscript inst/scripts/maestro_rapport.R --gpu
# =============================================================================

# --- Packages requis ---
pkgs_requis <- c("ggplot2", "patchwork", "scales", "sf", "terra", "fs")
for (pkg in pkgs_requis) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Package '%s' requis. Installez-le avec : install.packages('%s')", pkg, pkg))
  }
}

library(ggplot2)
library(patchwork)
library(sf)
library(terra)
library(fs)

# --- Charger le package maestro ---
if (requireNamespace("maestro", quietly = TRUE)) {
  library(maestro)
} else if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".")
} else {
  stop("Le package 'maestro' n'est pas installe.")
}

# =============================================================================
# Arguments
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
millesime <- NULL
utiliser_gpu <- FALSE
aoi_path <- file.path("data", "aoi.gpkg")
output_dir <- "outputs"

for (i in seq_along(args)) {
  if (args[i] == "--millesime" && i < length(args)) {
    millesime <- as.integer(args[i + 1])
  }
  if (args[i] == "--gpu") utiliser_gpu <- TRUE
  if (args[i] == "--aoi" && i < length(args)) aoi_path <- args[i + 1]
  if (args[i] == "--output" && i < length(args)) output_dir <- args[i + 1]
}

dossier_rapport <- file.path(output_dir, "rapport")
dir.create(dossier_rapport, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(aoi_path)) {
  stop("Fichier AOI introuvable : ", aoi_path,
       "\nPlacez votre fichier aoi.gpkg dans le repertoire data/")
}

# =============================================================================
# ETAPE 1 : Pipeline MAESTRO (telechargement + inference)
# =============================================================================

message("\n========================================================")
message(" MAESTRO - Pipeline complet + Rapport graphique")
message("========================================================\n")

# --- 1a. Charger l'AOI ---
message("=== Chargement de l'AOI ===")
aoi <- load_aoi(aoi_path)
bbox <- st_bbox(aoi)
message(sprintf("  AOI : %s", basename(aoi_path)))
message(sprintf("  Emprise : %.0f x %.0f m",
                bbox["xmax"] - bbox["xmin"], bbox["ymax"] - bbox["ymin"]))

# --- 1b. Telecharger ortho RVB + IRC ---
message("\n=== Telechargement ortho RVB + IRC ===")
message(sprintf("  Millesime : %s",
                if (is.null(millesime)) "plus recent (defaut)" else millesime))

ortho <- download_ortho_for_aoi(aoi, output_dir,
                                 millesime_ortho = millesime,
                                 millesime_irc = millesime)

# --- 1c. Combiner RVB + IRC -> RGBI ---
message("\n=== Combinaison RVB + IRC -> RGBI ===")
rgbi <- combine_rvb_irc(ortho$rvb, ortho$irc)

# --- 1d. Telecharger MNT ---
message("\n=== Telechargement MNT ===")
mnt_data <- download_mnt_for_aoi(aoi, output_dir, rgbi = rgbi)

# --- 1e. Image finale 5 bandes ---
message("\n=== Image finale ===")
if (!is.null(mnt_data)) {
  image_finale <- combine_rgbi_mnt(rgbi, mnt_data$mnt)
  n_bands <- 5L
} else {
  message("  MNT non disponible, utilisation RGBI (4 bandes)")
  image_finale <- rgbi
  n_bands <- 4L
}

finale_path <- file.path(output_dir, "image_finale.tif")
writeRaster(image_finale, finale_path, overwrite = TRUE, gdal = c("COMPRESS=LZW"))
message(sprintf("  Image finale : %s (%d bandes)", finale_path, n_bands))

# --- 1f. Grille de patches ---
message("\n=== Grille de patches ===")
patch_size <- 250L
resolution <- 0.2
taille_patch_m <- patch_size * resolution
grille <- creer_grille_patches(aoi, taille_patch_m)

# --- 1g. Inference (si Python disponible) ---
inference_ok <- FALSE
resultats <- NULL

tryCatch({
  message("\n=== Telechargement du modele ===")
  fichiers_modele <- telecharger_modele("IGNF/MAESTRO_FLAIR-HUB_base")

  message("\n=== Configuration Python ===")
  configurer_python()

  message("\n=== Extraction des patches ===")
  patches_data <- extraire_patches_raster(image_finale, grille, patch_size)

  message("\n=== Inference MAESTRO ===")
  predictions <- executer_inference(
    patches_data, fichiers_modele,
    n_classes = 13L, n_bands = n_bands,
    utiliser_gpu = utiliser_gpu
  )

  message("\n=== Assemblage des resultats ===")
  resultats <- assembler_resultats(grille, predictions, dossier_sortie = output_dir)
  raster_carte <- creer_carte_raster(resultats, resolution, output_dir)
  inference_ok <- TRUE

}, error = function(e) {
  message("\n[INFO] Inference non disponible : ", e$message)
  message("  Le rapport sera genere sans la carte des essences.")
  message("  Pour activer l'inference, installez PyTorch dans l'env conda 'maestro'.")
})

# =============================================================================
# ETAPE 2 : Construction des graphiques
# =============================================================================

message("\n========================================================")
message(" GENERATION DU RAPPORT GRAPHIQUE")
message("========================================================\n")

# --- Fonctions utilitaires ---
raster_to_rgb_df <- function(r, maxpix = 500000) {
  ncells <- ncell(r)
  if (ncells > maxpix) {
    fact <- ceiling(sqrt(ncells / maxpix))
    r <- aggregate(r, fact = fact, fun = "mean")
  }
  coords <- xyFromCell(r, 1:ncell(r))
  vals <- values(r)
  df <- data.frame(x = coords[, 1], y = coords[, 2])
  if (ncol(vals) >= 3) {
    rv <- vals[, 1]; gv <- vals[, 2]; bv <- vals[, 3]
    rv[is.na(rv)] <- 0; gv[is.na(gv)] <- 0; bv[is.na(bv)] <- 0
    # Normaliser en 0-1
    if (max(rv, na.rm = TRUE) > 1) rv <- rv / 255
    if (max(gv, na.rm = TRUE) > 1) gv <- gv / 255
    if (max(bv, na.rm = TRUE) > 1) bv <- bv / 255
    rv <- pmin(pmax(rv, 0), 1)
    gv <- pmin(pmax(gv, 0), 1)
    bv <- pmin(pmax(bv, 0), 1)
    df$hex <- rgb(rv, gv, bv)
  }
  df
}

raster_to_df <- function(r, maxpix = 500000) {
  ncells <- ncell(r)
  if (ncells > maxpix) {
    fact <- ceiling(sqrt(ncells / maxpix))
    r <- aggregate(r, fact = fact, fun = "mean")
  }
  coords <- xyFromCell(r, 1:ncell(r))
  vals <- values(r)[, 1]
  data.frame(x = coords[, 1], y = coords[, 2], val = vals)
}

# --- Theme commun ---
theme_carte <- theme_minimal(base_size = 10) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
    plot.subtitle = element_text(size = 8, hjust = 0.5, color = "grey40"),
    legend.position = "right"
  )

# --- 1. Orthophoto RVB ---
message("  1/7 Orthophoto RVB...")
df_rvb <- raster_to_rgb_df(ortho$rvb)
df_rvb <- df_rvb[complete.cases(df_rvb), ]

p_rvb <- ggplot(df_rvb, aes(x = x, y = y, fill = hex)) +
  geom_raster() +
  scale_fill_identity() +
  geom_sf(data = aoi, fill = NA, color = "red", linewidth = 0.6, inherit.aes = FALSE) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]),
           crs = sf::st_crs(2154), default_crs = sf::st_crs(2154)) +
  labs(title = "Orthophoto RVB",
       subtitle = sprintf("%.1f m | %d x %d px",
                          res(ortho$rvb)[1], ncol(ortho$rvb), nrow(ortho$rvb))) +
  theme_carte

# --- 2. Infrarouge couleur ---
message("  2/7 Infrarouge couleur...")
df_irc <- raster_to_rgb_df(ortho$irc)
df_irc <- df_irc[complete.cases(df_irc), ]

p_irc <- ggplot(df_irc, aes(x = x, y = y, fill = hex)) +
  geom_raster() +
  scale_fill_identity() +
  geom_sf(data = aoi, fill = NA, color = "yellow", linewidth = 0.6, inherit.aes = FALSE) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]),
           crs = sf::st_crs(2154), default_crs = sf::st_crs(2154)) +
  labs(title = "Infrarouge couleur (IRC)",
       subtitle = sprintf("%.1f m | %d x %d px",
                          res(ortho$irc)[1], ncol(ortho$irc), nrow(ortho$irc))) +
  theme_carte

# --- 3. MNT ---
message("  3/7 MNT...")
mnt_raster <- if (!is.null(mnt_data)) mnt_data$mnt else image_finale[[n_bands]]
df_mnt <- raster_to_df(mnt_raster)
df_mnt <- df_mnt[complete.cases(df_mnt), ]

p_mnt <- ggplot(df_mnt, aes(x = x, y = y, fill = val)) +
  geom_raster() +
  geom_sf(data = aoi, fill = NA, color = "black", linewidth = 0.6, inherit.aes = FALSE) +
  scale_fill_gradientn(name = "Alt. (m)",
                       colours = terrain.colors(20),
                       na.value = "transparent") +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]),
           crs = sf::st_crs(2154), default_crs = sf::st_crs(2154)) +
  labs(title = "Modele Numerique de Terrain",
       subtitle = sprintf("%.0f - %.0f m",
                          min(df_mnt$val, na.rm = TRUE),
                          max(df_mnt$val, na.rm = TRUE))) +
  theme_carte

# --- 4. NDVI ---
message("  4/7 NDVI...")
pir <- image_finale[[4]]
rouge <- image_finale[[1]]
ndvi <- (pir - rouge) / (pir + rouge + 1e-10)
df_ndvi <- raster_to_df(ndvi)
df_ndvi <- df_ndvi[complete.cases(df_ndvi), ]

p_ndvi <- ggplot(df_ndvi, aes(x = x, y = y, fill = val)) +
  geom_raster() +
  geom_sf(data = aoi, fill = NA, color = "black", linewidth = 0.6, inherit.aes = FALSE) +
  scale_fill_gradientn(
    name = "NDVI",
    colours = c("#d73027", "#fee08b", "#1a9850", "#006837"),
    limits = c(-0.2, 1), oob = scales::squish, na.value = "transparent"
  ) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]),
           crs = sf::st_crs(2154), default_crs = sf::st_crs(2154)) +
  labs(title = "Indice de vegetation (NDVI)",
       subtitle = "Bandes Rouge et PIR") +
  theme_carte

# --- 5. Grille de patches ---
message("  5/7 Grille de patches...")

p_grille <- ggplot() +
  geom_raster(data = df_rvb, aes(x = x, y = y, fill = hex), alpha = 0.5) +
  scale_fill_identity() +
  geom_sf(data = grille, fill = NA, color = "blue", linewidth = 0.3) +
  geom_sf(data = aoi, fill = NA, color = "red", linewidth = 0.8) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]),
           crs = sf::st_crs(2154), default_crs = sf::st_crs(2154)) +
  labs(title = "Grille de patches",
       subtitle = sprintf("%d patches de %g m", nrow(grille), taille_patch_m)) +
  theme_carte

# --- 6. Carte des essences (ou placeholder) ---
message("  6/7 Carte des essences...")

# Palette de couleurs pour les 13 essences PureForest
ess <- essences_pureforest()
palette_essences <- c(
  "Chene decidue"  = "#2ca02c",   # vert fonce
  "Chene vert"     = "#98df8a",   # vert clair
  "Hetre"          = "#ff7f0e",   # orange
  "Chataignier"    = "#d62728",   # rouge
  "Pin maritime"   = "#1f77b4",   # bleu
  "Pin sylvestre"  = "#aec7e8",   # bleu clair
  "Pin laricio/noir" = "#9467bd", # violet
  "Pin d'Alep"     = "#c5b0d5",   # violet clair
  "Epicea"         = "#17becf",   # cyan
  "Sapin"          = "#006400",   # vert sapin
  "Douglas"        = "#8c564b",   # brun
  "Meleze"         = "#e377c2",   # rose
  "Peuplier"       = "#bcbd22"    # jaune-vert
)

if (inference_ok && !is.null(resultats)) {
  p_essences <- ggplot() +
    geom_sf(data = resultats, aes(fill = classe), color = NA) +
    geom_sf(data = aoi, fill = NA, color = "black", linewidth = 0.8) +
    scale_fill_manual(name = "Essence", values = palette_essences, drop = TRUE) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]),
             crs = sf::st_crs(2154), default_crs = sf::st_crs(2154)) +
    labs(title = "Carte des essences forestieres",
         subtitle = sprintf("Modele MAESTRO - %d patches classifies", nrow(resultats))) +
    theme_carte +
    theme(legend.text = element_text(size = 7),
          legend.key.size = unit(0.4, "cm"))
} else {
  p_essences <- ggplot() +
    geom_sf(data = grille, fill = "grey80", color = "grey60", linewidth = 0.2) +
    geom_sf(data = aoi, fill = NA, color = "red", linewidth = 0.8) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]),
             crs = sf::st_crs(2154), default_crs = sf::st_crs(2154)) +
    annotate("label", x = mean(c(bbox["xmin"], bbox["xmax"])),
             y = mean(c(bbox["ymin"], bbox["ymax"])),
             label = "Inference non executee\n(PyTorch requis)",
             size = 3.5, fill = "white", alpha = 0.8) +
    labs(title = "Carte des essences forestieres",
         subtitle = "En attente d'inference") +
    theme_carte
}

# --- 7. Statistiques (barplot ou resume) ---
message("  7/7 Statistiques...")

if (inference_ok && !is.null(resultats)) {
  stats <- as.data.frame(table(resultats$classe))
  names(stats) <- c("Essence", "N")
  stats$Pct <- round(stats$N / sum(stats$N) * 100, 1)
  stats <- stats[order(-stats$N), ]

  p_stats <- ggplot(stats, aes(x = reorder(Essence, N), y = Pct, fill = Essence)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = sprintf("%.1f%%", Pct)), hjust = -0.1, size = 2.8) +
    scale_fill_manual(values = palette_essences) +
    coord_flip(clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = "Repartition des essences",
         subtitle = sprintf("%d patches | %d especes detectees",
                            sum(stats$N), nrow(stats)),
         x = NULL, y = "Proportion (%)") +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      plot.subtitle = element_text(size = 8, hjust = 0.5, color = "grey40"),
      axis.text.y = element_text(size = 7)
    )
} else {
  # Resume textuel si pas d'inference
  fichiers <- dir_ls(output_dir, type = "file", glob = "*.tif")
  info_lines <- sapply(fichiers, function(f) {
    sz <- file.info(f)$size
    taille <- if (sz > 1e6) sprintf("%.1f Mo", sz / 1e6) else sprintf("%.0f Ko", sz / 1e3)
    sprintf("  %s  (%s)", basename(f), taille)
  })

  resume_text <- paste0(
    "MAESTRO - Resume du pipeline\n",
    "==============================\n\n",
    sprintf("AOI : %s\n", basename(aoi_path)),
    sprintf("CRS : EPSG:2154 (Lambert-93)\n"),
    sprintf("Emprise : %.0f x %.0f m\n",
            bbox["xmax"] - bbox["xmin"], bbox["ymax"] - bbox["ymin"]),
    sprintf("Resolution : %.1f m\n", res(ortho$rvb)[1]),
    sprintf("Image finale : %d bandes\n", nlyr(image_finale)),
    sprintf("  (%s)\n", paste(names(image_finale), collapse = ", ")),
    sprintf("Patches : %d\n", nrow(grille)),
    sprintf("Especes : %d classes PureForest\n", nrow(ess)),
    "\n--- Fichiers generes ---\n",
    paste(info_lines, collapse = "\n"),
    "\n\n--- Inference ---\n",
    "Non executee (PyTorch requis)\n",
    "Relancez avec conda activate maestro"
  )

  p_stats <- ggplot() +
    annotate("text", x = 0, y = 0, label = resume_text,
             hjust = 0, vjust = 1, family = "mono", size = 2.8, lineheight = 1.2) +
    xlim(-0.1, 5) + ylim(-8, 0.5) +
    labs(title = "Resume du pipeline") +
    theme_void() +
    theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5))
}

# =============================================================================
# ETAPE 3 : Composition patchwork
# =============================================================================

message("\nAssemblage du rapport patchwork...")

# Layout :
#   Ligne 1 : RVB | IRC | MNT
#   Ligne 2 : NDVI | Grille | Essences
#   Ligne 3 (large) : Statistiques / barplot
rapport <- (p_rvb | p_irc | p_mnt) /
           (p_ndvi | p_grille | p_essences) /
           p_stats +
  plot_layout(heights = c(3, 3, 2)) +
  plot_annotation(
    title = "MAESTRO - Reconnaissance des essences forestieres",
    subtitle = sprintf("AOI : %s | Date : %s | %d patches de %g m",
                       basename(aoi_path), Sys.Date(), nrow(grille), taille_patch_m),
    theme = theme(
      plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey30")
    )
  )

# =============================================================================
# ETAPE 4 : Affichage RStudio + export PDF/PNG
# =============================================================================

message("Affichage dans RStudio...")
print(rapport)

# PDF
pdf_path <- file.path(dossier_rapport, "rapport_maestro.pdf")
ggsave(pdf_path, rapport, width = 16, height = 14, dpi = 150)
message(sprintf("  PDF : %s (%.1f Mo)", pdf_path, file.info(pdf_path)$size / 1e6))

# PNG
png_path <- file.path(dossier_rapport, "rapport_maestro.png")
ggsave(png_path, rapport, width = 16, height = 14, dpi = 200)
message(sprintf("  PNG : %s (%.1f Mo)", png_path, file.info(png_path)$size / 1e6))

message("\n========================================================")
message(" Rapport termine !")
message(sprintf(" Fichiers dans : %s/", dossier_rapport))
message("========================================================")
