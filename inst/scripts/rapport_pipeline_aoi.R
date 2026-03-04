#!/usr/bin/env Rscript
# =============================================================================
# rapport_pipeline_aoi.R
# Rapport graphique du pipeline MAESTRO avec patchwork
#
# Genere un rapport PDF et un affichage RStudio des donnees produites
# par test_pipeline_aoi.R (repertoire outputs/).
#
# Utilisation :
#   source("inst/scripts/rapport_pipeline_aoi.R")
# =============================================================================

# --- Packages necessaires ---
for (pkg in c("ggplot2", "patchwork", "terra", "sf", "fs")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Package '%s' requis. Installez-le avec : install.packages('%s')", pkg, pkg))
  }
}

library(ggplot2)
library(patchwork)
library(terra)
library(sf)
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
# Configuration
# =============================================================================

dossier_test <- "outputs"
dossier_rapport <- "outputs/rapport"
dir.create(dossier_rapport, showWarnings = FALSE, recursive = TRUE)

aoi_path <- file.path("data", "aoi.gpkg")

# Verifier que les fichiers existent
fichiers_requis <- c("ortho_rvb.tif", "ortho_irc.tif", "mnt_1m.tif", "image_finale.tif")
fichiers_presents <- file.exists(file.path(dossier_test, fichiers_requis))

if (!all(fichiers_presents)) {
  manquants <- fichiers_requis[!fichiers_presents]
  stop("Fichiers manquants dans outputs/ : ", paste(manquants, collapse = ", "),
       "\nExecutez d'abord : source('inst/scripts/test_pipeline_aoi.R')")
}

message("\n========================================================")
message(" RAPPORT GRAPHIQUE MAESTRO")
message("========================================================\n")

# =============================================================================
# Chargement des donnees
# =============================================================================

message("Chargement des rasters...")
ortho_rvb <- rast(file.path(dossier_test, "ortho_rvb.tif"))
ortho_irc <- rast(file.path(dossier_test, "ortho_irc.tif"))
mnt       <- rast(file.path(dossier_test, "mnt_1m.tif"))
img_finale <- rast(file.path(dossier_test, "image_finale.tif"))

aoi <- st_read(aoi_path, quiet = TRUE)
if (st_crs(aoi)$epsg != 2154) {
  aoi <- st_transform(aoi, 2154)
}

# =============================================================================
# Fonctions utilitaires
# =============================================================================

#' Convertir un SpatRaster RGB en data.frame pour ggplot
raster_to_rgb_df <- function(r, maxpix = 500000) {
  # Sous-echantillonner si trop grand
  ncells <- ncell(r)
  if (ncells > maxpix) {
    fact <- ceiling(sqrt(ncells / maxpix))
    r <- aggregate(r, fact = fact, fun = "mean")
  }
  coords <- xyFromCell(r, 1:ncell(r))
  vals <- values(r)
  df <- data.frame(x = coords[, 1], y = coords[, 2])

  if (ncol(vals) >= 3) {
    df$r <- vals[, 1]
    df$g <- vals[, 2]
    df$b <- vals[, 3]
    # Normaliser en 0-1
    for (col in c("r", "g", "b")) {
      v <- df[[col]]
      v[is.na(v)] <- 0
      vmax <- max(v, na.rm = TRUE)
      if (vmax > 1) df[[col]] <- v / 255
    }
  }
  df
}

#' Convertir un SpatRaster mono-bande en data.frame
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

# =============================================================================
# Construction des graphiques
# =============================================================================

message("Creation des graphiques...")

# --- Theme commun ---
theme_carte <- theme_minimal(base_size = 10) +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size = 6),
    plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
    plot.subtitle = element_text(size = 8, hjust = 0.5, color = "grey40"),
    legend.position = "right"
  )

# --- AOI bbox pour coord_sf ---
bbox <- st_bbox(aoi)

# --- 1. Orthophoto RVB ---
message("  1/6 Orthophoto RVB...")
df_rvb <- raster_to_rgb_df(ortho_rvb)
df_rvb <- df_rvb[complete.cases(df_rvb), ]

p_rvb <- ggplot(df_rvb, aes(x = x, y = y)) +
  geom_raster(fill = rgb(df_rvb$r, df_rvb$g, df_rvb$b)) +
  geom_sf(data = aoi, fill = NA, color = "red", linewidth = 0.6, inherit.aes = FALSE) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]), crs = 2154) +
  labs(title = "Orthophoto RVB",
       subtitle = sprintf("%.1f m resolution | %d x %d px",
                          res(ortho_rvb)[1], ncol(ortho_rvb), nrow(ortho_rvb))) +
  theme_carte

# --- 2. Infrarouge couleur (fausse couleur PIR-R-V) ---
message("  2/6 Infrarouge couleur...")
df_irc <- raster_to_rgb_df(ortho_irc)
df_irc <- df_irc[complete.cases(df_irc), ]

p_irc <- ggplot(df_irc, aes(x = x, y = y)) +
  geom_raster(fill = rgb(df_irc$r, df_irc$g, df_irc$b)) +
  geom_sf(data = aoi, fill = NA, color = "yellow", linewidth = 0.6, inherit.aes = FALSE) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]), crs = 2154) +
  labs(title = "Infrarouge couleur (IRC)",
       subtitle = sprintf("%.1f m resolution | %d x %d px",
                          res(ortho_irc)[1], ncol(ortho_irc), nrow(ortho_irc))) +
  theme_carte

# --- 3. MNT ---
message("  3/6 MNT...")
df_mnt <- raster_to_df(mnt)
df_mnt <- df_mnt[complete.cases(df_mnt), ]

p_mnt <- ggplot(df_mnt, aes(x = x, y = y, fill = val)) +
  geom_raster() +
  geom_sf(data = aoi, fill = NA, color = "black", linewidth = 0.6, inherit.aes = FALSE) +
  scale_fill_viridis_c(name = "Altitude (m)", option = "terrain",
                       na.value = "transparent") +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]), crs = 2154) +
  labs(title = "Modele Numerique de Terrain",
       subtitle = sprintf("Alt. %.0f - %.0f m",
                          min(df_mnt$val, na.rm = TRUE),
                          max(df_mnt$val, na.rm = TRUE))) +
  theme_carte

# --- 4. NDVI (PIR - R) / (PIR + R) ---
message("  4/6 NDVI...")
pir <- img_finale[[4]]  # PIR
rouge <- img_finale[[1]]  # Rouge
ndvi <- (pir - rouge) / (pir + rouge + 1e-10)
df_ndvi <- raster_to_df(ndvi)
df_ndvi <- df_ndvi[complete.cases(df_ndvi), ]

p_ndvi <- ggplot(df_ndvi, aes(x = x, y = y, fill = val)) +
  geom_raster() +
  geom_sf(data = aoi, fill = NA, color = "black", linewidth = 0.6, inherit.aes = FALSE) +
  scale_fill_gradientn(
    name = "NDVI",
    colours = c("#d73027", "#fee08b", "#1a9850", "#006837"),
    limits = c(-0.2, 1), oob = scales::squish,
    na.value = "transparent"
  ) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]), crs = 2154) +
  labs(title = "Indice de vegetation (NDVI)",
       subtitle = "Calcule depuis les bandes Rouge et PIR") +
  theme_carte

# --- 5. Grille de patches ---
message("  5/6 Grille de patches...")
taille_patch_m <- 250 * 0.2
grille <- creer_grille_patches(aoi, taille_patch_m)

p_grille <- ggplot() +
  geom_raster(data = df_rvb, aes(x = x, y = y),
              fill = rgb(df_rvb$r, df_rvb$g, df_rvb$b), alpha = 0.5) +
  geom_sf(data = grille, fill = NA, color = "blue", linewidth = 0.3) +
  geom_sf(data = aoi, fill = NA, color = "red", linewidth = 0.8) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]), crs = 2154) +
  labs(title = "Grille de patches",
       subtitle = sprintf("%d patches de %g m (250x250 px)",
                          nrow(grille), taille_patch_m)) +
  theme_carte

# --- 6. Tableau resume ---
message("  6/6 Tableau resume...")

# Infos sur les fichiers
fichiers <- dir_ls(dossier_test, type = "file", glob = "*.tif")
info_df <- data.frame(
  Fichier = basename(fichiers),
  Taille = sapply(fichiers, function(f) {
    sz <- file.info(f)$size
    if (sz > 1e6) sprintf("%.1f Mo", sz / 1e6) else sprintf("%.0f Ko", sz / 1e3)
  }),
  stringsAsFactors = FALSE
)
rownames(info_df) <- NULL

# Stats sur l'image finale
ess <- essences_pureforest()
resume_text <- paste0(
  "MAESTRO Pipeline - Rapport\n",
  "================================\n\n",
  sprintf("AOI : %s\n", basename(aoi_path)),
  sprintf("CRS : EPSG:2154 (Lambert-93)\n"),
  sprintf("Emprise : %.0f x %.0f m\n",
          bbox["xmax"] - bbox["xmin"], bbox["ymax"] - bbox["ymin"]),
  sprintf("Resolution : %.1f m\n", res(ortho_rvb)[1]),
  sprintf("Image finale : %d bandes (%s)\n",
          nlyr(img_finale), paste(names(img_finale), collapse = ", ")),
  sprintf("Patches : %d (%.0f x %.0f m)\n",
          nrow(grille), taille_patch_m, taille_patch_m),
  sprintf("Especes : %d classes PureForest\n", nrow(ess)),
  "\n--- Fichiers generes ---\n",
  paste(sprintf("  %s  (%s)", info_df$Fichier, info_df$Taille), collapse = "\n")
)

p_resume <- ggplot() +
  annotate("text", x = 0, y = 0, label = resume_text,
           hjust = 0, vjust = 1, family = "mono", size = 3, lineheight = 1.2) +
  xlim(-0.1, 5) + ylim(-6, 0.5) +
  labs(title = "Resume du pipeline") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5))

# =============================================================================
# Composition patchwork
# =============================================================================

message("\nAssemblage patchwork...")

rapport <- (p_rvb | p_irc | p_mnt) /
           (p_ndvi | p_grille | p_resume) +
  plot_annotation(
    title = "MAESTRO - Rapport du pipeline de reconnaissance forestiere",
    subtitle = sprintf("AOI : %s | %s", basename(aoi_path), Sys.Date()),
    theme = theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey30")
    )
  )

# =============================================================================
# Affichage RStudio
# =============================================================================

message("Affichage dans RStudio...")
print(rapport)

# =============================================================================
# Export PDF
# =============================================================================

pdf_path <- file.path(dossier_rapport, "rapport_maestro.pdf")
message(sprintf("Export PDF : %s", pdf_path))

ggsave(pdf_path, rapport, width = 16, height = 10, dpi = 150)

message(sprintf("\nRapport PDF genere : %s", pdf_path))
message(sprintf("Taille : %.1f Mo", file.info(pdf_path)$size / 1e6))

# Export PNG aussi pour apercu rapide
png_path <- file.path(dossier_rapport, "rapport_maestro.png")
ggsave(png_path, rapport, width = 16, height = 10, dpi = 200)
message(sprintf("Apercu PNG : %s", png_path))

message("\nTermine !")
