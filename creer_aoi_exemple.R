#!/usr/bin/env Rscript
# =============================================================================
# creer_aoi_exemple.R
# Cree un fichier aoi.gpkg d'exemple pour tester le script maestro_essences.R
#
# La zone d'interet est un rectangle de ~1 km2 dans la foret de Fontainebleau
# (Seine-et-Marne, France) en Lambert-93 (EPSG:2154)
# =============================================================================

if (!requireNamespace("sf", quietly = TRUE)) {
  install.packages("sf", repos = "https://cloud.r-project.org")
}

library(sf)

# --- Foret de Fontainebleau (exemple) ---
# Coordonnees en Lambert-93 (EPSG:2154)
# Zone d'environ 1 km x 1 km dans la foret
xmin <- 652000
ymin <- 6812000
xmax <- 653000
ymax <- 6813000

# Creer le polygone
coords <- matrix(c(
  xmin, ymin,
  xmax, ymin,
  xmax, ymax,
  xmin, ymax,
  xmin, ymin
), ncol = 2, byrow = TRUE)

poly <- st_polygon(list(coords))
aoi <- st_sf(
  nom = "Foret de Fontainebleau (exemple)",
  description = "Zone test pour la reconnaissance des essences forestieres",
  geometry = st_sfc(poly, crs = 2154)
)

# Exporter en GeoPackage
st_write(aoi, "aoi.gpkg", delete_dsn = TRUE, quiet = TRUE)

cat("Fichier aoi.gpkg cree avec succes.\n")
cat(sprintf("  Etendue (Lambert-93) : [%d, %d] - [%d, %d]\n", xmin, ymin, xmax, ymax))
cat(sprintf("  Surface : %.2f km2\n", as.numeric(st_area(aoi)) / 1e6))
cat("  CRS : EPSG:2154 (RGF93 / Lambert-93)\n")
