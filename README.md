# MAESTRO Nemeton - Reconnaissance des essences forestieres

Reconnaissance automatique des essences forestieres a partir d'images aeriennes
en utilisant le modele [MAESTRO](https://github.com/IGNF/maestro) de l'IGNF
(Institut national de l'information geographique et forestiere) telecharge
depuis [Hugging Face](https://huggingface.co/IGNF) via le package R
[hfhub](https://cran.r-project.org/package=hfhub).

## Essences detectees (13 classes PureForest)

| Code | Essence | Nom latin | Type |
|------|---------|-----------|------|
| 0 | Chene decidue | *Quercus spp.* | Feuillu |
| 1 | Chene vert | *Quercus ilex* | Feuillu |
| 2 | Hetre | *Fagus sylvatica* | Feuillu |
| 3 | Chataignier | *Castanea sativa* | Feuillu |
| 4 | Pin maritime | *Pinus pinaster* | Resineux |
| 5 | Pin sylvestre | *Pinus sylvestris* | Resineux |
| 6 | Pin laricio/noir | *Pinus nigra* | Resineux |
| 7 | Pin d'Alep | *Pinus halepensis* | Resineux |
| 8 | Epicea | *Picea abies* | Resineux |
| 9 | Sapin | *Abies alba* | Resineux |
| 10 | Douglas | *Pseudotsuga menziesii* | Resineux |
| 11 | Meleze | *Larix spp.* | Resineux |
| 12 | Peuplier | *Populus spp.* | Feuillu |

## Pre-requis

### R (>= 4.1)

```r
install.packages(c("hfhub", "sf", "terra", "reticulate", "jsonlite", "optparse"))
```

### Python (>= 3.11)

```bash
pip install torch torchvision numpy rasterio geopandas
# Optionnel pour les poids .safetensors :
pip install safetensors
```

## Donnees d'entree

1. **Zone d'interet** (`aoi.gpkg`) : fichier GeoPackage contenant un ou
   plusieurs polygones delimitant la zone forestiere a analyser.

2. **Raster d'orthophotographie IRC** : image aerienne a haute resolution
   (0.2 m) avec 4 bandes (NIR, Rouge, Vert, Bleu). Telechargeable depuis :
   - [BD ORTHO IGN](https://geoservices.ign.fr/bdortho)
   - [Geoplateforme](https://data.geopf.fr/)

## Utilisation

### 1. Creer une zone d'interet d'exemple

```bash
Rscript creer_aoi_exemple.R
```

### 2. Lancer la reconnaissance des essences

```bash
# Mode demonstration (sans raster, predictions aleatoires)
Rscript maestro_essences.R --aoi aoi.gpkg

# Avec un raster d'orthophotographie
Rscript maestro_essences.R --aoi aoi.gpkg --raster ortho_irc.tif

# Avec GPU
Rscript maestro_essences.R --aoi aoi.gpkg --raster ortho_irc.tif --gpu

# Modele alternatif
Rscript maestro_essences.R --aoi aoi.gpkg --raster ortho_irc.tif \
  --model IGNF/MAESTRO_S2-NAIP-urban_base
```

### Options

| Option | Description | Defaut |
|--------|-------------|--------|
| `--aoi` | Fichier GeoPackage de la zone d'interet | `aoi.gpkg` |
| `--raster` | Raster d'entree (ortho IRC, 4 bandes) | *(optionnel)* |
| `--output` | Repertoire de sortie | `resultats/` |
| `--model` | Identifiant du modele Hugging Face | `IGNF/MAESTRO_FLAIR-HUB_base` |
| `--patch_size` | Taille des patches en pixels | `250` |
| `--resolution` | Resolution spatiale (m) | `0.2` |
| `--gpu` | Utiliser le GPU (CUDA) | `FALSE` |
| `--token` | Token Hugging Face | *(env var)* |

## Sorties

Le script produit dans le repertoire de sortie :

- `essences_forestieres.gpkg` : couche vectorielle avec les essences par patch
- `essences_forestieres.tif` : raster de classification (codes 0-12)
- `statistiques_essences.csv` : statistiques des essences detectees

## Pipeline

```
aoi.gpkg ──> Grille de patches ──> Extraction raster ──> MAESTRO (inference)
                                                              │
              essences_forestieres.gpkg <── Post-traitement <─┘
              essences_forestieres.tif
              statistiques_essences.csv
```

## References

- **MAESTRO** : Labatie et al. (2025), *MAESTRO: Masked AutoEncoders for
  Multimodal, Multitemporal, and Multispectral Earth Observation Data*,
  [arXiv:2508.10894](https://arxiv.org/abs/2508.10894)
- **PureForest** : Gaydon & Roche (2024), *PureForest: A Large-Scale Aerial
  Lidar and Aerial Imagery Dataset for Tree Species Classification*,
  [arXiv:2404.12064](https://arxiv.org/abs/2404.12064)
- **IGNF sur Hugging Face** : [huggingface.co/IGNF](https://huggingface.co/IGNF)
- **hfhub** : [cran.r-project.org/package=hfhub](https://cran.r-project.org/package=hfhub)
