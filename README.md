# maestro - Reconnaissance des essences forestieres

Package R pour la reconnaissance automatique des essences forestieres a
partir d'une zone d'interet (`aoi.gpkg`) en utilisant le modele
[MAESTRO](https://github.com/IGNF/maestro) de l'IGNF telecharge depuis
[Hugging Face](https://huggingface.co/IGNF) via le package R
[hfhub](https://cran.r-project.org/package=hfhub).

Les donnees d'entree (orthophotos RVB, IRC et MNT 1m) sont telechargees
automatiquement depuis la [Geoplateforme IGN](https://data.geopf.fr/) via
le service WMS-R (pas de cle API necessaire).

## Installation

```r
# Depuis le depot Git
devtools::install_github("pobsteta/maestro_nemeton")

# Ou localement
devtools::install(".")
```

### Dependances Python

```bash
conda create -n maestro python=3.11 -y
conda activate maestro
pip install torch numpy safetensors
```

## Utilisation rapide

### En tant que package R

```r
library(maestro)

# Pipeline complet en une ligne
maestro_pipeline("data/aoi.gpkg",
                  millesime_ortho = 2023,
                  millesime_irc = 2023)

# Ou etape par etape
aoi   <- load_aoi("data/aoi.gpkg")
ortho <- download_ortho_for_aoi(aoi, "outputs/", millesime_ortho = 2023)
rgbi  <- combine_rvb_irc(ortho$rvb, ortho$irc)
mnt   <- download_mnt_for_aoi(aoi, "outputs/", rgbi = rgbi)
image <- combine_rgbi_mnt(rgbi, mnt$mnt)
# ... inference, export
```

### Composite multi-annuel (multitemporel)

Pour gommer les annees seches et reduire l'impact des nuages, il est
possible de calculer un **composite median** a partir de plusieurs
annees et saisons :

```r
# Composite median sur 3 etes (2022-2024)
maestro_pipeline("data/aoi.gpkg",
                  use_s2 = TRUE, use_s1 = TRUE,
                  annees_sentinel = 2022:2024,
                  saison = "ete")

# Composite sur 5 annees completes
maestro_pipeline("data/aoi.gpkg",
                  use_s2 = TRUE, use_s1 = TRUE,
                  annees_sentinel = 2020:2024,
                  saison = "annee",
                  max_scenes_par_annee = 5)

# Saisons disponibles : "ete", "printemps", "automne", "annee"
# Ou mois personnalises : saison = c(4, 8) pour avril-aout
```

Le composite median est calcule pixel par pixel pour chaque bande.
La mediane est robuste aux valeurs aberrantes (nuages residuels,
secheresse ponctuelle, anomalies radiometriques).

### En ligne de commande

```bash
Rscript inst/scripts/maestro_cli.R --aoi data/aoi.gpkg
Rscript inst/scripts/maestro_cli.R --aoi data/aoi.gpkg \
  --millesime_ortho 2023 --millesime_irc 2023 --gpu

# Composite multi-annuel
Rscript inst/scripts/maestro_cli.R --aoi data/aoi.gpkg \
  --s2 --s1 --annees "2022:2024" --saison ete

# Avec annees specifiques
Rscript inst/scripts/maestro_cli.R --aoi data/aoi.gpkg \
  --s2 --s1 --annees "2021,2023,2024" --saison annee --max_scenes 5
```

### Test du pipeline (sans modele)

```bash
# Test complet (500m x 500m, Fontainebleau)
Rscript inst/scripts/test_pipeline.R

# Avec millesime
Rscript inst/scripts/test_pipeline.R --millesime 2023
```

## Pipeline

```
data/aoi.gpkg
   |
   v
Telechargement IGN (WMS-R Geoplateforme)
   |-- Ortho RVB (0.2m, millesime au choix)
   |-- Ortho IRC (0.2m, millesime au choix)
   |-- DEM : DSM + DTM (1m, reechantillonne a 0.2m)
   |
   v
Combinaison -> RGBI 4 bandes (R, G, B, NIR)
   |
   v
[Optionnel] Telechargement Sentinel (STAC Planetary Computer)
   |-- S2 L2A : 10 bandes spectrales (10m)
   |-- S1 RTC : VV + VH ascending + descending (10m)
   |-- Mode multitemporel : composite median multi-annuel
   |
   v
Decoupage en patches 250x250 px (50m x 50m)
   |
   v
Inference MAESTRO multi-modal (ViT/MAE, 13 classes PureForest)
   |
   v
outputs/
   |-- essences_forestieres.gpkg (carte vectorielle)
   |-- essences_forestieres.tif  (carte raster)
   |-- statistiques_essences.csv
```

## Structure du package

```
maestro_nemeton/
  DESCRIPTION
  NAMESPACE
  LICENSE
  data/                   # Placer aoi.gpkg ici
  R/
    aoi.R            # load_aoi()
    combine.R        # combine_rvb_irc(), combine_rgbi_mnt()
    download_ign.R   # download_ortho_for_aoi(), download_mnt_for_aoi(), ...
    essences.R       # essences_pureforest()
    export.R         # assembler_resultats(), creer_carte_raster()
    inference.R      # configurer_python(), executer_inference()
    model.R          # telecharger_modele(), find_checkpoint_name()
    patches.R        # creer_grille_patches(), extraire_patches_raster()
    pipeline.R       # maestro_pipeline()
    zzz.R            # Configuration interne
  inst/
    python/
      maestro_inference.py    # Module PyTorch (ViT/MAE)
    scripts/
      maestro_cli.R           # Interface ligne de commande
      test_pipeline.R         # Script de test
  man/                        # Documentation (generee par roxygen2)
```

## Fonctions exportees

| Fonction | Description |
|----------|-------------|
| `maestro_pipeline()` | Pipeline complet AOI -> carte des essences |
| `load_aoi()` | Charger un GeoPackage et reprojeter en Lambert-93 |
| `download_ortho_for_aoi()` | Telecharger ortho RVB + IRC depuis IGN |
| `download_mnt_for_aoi()` | Telecharger MNT RGE ALTI 1m depuis IGN |
| `combine_rvb_irc()` | Combiner RVB + IRC en image 4 bandes RGBI |
| `combine_rgbi_mnt()` | Ajouter le MNT comme 5eme bande |
| `telecharger_modele()` | Telecharger le modele depuis Hugging Face |
| `configurer_python()` | Configurer l'environnement Python |
| `creer_grille_patches()` | Creer la grille de patches pour l'inference |
| `extraire_patches_raster()` | Extraire les valeurs des patches |
| `executer_inference()` | Executer l'inference MAESTRO |
| `assembler_resultats()` | Exporter les resultats en GeoPackage + CSV |
| `creer_carte_raster()` | Creer le raster de classification |
| `essences_pureforest()` | Table des 13 classes PureForest |
| `download_s2_for_aoi()` | Telecharger Sentinel-2 (mono-date ou composite) |
| `download_s1_for_aoi()` | Telecharger Sentinel-1 (mono-date ou composite) |
| `build_date_ranges()` | Construire les periodes multi-annuelles |
| `calculer_composite_median()` | Composite median pixel par pixel |
| `aligner_sentinel()` | Aligner un raster Sentinel sur la grille |
| `ign_layer_name()` | Construire un nom de couche WMS IGN |
| `validate_wms_data()` | Verifier la validite d'un raster WMS |

## Options CLI

| Option | Description | Defaut |
|--------|-------------|--------|
| `--aoi` | Fichier GeoPackage | `data/aoi.gpkg` |
| `--output` | Repertoire de sortie | `outputs/` |
| `--model` | Modele Hugging Face | `IGNF/MAESTRO_FLAIR-HUB_base` |
| `--millesime_ortho` | Annee ortho RVB | `NULL` (plus recent) |
| `--millesime_irc` | Annee ortho IRC | `NULL` (plus recent) |
| `--patch_size` | Taille patches (px) | `250` |
| `--resolution` | Resolution (m) | `0.2` |
| `--s2` | Inclure Sentinel-2 | `FALSE` |
| `--s1` | Inclure Sentinel-1 | `FALSE` |
| `--date_sentinel` | Date cible Sentinel | `NULL` (ete) |
| `--annees` | Annees composite (ex: `2022:2024`) | `NULL` (mono-date) |
| `--saison` | Saison composite | `ete` |
| `--max_scenes` | Max scenes/annee composite | `3` |
| `--gpu` | Utiliser CUDA | `FALSE` |
| `--token` | Token Hugging Face | *(env var)* |

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

## Donnees IGN

Les donnees sont telechargees via le WMS-R de la Geoplateforme IGN :

| Donnee | Couche WMS | Resolution |
|--------|-----------|-----------|
| Ortho RVB | `ORTHOIMAGERY.ORTHOPHOTOS[YYYY]` | 0.2 m |
| Ortho IRC | `ORTHOIMAGERY.ORTHOPHOTOS.IRC[.YYYY]` | 0.2 m |
| MNT | `ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES` | 1 m |

- **Pas de cle API** necessaire
- Fallback automatique vers la mosaique la plus recente si le millesime
  n'est pas disponible
- Cache des fichiers telecharges dans le repertoire de sortie
- Tuilage automatique pour les grandes emprises (4096 px max par requete)

Code de telechargement adapte de
[pobsteta/flair_hub_nemeton](https://github.com/pobsteta/flair_hub_nemeton).

## Entrainement GPU sur Scaleway

Le package inclut des scripts pour entrainer un modele fine-tune (TreeSatAI,
8 classes) sur une instance GPU Scaleway.

### Deploiement depuis Windows (PowerShell)

```powershell
# Pre-requis : CLI Scaleway (scw init) + OpenSSH

# Test a blanc (voir les commandes sans executer)
.\inst\scripts\deploy_scaleway.ps1 -DryRun

# Lancer avec un GPU RTX 3070 (defaut, ~2 EUR pour 30 epochs)
.\inst\scripts\deploy_scaleway.ps1

# Avec un GPU L4 pour le multi-modal
.\inst\scripts\deploy_scaleway.ps1 -InstanceType L4-1-24G -Epochs 50

# Recuperer le modele entraine
.\inst\scripts\recover_model.ps1

# Ou avec l'IP directement
.\inst\scripts\recover_model.ps1 -IP 51.15.x.x
```

### Deploiement depuis Linux/macOS (Bash)

```bash
# Test a blanc
bash inst/scripts/deploy_scaleway.sh --dry-run

# Lancer l'entrainement
bash inst/scripts/deploy_scaleway.sh

# Recuperer le modele
bash inst/scripts/recover_model.sh
```

### Prediction apres entrainement

```bash
Rscript inst/scripts/predict_from_checkpoint.R \
    --aoi data/aoi.gpkg \
    --checkpoint outputs/training/maestro_treesatai_best.pt
```

## References

- **MAESTRO** : *MAESTRO: Masked AutoEncoders for Multimodal, Multitemporal,
  and Multispectral Earth Observation Data*
- **PureForest** : *A Large-Scale Aerial Lidar and Aerial Imagery Dataset
  for Tree Species Classification*
- **IGNF sur Hugging Face** : [huggingface.co/IGNF](https://huggingface.co/IGNF)
- **hfhub** : [cran.r-project.org/package=hfhub](https://cran.r-project.org/package=hfhub)
- **Geoplateforme IGN** : [data.geopf.fr](https://data.geopf.fr/)
