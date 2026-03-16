# =============================================================================
# Telechargement du dataset FLAIR-HUB depuis HuggingFace
# =============================================================================

#' Lister les fichiers d'un dataset HuggingFace
#'
#' Interroge l'API HuggingFace pour lister les fichiers disponibles dans un
#' dataset (ou sous-ensemble).
#'
#' @param repo_id Identifiant du repository HuggingFace (ex: "IGNF/FLAIR-HUB")
#' @param path Sous-chemin optionnel pour filtrer
#' @param token Token HuggingFace (optionnel, pour les repos prives)
#' @return Vecteur de noms de fichiers
#' @export
hf_list_files <- function(repo_id, path = NULL, token = NULL) {
  url <- sprintf("https://huggingface.co/api/datasets/%s/tree/main", repo_id)
  if (!is.null(path)) {
    url <- paste0(url, "/", path)
  }

  h <- curl::new_handle()
  if (!is.null(token)) {
    curl::handle_setheaders(h, Authorization = paste("Bearer", token))
  }

  resp <- tryCatch(
    curl::curl_fetch_memory(url, handle = h),
    error = function(e) {
      warning("Erreur API HuggingFace: ", e$message)
      return(NULL)
    }
  )

  if (is.null(resp) || resp$status_code != 200) {
    warning("Impossible de lister les fichiers (HTTP ", resp$status_code, ")")
    return(character(0))
  }

  items <- jsonlite::fromJSON(rawToChar(resp$content))
  if (is.data.frame(items) && "path" %in% names(items)) {
    return(items$path)
  }
  character(0)
}

#' Telecharger un fichier depuis un dataset HuggingFace
#'
#' @param repo_id Identifiant du repository (ex: "IGNF/FLAIR-HUB")
#' @param filename Nom du fichier dans le repo
#' @param dest_dir Repertoire de destination local
#' @param token Token HuggingFace (optionnel)
#' @return Chemin du fichier telecharge
#' @export
hf_download_file <- function(repo_id, filename, dest_dir, token = NULL) {
  fs::dir_create(dest_dir)

  url <- sprintf("https://huggingface.co/datasets/%s/resolve/main/%s",
                 repo_id, filename)
  dest_file <- file.path(dest_dir, basename(filename))

  if (file.exists(dest_file)) {
    message("  Deja telecharge: ", basename(filename))
    return(dest_file)
  }

  h <- curl::new_handle()
  curl::handle_setopt(h, followlocation = TRUE)
  if (!is.null(token)) {
    curl::handle_setheaders(h, Authorization = paste("Bearer", token))
  }

  message("  Telechargement: ", filename)
  tryCatch(
    curl::curl_download(url, dest_file, handle = h, quiet = TRUE),
    error = function(e) {
      warning("Echec telechargement ", filename, ": ", e$message)
      return(NULL)
    }
  )

  dest_file
}

#' Mapping des noms de modalites vers les suffixes ZIP FLAIR-HUB
#'
#' @return Liste nommee : nom interne -> suffixe dans le nom du ZIP
#' @keywords internal
.flair_modality_mapping <- function() {
  list(
    aerial     = "AERIAL_RGBI",
    dem        = "DEM_ELEV",
    s2         = "SENTINEL2_TS",
    s1_asc     = "SENTINEL1-ASC_TS",
    s1_des     = "SENTINEL1-DESC_TS",
    spot       = "SPOT_RGBI",
    labels_cosia = "AERIAL_LABEL-COSIA",
    labels_lpis  = "ALL_LABEL-LPIS"
  )
}


#' Telecharger un sous-ensemble du dataset FLAIR-HUB
#'
#' Telecharge le fichier ZIP d'une modalite et d'un domaine specifiques
#' du dataset FLAIR-HUB depuis HuggingFace, puis le dezippe.
#'
#' Les fichiers FLAIR-HUB sont organises en archives ZIP nommees
#' `data/DXXX-YYYY_MODALITY.zip` (ex: `data/D033-2018_AERIAL_RGBI.zip`).
#'
#' @param modalite Modalite a telecharger: "aerial", "dem", "s2", "s1_asc",
#'   "s1_des", "spot", "labels_cosia", "labels_lpis"
#' @param domaine Identifiant domaine-annee (ex: "D033-2018"). Utiliser
#'   [lister_domaines_flair()] pour voir les domaines disponibles.
#' @param data_dir Repertoire de base pour les donnees
#' @param repo_id Identifiant du repository (defaut: "IGNF/FLAIR-HUB")
#' @param token Token HuggingFace (optionnel)
#' @return Chemin du repertoire contenant les TIF extraits (invisible)
#' @export
download_flair_subset <- function(modalite, domaine = NULL,
                                   data_dir = "data/flair_hub",
                                   repo_id = "IGNF/FLAIR-HUB",
                                   token = NULL) {
  mod_map <- .flair_modality_mapping()
  if (!modalite %in% names(mod_map)) {
    stop(sprintf("Modalite inconnue: '%s'. Valides: %s",
                 modalite, paste(names(mod_map), collapse = ", ")))
  }
  mod_suffix <- mod_map[[modalite]]

  if (is.null(domaine)) {
    stop("Le parametre 'domaine' est requis (ex: 'D033-2018')")
  }

  # Construire le nom du fichier ZIP
  zip_name <- sprintf("data/%s_%s.zip", domaine, mod_suffix)
  message(sprintf("  Telechargement: %s", zip_name))

  # Repertoire de destination pour le ZIP
  zip_dir <- file.path(data_dir, ".zips")
  dir.create(zip_dir, recursive = TRUE, showWarnings = FALSE)
  zip_path <- file.path(zip_dir, basename(zip_name))

  # Repertoire d'extraction
  extract_dir <- file.path(data_dir, modalite, domaine)

  # Verifier si deja extrait
  if (dir.exists(extract_dir)) {
    tifs <- list.files(extract_dir, pattern = "\\.tif$", recursive = TRUE)
    if (length(tifs) > 0) {
      message(sprintf("  Deja extrait: %d TIF dans %s", length(tifs), extract_dir))
      return(invisible(extract_dir))
    }
  }

  # Telecharger le ZIP
  if (!file.exists(zip_path)) {
    hf_download_file(repo_id, zip_name, zip_dir, token)
  }

  if (!file.exists(zip_path)) {
    warning(sprintf("Echec telechargement: %s", zip_name))
    return(invisible(NULL))
  }

  # Extraire
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
  message(sprintf("  Extraction dans: %s", extract_dir))
  utils::unzip(zip_path, exdir = extract_dir)

  # Compter les TIF extraits
  tifs <- list.files(extract_dir, pattern = "\\.tif$", recursive = TRUE)
  message(sprintf("  %d fichiers TIF extraits", length(tifs)))

  invisible(extract_dir)
}

#' Lister les domaines FLAIR-HUB disponibles
#'
#' Interroge l'API HuggingFace pour lister les domaines geographiques
#' disponibles dans le dataset FLAIR-HUB. Chaque domaine correspond
#' a un departement francais + annee (ex: D001_2019 = Ain 2019).
#'
#' @param repo_id Identifiant du repository HuggingFace
#' @param token Token HuggingFace (optionnel)
#' @return data.frame avec colonnes domaine, departement, annee
#' @export
lister_domaines_flair <- function(repo_id = "IGNF/FLAIR-HUB",
                                    token = NULL) {
  # Mapping des numeros de departement vers les noms
  deps <- c(
    "001" = "Ain", "002" = "Aisne", "003" = "Allier",
    "004" = "Alpes-de-Haute-Provence", "005" = "Hautes-Alpes",
    "006" = "Alpes-Maritimes", "007" = "Ardeche", "008" = "Ardennes",
    "009" = "Ariege", "010" = "Aube", "011" = "Aude",
    "012" = "Aveyron", "013" = "Bouches-du-Rhone", "014" = "Calvados",
    "015" = "Cantal", "016" = "Charente", "017" = "Charente-Maritime",
    "018" = "Cher", "019" = "Correze", "021" = "Cote-d'Or",
    "022" = "Cotes-d'Armor", "023" = "Creuse", "024" = "Dordogne",
    "025" = "Doubs", "026" = "Drome", "027" = "Eure",
    "028" = "Eure-et-Loir", "029" = "Finistere", "030" = "Gard",
    "031" = "Haute-Garonne", "032" = "Gers", "033" = "Gironde",
    "034" = "Herault", "035" = "Ille-et-Vilaine", "036" = "Indre",
    "037" = "Indre-et-Loire", "038" = "Isere", "039" = "Jura",
    "040" = "Landes", "041" = "Loir-et-Cher", "042" = "Loire",
    "043" = "Haute-Loire", "044" = "Loire-Atlantique", "045" = "Loiret",
    "046" = "Lot", "047" = "Lot-et-Garonne", "048" = "Lozere",
    "049" = "Maine-et-Loire", "050" = "Manche", "051" = "Marne",
    "052" = "Haute-Marne", "053" = "Mayenne", "054" = "Meurthe-et-Moselle",
    "055" = "Meuse", "056" = "Morbihan", "057" = "Moselle",
    "058" = "Nievre", "059" = "Nord", "060" = "Oise",
    "061" = "Orne", "062" = "Pas-de-Calais", "063" = "Puy-de-Dome",
    "064" = "Pyrenees-Atlantiques", "065" = "Hautes-Pyrenees",
    "066" = "Pyrenees-Orientales", "067" = "Bas-Rhin", "068" = "Haut-Rhin",
    "069" = "Rhone", "070" = "Haute-Saone", "071" = "Saone-et-Loire",
    "072" = "Sarthe", "073" = "Savoie", "074" = "Haute-Savoie",
    "075" = "Paris", "076" = "Seine-Maritime", "077" = "Seine-et-Marne",
    "078" = "Yvelines", "079" = "Deux-Sevres", "080" = "Somme",
    "081" = "Tarn", "082" = "Tarn-et-Garonne", "083" = "Var",
    "084" = "Vaucluse", "085" = "Vendee", "086" = "Vienne",
    "087" = "Haute-Vienne", "088" = "Vosges", "089" = "Yonne",
    "090" = "Territoire de Belfort", "091" = "Essonne",
    "092" = "Hauts-de-Seine", "093" = "Seine-Saint-Denis",
    "094" = "Val-de-Marne", "095" = "Val-d'Oise"
  )

  # Lister les fichiers ZIP dans data/
  files <- hf_list_files(repo_id, path = "data", token = token)
  if (length(files) == 0) {
    warning("Impossible de lister les domaines FLAIR-HUB")
    return(data.frame())
  }

  # Filtrer les ZIP AERIAL_RGBI pour extraire les domaines
  aerial_zips <- files[grepl("_AERIAL_RGBI\\.zip$", files)]
  # Extraire le domaine-annee : "data/D033-2018_AERIAL_RGBI.zip" -> "D033-2018"
  domaines <- sub("^data/(.+)_AERIAL_RGBI\\.zip$", "\\1", aerial_zips)
  domaines <- sort(domaines)

  # Construire le data.frame
  # Format: D033-2018, D024047-2021, D054057-2018
  result <- data.frame(
    domaine = domaines,
    numero_dep = sub("^D([0-9]{3,6})-.*", "\\1", domaines),
    annee = as.integer(sub("^D[0-9]{3,6}-([0-9]{4})$", "\\1", domaines)),
    stringsAsFactors = FALSE
  )
  result$departement <- deps[result$numero_dep]
  result$departement[is.na(result$departement)] <- "?"

  message(sprintf("  %d domaines FLAIR-HUB disponibles", nrow(result)))
  result
}


#' Domaines FLAIR-HUB recommandes pour l'entrainement segmentation
#'
#' Retourne une selection de domaines couvrant une diversite d'essences
#' forestieres (chenes, hetres, pins, sapins, douglas, chataigniers, etc.)
#' repartis sur differentes regions de France.
#'
#' @param niveau Niveau de couverture : "minimal" (5 domaines, ~500 patches),
#'   "standard" (10 domaines, ~1000 patches), "complet" (20 domaines,
#'   ~2000 patches)
#' @return Vecteur de noms de domaines
#' @export
domaines_recommandes_segmentation <- function(niveau = "standard") {
  # Selection basee sur la couverture des 10 classes NDP0 :
  # - Chene : largement distribue (Centre, Ouest, Sud-Ouest)
  # - Hetre : Nord-Est, montagnes
  # - Chataignier : Massif Central, Perigord, Corse
  # - Pin maritime : Landes, Gironde
  # - Pin sylvestre : Massif Central, Alpes
  # - Epicea/Sapin : Vosges, Jura, Alpes
  # - Douglas : Massif Central
  # - Meleze : Alpes du Sud
  # - Peuplier : vallees fluviales (Loire, Garonne)

  # Noms reels des domaines FLAIR-HUB (format DXXX-YYYY)
  # Certains departements sont groupes (D024047, D025039, D054057)
  minimal <- c(
    "D033-2018",     # Gironde     : pin maritime, chene
    "D063-2019",     # Puy-de-Dome : douglas, hetre, epicea
    "D068-2021",     # Haut-Rhin   : epicea, sapin, hetre (Vosges)
    "D024047-2021",  # Dordogne+Lot-et-Garonne : chataignier, chene, peuplier
    "D077-2021"      # Seine-et-Marne : chene, hetre (Fontainebleau)
  )

  standard <- c(
    minimal,
    "D040-2021",     # Landes      : pin maritime
    "D025039-2020",  # Doubs+Jura  : epicea, sapin, hetre
    "D058-2020",     # Nievre      : chene, hetre, douglas
    "D005-2018",     # Hautes-Alpes: meleze, pin sylvestre
    "D045-2020"      # Loiret      : chene, pin sylvestre, peuplier
  )

  complet <- c(
    standard,
    "D046-2019",     # Lot         : chene, chataignier
    "D038-2021",     # Isere       : epicea, sapin, hetre
    "D015-2020",     # Cantal      : hetre, epicea, douglas
    "D054057-2018",  # Meurthe-et-Moselle+Moselle : hetre, chene
    "D073-2022",     # Savoie      : epicea, meleze
    "D012-2019",     # Aveyron     : chene, pin sylvestre, hetre
    "D067-2021",     # Bas-Rhin    : hetre, sapin, chene
    "D081-2020",     # Tarn        : chene, chataignier, douglas
    "D007-2020",     # Ardeche     : chataignier, pin, chene
    "D064-2021"      # Pyrenees-Atlantiques : hetre, chene, pin
  )

  domaines <- switch(niveau,
    minimal  = minimal,
    standard = standard,
    complet  = complet,
    stop("Niveau inconnu: '", niveau, "'. Utilisez 'minimal', 'standard' ou 'complet'")
  )

  message(sprintf("  %d domaines recommandes (niveau '%s')", length(domaines), niveau))
  message(sprintf("  Domaines: %s", paste(domaines, collapse = ", ")))
  domaines
}


#' Telecharger les domaines FLAIR-HUB recommandes pour la segmentation
#'
#' Telecharge les patches FLAIR-HUB (aerial + modalites choisies) pour
#' les domaines recommandes, couvrant une diversite d'essences forestieres.
#'
#' @param niveau Niveau de couverture : "minimal", "standard", "complet"
#' @param modalites Modalites a telecharger (defaut: `c("aerial", "dem")`)
#' @param data_dir Repertoire de destination
#' @param token Token HuggingFace (optionnel)
#' @return Vecteur de domaines telecharges
#' @export
download_flair_segmentation <- function(niveau = "standard",
                                          modalites = c("aerial", "dem"),
                                          data_dir = "data/flair_hub",
                                          token = NULL) {
  domaines <- domaines_recommandes_segmentation(niveau)

  message(sprintf("\n=== Telechargement FLAIR-HUB : %d domaines x %d modalites ===",
                   length(domaines), length(modalites)))

  for (dom in domaines) {
    for (mod in modalites) {
      tryCatch(
        download_flair_subset(mod, domaine = dom, data_dir = data_dir,
                               token = token),
        error = function(e) {
          warning(sprintf("Echec %s/%s: %s", mod, dom, e$message))
        }
      )
    }
  }

  message(sprintf("\n=== Telechargement termine : %s/ ===", data_dir))
  invisible(domaines)
}


#' Telecharger le toy dataset FLAIR-HUB
#'
#' Telecharge un petit jeu de donnees d'exemple pour tester le pipeline.
#'
#' @param data_dir Repertoire de destination
#' @param token Token HuggingFace (optionnel)
#' @return Chemin du repertoire contenant les donnees
#' @export
download_flair_toy <- function(data_dir = "data/flair_hub_toy",
                                token = NULL) {
  message("=== Telechargement du toy dataset FLAIR-HUB ===")
  fs::dir_create(data_dir)

  # Utiliser un petit domaine d'exemple
  download_flair_subset("aerial", domaine = "D004-2021",
                         data_dir = data_dir)
  download_flair_subset("dem", domaine = "D004-2021",
                         data_dir = data_dir)

  message("Toy dataset telecharge dans: ", data_dir)
  data_dir
}

#' Charger un patch FLAIR-HUB depuis un fichier GeoTIFF
#'
#' @param tif_path Chemin vers le fichier GeoTIFF du patch
#' @return SpatRaster
#' @export
load_flair_patch <- function(tif_path) {
  if (!file.exists(tif_path)) {
    stop("Fichier introuvable: ", tif_path)
  }
  terra::rast(tif_path)
}

#' Scanner les fichiers FLAIR-HUB disponibles localement
#'
#' Explore un repertoire pour detecter les patches FLAIR-HUB
#' et leurs modalites.
#'
#' @param data_dir Repertoire contenant les donnees FLAIR-HUB
#' @return Liste nommee par modalite, chacune contenant les chemins des fichiers
#' @export
scan_flair_files <- function(data_dir = "data/flair_hub") {
  if (!dir.exists(data_dir)) {
    message("Repertoire introuvable: ", data_dir)
    return(list())
  }

  modalites <- c("aerial", "dem", "spot", "s2", "s1_asc", "s1_des",
                 "labels_cosia", "labels_lpis", "labels_ndp0")

  result <- list()
  for (mod in modalites) {
    mod_dir <- file.path(data_dir, mod)
    if (dir.exists(mod_dir)) {
      files <- list.files(mod_dir, pattern = "\\.tif$",
                          recursive = TRUE, full.names = TRUE)
      if (length(files) > 0) {
        result[[mod]] <- files
        message(sprintf("  %s: %d fichiers", mod, length(files)))
      }
    }
  }

  if (length(result) == 0) {
    message("Aucun fichier FLAIR-HUB trouve dans: ", data_dir)
  }

  result
}
