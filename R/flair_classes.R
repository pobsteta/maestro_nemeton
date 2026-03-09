# =============================================================================
# Classes d'occupation du sol FLAIR-HUB
# =============================================================================

#' Classes d'occupation du sol CoSIA (19 classes)
#'
#' Table des 19 classes d'occupation du sol du jeu de donnees FLAIR-HUB,
#' issue de la photo-interpretation experte CoSIA (IGN).
#'
#' @return Un data.frame avec les colonnes code, classe, couleur
#' @export
#' @examples
#' cls <- classes_cosia()
#' cls[cls$code <= 5, ]
classes_cosia <- function() {
  data.frame(
    code = 1:19,
    classe = c(
      "Batiment",
      "Zone permeable",
      "Zone impermeable",
      "Sol nu",
      "Eau",
      "Conifere",
      "Feuillu",
      "Broussaille / lande",
      "Vigne",
      "Pelouse / prairie",
      "Culture",
      "Terre labouree",
      "Serre / bache plastique",
      "Piscine",
      "Neige",
      "Coupe forestiere",
      "Mixte (conifere + feuillu)",
      "Ligneux (haie, bosquet)",
      "Verger"
    ),
    couleur = c(
      "#db0e9a", "#938e7b", "#f80c00", "#a97101", "#1553ae",
      "#194a26", "#46e483", "#f3a60d", "#660082", "#55ff00",
      "#fff30d", "#e4df7c", "#3de6eb", "#ffffff", "#c4b5d2",
      "#8ab3a0", "#6b714f", "#c5dc42", "#9999ff"
    ),
    stringsAsFactors = FALSE
  )
}

#' Classes de cultures LPIS/RPG (23 classes)
#'
#' Table des 23 classes de cultures du jeu de donnees FLAIR-HUB,
#' issues du Registre Parcellaire Graphique (RPG / LPIS).
#'
#' @return Un data.frame avec les colonnes code, classe, couleur
#' @export
#' @examples
#' cls <- classes_lpis()
#' head(cls)
classes_lpis <- function() {
  data.frame(
    code = 1:23,
    classe = c(
      "Ble tendre",
      "Mais grain / ensilage",
      "Orge",
      "Autres cereales",
      "Colza",
      "Tournesol",
      "Autres oleagineux",
      "Proteagineux",
      "Plantes a fibres",
      "Semences",
      "Gel / jacheres",
      "Fourrage",
      "Estives / landes",
      "Prairies permanentes",
      "Prairies temporaires",
      "Vergers",
      "Vignes",
      "Fruits a coque",
      "Oliviers",
      "Autres cultures",
      "Legumes / fleurs",
      "Canne a sucre",
      "Divers"
    ),
    couleur = c(
      "#ffff00", "#ff6600", "#d8b56b", "#aa7841", "#00ff00",
      "#ffaa00", "#7faa00", "#00aa7f", "#00ffff", "#007fff",
      "#d2691e", "#7fff00", "#556b2f", "#98fb98", "#90ee90",
      "#ff69b4", "#800080", "#a0522d", "#808000", "#ffa07a",
      "#ff1493", "#00ced1", "#c0c0c0"
    ),
    stringsAsFactors = FALSE
  )
}

#' Modeles FLAIR-HUB pre-entraines disponibles
#'
#' Catalogue des modeles pre-entraines pour la segmentation d'occupation du sol.
#' Les modeles sont heberges sur HuggingFace par l'IGNF.
#'
#' @return Un data.frame avec les colonnes id, architecture, encoder, decoder,
#'   n_bands, supervision, miou
#' @export
#' @examples
#' mods <- flair_models()
#' mods[mods$miou > 60, ]
flair_models <- function() {
  data.frame(
    id = c(
      "IGNF/FLAIR-INC_RGBI_15cl",
      "IGNF/FLAIR-HUB_RGBI_19cl",
      "IGNF/FLAIR-HUB_RGBI-DEM_19cl"
    ),
    architecture = c(
      "ResNet34-UNet",
      "ConvNeXTV2-UPerNet",
      "ConvNeXTV2-UPerNet"
    ),
    encoder = c("resnet34", "convnextv2_nano", "convnextv2_nano"),
    decoder = c("unet", "upernet", "upernet"),
    n_bands = c(4L, 4L, 5L),
    supervision = c("cosia_15cl", "cosia_19cl", "cosia_19cl"),
    miou = c(55.0, 64.1, 65.1),
    stringsAsFactors = FALSE
  )
}
