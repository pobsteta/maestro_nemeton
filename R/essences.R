#' Classes d'essences forestieres PureForest
#'
#' Table des 13 classes d'essences forestieres du jeu de donnees PureForest,
#' utilise pour l'entrainement du modele MAESTRO. Source : BD Foret V2 / IGN.
#'
#' @return Un data.frame avec les colonnes code, classe, nom_latin et type
#' @export
#' @examples
#' ess <- essences_pureforest()
#' ess[ess$type == "feuillu", ]
essences_pureforest <- function() {
  data.frame(
    code = 0:12,
    classe = c(
      "Chene decidue",
      "Chene vert",
      "Hetre",
      "Chataignier",
      "Pin maritime",
      "Pin sylvestre",
      "Pin laricio/noir",
      "Pin d'Alep",
      "Epicea",
      "Sapin",
      "Douglas",
      "Meleze",
      "Peuplier"
    ),
    nom_latin = c(
      "Quercus spp. (deciduous)",
      "Quercus ilex",
      "Fagus sylvatica",
      "Castanea sativa",
      "Pinus pinaster",
      "Pinus sylvestris",
      "Pinus nigra",
      "Pinus halepensis",
      "Picea abies",
      "Abies alba",
      "Pseudotsuga menziesii",
      "Larix spp.",
      "Populus spp."
    ),
    type = c(
      "feuillu", "feuillu", "feuillu", "feuillu",
      "resineux", "resineux", "resineux", "resineux",
      "resineux", "resineux", "resineux", "resineux",
      "feuillu"
    ),
    stringsAsFactors = FALSE
  )
}


#' Classes d'essences forestieres TreeSatAI (7 classes)
#'
#' Table des 7 classes regroupees pour le fine-tuning sur TreeSatAI.
#' Ce schema simplifie les 20 especes TreeSatAI en 7 classes forestieres.
#' Quand le LiDAR sera integre, on passera aux 13 classes PureForest
#' completes. La detection coupe/vide est geree en amont via FLAIR.
#'
#' Regroupement :
#' - Chene : Quercus robur, Q. petraea, Q. rubra
#' - Hetre : Fagus sylvatica
#' - Pin : Pinus sylvestris, P. nigra, P. strobus
#' - Epicea : Picea abies
#' - Douglas/Sapin : Pseudotsuga menziesii, Abies alba (resineux sempervirents sombres)
#' - Meleze : Larix decidua, L. kaempferi (caduc, phenologie distincte)
#' - Feuillus divers : Betula, Populus, Alnus, Fraxinus, Acer, Castanea,
#'   Robinia, Salix, Prunus, Sorbus, Tilia, Carpinus, Taxus
#'
#' @return Un data.frame avec les colonnes code, classe, nom_latin et type
#' @export
#' @examples
#' ess <- essences_treesatai()
#' ess[ess$type == "resineux", ]
essences_treesatai <- function() {
  data.frame(
    code = 0:6,
    classe = c(
      "Chene",
      "Hetre",
      "Pin",
      "Epicea",
      "Douglas/Sapin",
      "Meleze",
      "Feuillus divers"
    ),
    nom_latin = c(
      "Quercus spp.",
      "Fagus sylvatica",
      "Pinus spp.",
      "Picea abies",
      "Pseudotsuga menziesii, Abies alba",
      "Larix spp.",
      "Betula, Populus, Alnus, Fraxinus, Acer, etc."
    ),
    type = c(
      "feuillu", "feuillu",
      "resineux", "resineux", "resineux", "resineux",
      "feuillu"
    ),
    stringsAsFactors = FALSE
  )
}
