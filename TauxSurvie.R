#' Calcul du taux de survie
#'
#' Calcule le taux de survie selon une variable du jeu TitanicSurvival
#'
#'@author Mierzch
#'
#'@param data Le jeu de données (ex : TitanicSurvival)
#'@param variable Nom de la variable de regroupement (ex : 'sex')
#'@return Un tableau avec le taux de survie pour chaque modalité
#'@export
#'
#'@examples
#'library(carData)
#'taux_survie(TitanicSurvival,'sex')
#'taux_survie(TitanicSurvival,'passengerClass')

taux_survie <- function(data,variable) {
  library(dplyr)
  data %>%
    group_by(data[,variable]) %>%
    summarize(TauxSurvie = sum(survived == "yes") / n())
}