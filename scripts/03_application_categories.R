# Application des catégorie identifier dans le script 02_identification_categories.R
# Vérification et installation des packages nécessaires
packages_needed <- c("cluster", "ggplot2", "dplyr")
packages_to_install <- packages_needed[!(packages_needed %in% installed.packages()[,"Package"])]
if(length(packages_to_install)) install.packages(packages_to_install)


# Chargement des packages
library(cluster)
library(ggplot2)
library(dplyr)



# Chemin vers les données nettoyées
chemin_data <- "data/cleaned"

# Fonction pour charger
load_data <- function(file_name) {
  data_path <- file.path(chemin_data, file_name)
  if(!file.exists(data_path)) {
    message("Le fichier ", data_path, " n'existe pas.")
    return(NULL)
  }
  data <- read.csv(data_path, header = TRUE, sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")

  return(data)
}

# Charger les données nettoyées
catalogue <- load_data("Catalogue_sans_accents_clean.csv")


# Fonction pour attribuer une catégorie
attribuer_categorie_complexe <- function(catalogue) {
  catalogue <- catalogue %>%
    mutate(Categorie = case_when(
      puissance < 100 & prix <= 15000 ~ "Citadines compactes et abordables",
      puissance >= 100 & puissance < 200 & prix > 15000 & prix <= 30000 ~ "Berlines familiales puissantes et confortables",
      puissance >= 100 & puissance < 200 & prix <= 20000 ~ "Voitures compactes sportives et économiques",
      puissance >= 200 & prix > 30000 ~ "Voitures de sport puissantes et luxueuses",
      nbPlaces >= 5 & prix <= 25000 ~ "Monospaces familiaux spacieux et abordables",
      TRUE ~ "Autre"
    ))
  return(catalogue)
}

# Appliquer la fonction étendue pour attribuer les catégories
catalogue <- attribuer_categorie_complexe(catalogue)

# Affichez ou enregistrez votre catalogue avec les catégories attribuées
head(catalogue, 50)
