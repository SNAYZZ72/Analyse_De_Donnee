# Application des catégorie identifier dans le script 02_identification_categories.R
# Vérification et installation des packages nécessaires
packages_needed <- c("cluster", "ggplot2", "dplyr")
packages_to_install <- packages_needed[!(packages_needed %in% installed.packages()[,"Package"])]
if(length(packages_to_install)) install.packages(packages_to_install)

# Chargement des packages
library(cluster)
library(ggplot2)
library(dplyr)
library(readr)

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
        longueur == "courte" ~ "Citadines compactes et abordables",
        longueur == "moyenne" ~ "Berlines familiales puissantes et confortables",
        longueur == "longue" & nbPlaces == 5 & puissance < 180 ~ "Voitures compactes sportives et economiques",
        longueur == "longue" & nbPlaces == 7 & puissance < 180 ~ "Monospaces familiaux spacieux et abordables",
        (longueur == "longue" | longueur == "tres longue") & puissance > 180 & puissance < 300 ~ "Voitures de sport puissantes et luxueuses",
        longueur == "tres longue" & puissance > 300 ~ "Autres",
        TRUE ~ "Non spécifié"
        ))
  return(catalogue)
}

# Appliquer la fonction étendue pour attribuer les catégories
catalogue <- attribuer_categorie_complexe(catalogue)

# Affichez ou enregistrez votre catalogue avec les catégories attribuées
head(catalogue, 50)

# Affichez la distribution des catégories
ggplot(catalogue, aes(x = Categorie)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Enregistrer le catalogue mis à jour dans un nouveau fichier CSV
# write.csv(catalogue, "data/cleaned/Catalogue_avec_categories.csv", row.names = FALSE)

