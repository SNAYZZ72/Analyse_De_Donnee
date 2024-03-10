# Application des catégorie identifier dans le script 02_identification_categories.R
# Vérification et installation des packages nécessaires
packages_needed <- c("cluster", "ggplot2", "dplyr", "readr", "data.table", "scales")
packages_to_install <- packages_needed[!(packages_needed %in% installed.packages()[,"Package"])]
if(length(packages_to_install)) install.packages(packages_to_install)

# Chargement des packages
# library(cluster)
# library(ggplot2)
# library(dplyr)
# library(readr)
# library(data.table)
# library(scales)
lapply(packages_needed, library, character.only = TRUE)

# Chemin vers les données nettoyées
chemin_data <- "data/cleaned"

# Fonction pour charger
load_data <- function(file_name) {
  data_path <- file.path(chemin_data, file_name)
  if(!file.exists(data_path)) {
    message("Le fichier ", data_path, " n'existe pas.")
    return(NULL)
  }
  data <- fread(data_path, encoding = "UTF-8")
  return(data)
}

# Charger les données nettoyées
catalogue <- load_data("Catalogue_sans_accents_clean.csv")

# Fonction pour attribuer une catégorie
attribuer_categorie_complexe <- function(catalogue) {
  # Assurer que 'longueur' et 'puissance' sont au format correct (si nécessaire)
  catalogue$longueur <- as.character(catalogue$longueur)
  catalogue$puissance <- as.numeric(as.character(catalogue$puissance))

  # Application des catégories en fonction de critères prédéfinis
  catalogue <- catalogue %>%
    mutate(Categorie = case_when(
      longueur == "courte" & puissance <= 120 ~ "Citadines compactes et abordables",
      longueur %in% c("moyenne", "longue") & puissance >= 120 & puissance <= 180 ~ "Berlines familiales puissantes et confortables",
      longueur == "longue" & nbPlaces <= 5 & puissance <= 250 ~ "Voitures compactes sportives et economiques",
      longueur == "longue" & nbPlaces > 5 & puissance <= 180 ~ "Monospaces familiaux spacieux et abordables",
      longueur %in% c("longue", "tres longue") & puissance > 180 & puissance <= 300 ~ "Voitures de sport puissantes et luxueuses",
      longueur == "tres longue" & puissance > 300 ~ "Autres",
      TRUE ~ "Non specifie"
    ))
  return(catalogue)
}

# Appliquer la fonction étendue pour attribuer les catégories
catalogue <- attribuer_categorie_complexe(catalogue)

# Affichez ou enregistrez votre catalogue avec les catégories attribuées
head(catalogue, 50)

# Affichez la distribution des catégories
ggplot(catalogue, aes(x = factor(Categorie), fill = Categorie)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = "black") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 64, hjust = 1)) +
  labs(title = "Distribution des Categories de Vehicules", x = "Categorie", y = "Nombre de Vehicules")



# Enregistrer le catalogue mis à jour dans un nouveau fichier CSV
# write.csv(catalogue, "data/cleaned/Catalogue_avec_categories.csv", row.names = FALSE)

