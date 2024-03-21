# Application du modèle de prédiction aux données Marketing

# Chargement des packages nécessaires
packages_needed <- c("readr", "randomForest", "caret", "rmarkdown")
new_packages <- packages_needed[!packages_needed %in% installed.packages()[, "Package"]]
if (length(new_packages)) install.packages(new_packages)

# Chargement des packages avec lapply
lapply(packages_needed, require, character.only = TRUE)

# Définition des chemins
chemin_data <- "data/cleaned"
chemin_models <- "models"
chemin_results <- "results"
chemin_reports <- "reports"

# Fonction pour charger des données
load_data <- function(file_name) {
  data_path <- file.path(chemin_data, file_name)
  if (!file.exists(data_path)) {
    stop("Le fichier ", data_path, " n'existe pas.")
  }
  data <- read_csv(data_path)
  return(data)
}

# Chargement des données
donnees_marketing <- load_data("Marketing_sans_accents_clean.csv")

# application du modèle de prédiction
# Chargement du modèle
model_path <- file.path(chemin_models, "decision_tree_model.rds")
model <- readRDS(model_path)

# Prédiction sur les données de marketing
predictions <- predict(model, donnees_marketing, type = "class")

# Ajout des prédictions aux données
donnees_marketing$Categorie <- predictions

# Enregistrement des données avec les prédictions
data_path <- file.path(chemin_results, "Marketing_with_predictions.csv")
write_csv(donnees_marketing, data_path)

# afficher les 6 premières lignes
head(donnees_marketing)