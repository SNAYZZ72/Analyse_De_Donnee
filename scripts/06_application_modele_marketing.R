# Application du modèle de prédiction aux données Marketing
# Chargement des packages nécessaires
packages_needed <- c("readr", "randomForest", "caret")
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

# # Chargement du modèle Random Forest
# model_rf <- readRDS(file.path(chemin_models, "random_forest_model.rds"))
#
# # Prédiction des catégories
# predictions <- predict(model_rf, donnees_marketing)
#
# # Ajout des prédictions au jeu de données
# donnees_marketing$Categorie <- predictions

donnees_marketing_prepared <- donnees_marketing # Remplacer cette ligne par les étapes de préparation réelles

# Prédiction des catégories de véhicules pour les données marketing
predictions_marketing <- predict(model_rf, donnees_marketing_prepared)

# Sauvegarde des données avec les prédictions
write.csv(data.frame(Client = donnees_marketing$Client, Categorie_Predite = predictions_marketing),
          file.path(chemin_results, "predictions_marketing.csv"), row.names = FALSE)

head(predictions_marketing)
head(predictions_marketing)

