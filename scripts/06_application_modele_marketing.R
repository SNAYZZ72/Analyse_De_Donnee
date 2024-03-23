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

# Noms des modèles à charger et à appliquer
model_names <- c("decision_tree_model.rds", "logistic_regression_multinom_model.rds", "svm_model.rds", "random_forest_model.rds")

# Application des modèles et enregistrement des prédictions dans des fichiers séparés
for (model_name in model_names) {
  model_path <- file.path(chemin_models, model_name)
  model <- readRDS(model_path)

  # Prédiction sur les données de marketing
  predictions <- predict(model, donnees_marketing, type = "class")

  # Préparation du dataframe pour l'enregistrement
  donnees_avec_predictions <- donnees_marketing
  prediction_col_name <- gsub("(.rds)", "", model_name)
  prediction_col_name <- paste(prediction_col_name, "prediction", sep = "_")

  donnees_avec_predictions[[prediction_col_name]] <- predictions

  # Nom du fichier pour les prédictions
  file_name <- gsub(".rds", "_with_predictions.csv", model_name)
  data_path <- file.path(chemin_results, file_name)

  # Enregistrement des données avec les prédictions
  write_csv(donnees_avec_predictions[, c(names(donnees_marketing), prediction_col_name)], data_path)
}


# # application du modèle de prédiction
# # Chargement du modèle
# model_path <- file.path(chemin_models, "decision_tree_model.rds")
# model <- readRDS(model_path)
#
# # Prédiction sur les données de marketing
# predictions <- predict(model, donnees_marketing, type = "class")
#
# # Ajout des prédictions aux données
# donnees_marketing$Categorie <- predictions
#
# # Enregistrement des données avec les prédictions
# data_path <- file.path(chemin_results, "Marketing_with_predictions.csv")
# write_csv(donnees_marketing, data_path)

# afficher les 6 premières lignes
head(donnees_avec_predictions)