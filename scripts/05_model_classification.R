# Chargement des packages nécessaires
packages_needed <- c("dplyr", "readr", "randomForest", "caret", "e1071", "nnet", "keras", "DMwR", "ranger", "caretEnsemble", "ggplot2", "rmarkdown", "smotefamily", "DMwR2")
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

# Chargement des données fusionnées
donnees_fusionnees <- load_data("Donnees_Fusionnees.csv")
donnees_fusionnees$Categorie <- as.factor(donnees_fusionnees$Categorie)

# Séparation des données en ensembles d'entraînement et de test
set.seed(123)
index <-  caret::createDataPartition(donnees_fusionnees$Categorie, p = 0.8, list = FALSE)
trainData <- donnees_fusionnees[index,]
testData <- donnees_fusionnees[-index,]

# Liste pour stocker les résultats des modèles
model_results <- list()

# Fonction pour évaluer et comparer les modèles
evaluate_model <- function(model, testData, modelName) {
  predictions <- predict(model, testData)
  conf_mat <- confusionMatrix(predictions, testData$Categorie)
  model_results[[modelName]] <- conf_mat
  write.csv(conf_mat$table, file.path(chemin_results, paste0("confusion_matrix_", modelName, ".csv")), row.names = FALSE)


  rapport <- paste0("Rapport pour ", modelName, ":\n", "Accuracy: ", round(conf_mat$overall['Accuracy'], 4), "\n", "95% CI: ", paste0(round(conf_mat$overall['AccuracyLower'], 4), "-", round(conf_mat$overall['AccuracyUpper'], 4)), "\n", "Kappa: ", round(conf_mat$overall['Kappa'], 4), "\n", "Sensibilite (Recall): ", paste(round(conf_mat$byClass['Sensitivity'], 4), collapse = ", "), "\n", "Specificite: ", paste(round(conf_mat$byClass['Specificity'], 4), collapse = ", "), "\n", "Precision (Precision): ", paste(round(conf_mat$byClass['Pos Pred Value'], 4), collapse = ", "), "\n", "F1 Score: ", paste(round(conf_mat$byClass['F1'], 4), collapse = ", "), "\n")

  # Sauvegarde du rapport dans un fichier
  rapport_path <- file.path(chemin_reports, paste0("rapport_complet_", modelName, ".txt"))
  writeLines(rapport, rapport_path)

  return(conf_mat)
}

# Modèle Random Forest original
model_rf <- randomForest(Categorie ~  age + sexe + taux + situationFamiliale + nbEnfantsAcharge + X2eme.voiture, data = trainData, ntree = 30, importance = TRUE)
evaluate_model(model_rf, testData, "random_forest")
saveRDS(model_rf, file.path(chemin_models, "random_forest_model.rds"))

# Visualisation des ntree de random forest
plot(model_rf)

gc()

# Modèle SVM
model_svm <- e1071::svm(Categorie ~  age + sexe + taux + situationFamiliale + nbEnfantsAcharge + X2eme.voiture, data = trainData, kernel = "radial", cost = 10, gamma = 0.1)
evaluate_model(model_svm, testData, "svm")
saveRDS(model_svm, file.path(chemin_models, "svm_model.rds"))

gc()






