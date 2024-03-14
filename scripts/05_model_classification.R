# Vérification et installation des packages nécessaires
packages_needed <- c("dplyr", "readr", "randomForest", "caret")
new_packages <- packages_needed[!packages_needed %in% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)

# Chargement des packages avec lapply
lapply(packages_needed, require, character.only = TRUE)

# Chemin vers les données
chemin_data <- "data/cleaned"
chemin_models <- "models"
chemin_results <- "results"
chemin_reports <- "reports"

# Fonction pour charger des données
load_data <- function(file_name) {
data_path <- file.path(chemin_data, file_name)
if(!file.exists(data_path)) {
stop("Le fichier ", data_path, " n'existe pas.")
}
data <- read_csv(data_path)
return(data)
}

# Charger les données fusionnées
donnees_fusionnees <- load_data("Donnees_Fusionnees.csv")

# Préparation des données
donnees_fusionnees$Categorie <- as.factor(donnees_fusionnees$Categorie)

# Séparation des données en ensembles d'entraînement et de test
set.seed(123) # Pour la reproductibilité
index <- createDataPartition(donnees_fusionnees$Categorie, p=0.8, list=FALSE)
trainData <- donnees_fusionnees[index, ]
testData <- donnees_fusionnees[-index, ]

# Entraînement du modèle Random Forest
model_rf <- randomForest(Categorie ~ ., data = trainData, ntree = 100)

# Prédiction sur l'ensemble de test
predictions <- predict(model_rf, testData)

# Évaluation du modèle et sauvegarde des résultats
conf_mat <- confusionMatrix(predictions, testData$Categorie)
print(conf_mat)
write.csv(conf_mat$table, file.path(chemin_results, "confusion_matrix.csv"), row.names = FALSE)

# Sauvegarde du modèle entraîné
saveRDS(model_rf, file.path(chemin_models, "random_forest_model.rds"))

# Génération d'un rapport simple (exemple)
rapport <- paste("Rapport du modèle Random Forest :\n",
"Accuracy: ", conf_mat$overall['Accuracy'], "\n",
"95% CI: ", paste(conf_mat$overall['AccuracyLower'], conf_mat$overall['AccuracyUpper'], sep = "-"), "\n",
"No. of variables tried at each split: ", model_rf$mtry, "\n")

writeLines(rapport, file.path(chemin_reports, "rapport_rf.txt"))