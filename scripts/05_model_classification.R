# Vérification et installation des packages nécessaires
packages_needed <- c("rpart", "rpart.plot", "caret", "dplyr", "readr", "data.table", "ggplot2", "randomForest", "nnet", "e1071", "MASS", "nnet", "rmarkdown", "keras", "tensorflow", "kerasformula", "kerasR")
packages_to_install <- packages_needed[!(packages_needed %in% installed.packages()[,"Package"])]
if(length(packages_to_install)) install.packages(packages_to_install)

# Chargement des packages
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

# Charger les données des clients et des voitures
data <- load_data("Donnees_Fusionnees.csv")

# Nettoyage des données
# Par exemple, conversion de certaines variables en facteurs si nécessaire
data$sexe <- factor(data$sexe)
data$situationFamiliale <- factor(data$situationFamiliale)

# Diviser les données en ensemble d'entraînement et ensemble de test
set.seed(12345)
train_indices <- createDataPartition(data$Categorie, p = 0.8, list = FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Construction de l'arbre de décision avec des paramètres ajustés
fit <- rpart(Categorie ~ age + sexe + taux + situationFamiliale + nbEnfantsAcharge + X2eme.voiture,
             data = train_data,
             method = "class",
             cp = 0.0001)



# Visualisation de l'arbre de décision
rpart.plot(fit, extra = 102, under = TRUE, cex = 0.8, tweak = 1.2,
           fallen.leaves = TRUE, type = 3, shadow.col = "gray", nn = TRUE,
           yesno = 2, box.palette = "RdYlGn")

# # Visualisation de l'arbre de décision avec une police d'écriture moyenne
# rpart.plot(fit, extra = 101, fallen.leaves = TRUE, type = 0, under = TRUE, faclen = 0, cex = 0.8)
#
# # Visualisation de l'arbre de décision avec une police d'écriture plus grande
# rpart.plot(fit, extra = 101, fallen.leaves = TRUE, type = 0, under = TRUE, faclen = 0, cex = 1.2)




# Extraction de l'importance des variables
var_importance <- fit$variable.importance

# Transformation en data frame pour ggplot2
var_importance_df <- as.data.frame(var_importance)
var_importance_df$Variable <- rownames(var_importance_df)
var_importance_df$Importance <- var_importance_df$var_importance
var_importance_df <- var_importance_df[ , -1] # Enlever la colonne originale


# Prédiction sur l'ensemble de test
predictions <- predict(fit, test_data, type = "class")

# Évaluation de la précision
accuracy <- sum(predictions == test_data$Categorie) / length(predictions)
print(paste("Accuracy:", accuracy))


# Matrice de confusion
confusion_matrix <- table(predictions, test_data$Categorie)
print("Matrice de Confusion tree model:")
print(confusion_matrix)
# save confusion matrix
write.csv(confusion_matrix, "results/confusion_matrix_tree_model.csv")


# Création du graphique d'importance des variables avec ggplot2
library(ggplot2)
ggplot(var_importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Importance des Variables dans l'Arbre de Decision",
       x = "Variables",
       y = "Importance") +
  theme_minimal()


# Sauvegarde du modèle
saveRDS(fit, "models/decision_tree_model.rds")

train_data$Categorie <- as.factor(train_data$Categorie)
test_data$Categorie <- as.factor(test_data$Categorie)

# Regression Logistique
fit_glm_multinom <- multinom(Categorie ~ age + sexe + taux + situationFamiliale + nbEnfantsAcharge + X2eme.voiture,
                             data = train_data,
                             maxit = 200) # Augmenter le nombre maximum d'itérations si nécessaire

# Prédiction sur l'ensemble de test
predictions_glm_multinom <- predict(fit_glm_multinom, newdata = test_data)


# Évaluation de la précision de la Régression Logistique Multinomiale
accuracy_glm_multinom <- sum(predictions_glm_multinom == test_data$Categorie) / nrow(test_data)
print(paste("Accuracy for Multinomial Logistic Regression:", accuracy_glm_multinom))

# Matrice de confusion pour la Régression Logistique Multinomiale
confusion_matrix_glm_multinom <- table(predictions_glm_multinom, test_data$Categorie)
print("Matrice de Confusion pour Multinomial Logistic Regression:")
print(confusion_matrix_glm_multinom)
# Sauvegarde de la matrice de confusion
write.csv(confusion_matrix_glm_multinom, file.path(chemin_results, "confusion_matrix_logistic_regression_multinom.csv"))

# Sauvegarde du modèle de Régression Logistique Multinomiale
saveRDS(fit_glm_multinom, file.path(chemin_models, "logistic_regression_multinom_model.rds"))

gc()

# SVM
fit_svm <- svm(Categorie ~ age + sexe + taux + situationFamiliale + nbEnfantsAcharge + X2eme.voiture,
                data = train_data,
                kernel = "linear",
                cost = 10)

# Prédiction sur l'ensemble de test
predictions_svm <- predict(fit_svm, newdata = test_data)

# Évaluation de la précision de la SVM
accuracy_svm <- sum(predictions_svm == test_data$Categorie) / nrow(test_data)
print(paste("Accuracy for Support Vector Machine:", accuracy_svm))

# Matrice de confusion pour la SVM
confusion_matrix_svm <- table(predictions_svm, test_data$Categorie)
print("Matrice de Confusion pour Support Vector Machine:")
print(confusion_matrix_svm)
# Sauvegarde de la matrice de confusion
write.csv(confusion_matrix_svm, file.path(chemin_results, "confusion_matrix_svm.csv"))

# Sauvegarde du modèle de SVM
saveRDS(fit_svm, file.path(chemin_models, "svm_model.rds"))

gc()

# random forest
fit_rf <- randomForest(Categorie ~ age + sexe + taux + situationFamiliale + nbEnfantsAcharge + X2eme.voiture,
                       data = train_data,
                       ntree = 500, # Nombre d'arbres
                       importance = TRUE, # Calcul de l'importance des variables
                       method = "class")

# Évaluation de la précision des Forêts Aléatoires
predictions_rf <- predict(fit_rf, test_data)
accuracy_rf <- sum(predictions_rf == test_data$Categorie) / length(predictions_rf)
print(paste("Accuracy for Random Forest:", accuracy_rf))

# Matrice de confusion pour les Forêts Aléatoires
confusion_matrix_rf <- table(predictions_rf, test_data$Categorie)
print("Matrice de Confusion pour Random Forest:")
print(confusion_matrix_rf)
# Sauvegarde de la matrice de confusion
write.csv(confusion_matrix_rf, file.path(chemin_results, "confusion_matrix_random_forest.csv"))

# Fin du script