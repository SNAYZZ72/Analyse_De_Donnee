# Vérification et installation des packages nécessaires
packages_needed <- c("rpart", "rpart.plot", "caret", "dplyr")
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
print("Matrice de Confusion:")
print(confusion_matrix)
# save confusion matrix
write.csv(confusion_matrix, "results/confusion_matrix.csv")


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