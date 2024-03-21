# Vérification et installation des packages nécessaires
packages_needed <- c("rpart", "rpart.plot", "caret", "dplyr")
packages_to_install <- packages_needed[!(packages_needed %in% installed.packages()[,"Package"])]
if(length(packages_to_install)) install.packages(packages_to_install)

# Chargement des packages
library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)

# Charger les données des clients et des voitures
data <- read.csv("data/cleaned/Donnees_Fusionnees.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Nettoyage des données
# Par exemple, conversion de certaines variables en facteurs si nécessaire
data$sexe <- factor(data$sexe)
data$situationFamiliale <- factor(data$situationFamiliale)

# Diviser les données en ensemble d'entraînement et ensemble de test
set.seed(123)
train_indices <- createDataPartition(data$Categorie, p = 0.7, list = FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Construction de l'arbre de décision avec des paramètres ajustés
fit <- rpart(Categorie ~ age + sexe + taux + situationFamiliale + nbEnfantsAcharge + X2eme.voiture,
             data = train_data,
             method = "class",
             cp = 0.001)  # Nombre maximum de variables à considérer pour une division

# Visualisation de l'arbre de décision
rpart.plot(fit, extra = 102, under = TRUE, cex = 0.8, tweak = 1.5,
           fallen.leaves = TRUE, type = 3, shadow.col = "gray", nn = TRUE,
           yesno = 2, box.palette = "RdYlGn")




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

# Création du graphique d'importance des variables avec ggplot2
library(ggplot2)
ggplot(var_importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + # Pour une meilleure lisibilité
  labs(title = "Importance des Variables dans l'Arbre de Decision",
       x = "Variables",
       y = "Importance") +
  theme_minimal()