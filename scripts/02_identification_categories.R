# Vérification et installation des packages nécessaires
packages_needed <- c("cluster", "ggplot2", "dplyr", "rpart", "rpart.plot", "caret")
packages_to_install <- packages_needed[!(packages_needed %in% installed.packages()[,"Package"])]
if(length(packages_to_install)) install.packages(packages_to_install)


# Chargement des packages avec lapply
lapply(packages_needed, require, character.only = TRUE)

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

# Chargement des données
data <- load_data("Catalogue_sans_accents_clean.csv")
# data <- load_data("Immatriculations_sans_accents_clean.csv")

# Conversion de la variable "longueur" en numérique
data$longueur <- factor(data$longueur, levels = c("courte", "moyenne", "longue", "tres longue"), labels = c(1, 2, 3, 4))
data$longueur <- as.numeric(as.character(data$longueur))

# Conversion des variables au format correct (si nécessaire)
data$puissance <- as.numeric(as.character(data$puissance))
data$nbPlaces <- as.numeric(as.character(data$nbPlaces))
data$nbPortes <- as.numeric(as.character(data$nbPortes))
data$prix <- as.numeric(as.character(data$prix))

# Exclusion des variables non pertinentes
variables_a_exclure <- c("marque", "nom", "couleur", "occasion")
# variables_a_exclure <- c("immatriculation","marque", "nom", "couleur", "occasion")
data <- select(data, -variables_a_exclure)

# Sélection des variables restantes
selected_data <- data[, c("puissance", "longueur", "nbPlaces", "nbPortes", "prix")]

# Normalisation des données
normalized_data <- scale(selected_data)

# Fixer le seed pour la reproductibilité
set.seed(112)

# Application de l'algorithme de clustering (K-means)
k <- 6
clusters <- kmeans(normalized_data, centers=k)

# Ajout des clusters à nos données
data$cluster <- clusters$cluster

# Analyse des clusters
cluster_summary <- data %>%
  group_by(cluster) %>%
  summarise_all(mean)

print(cluster_summary)

# Visualisation des clusters
visualiser_clusters <- function(data, aes_x, aes_y, titre) {
  ggplot(data, aes_string(x = aes_x, y = aes_y, color = "factor(cluster)")) +
    geom_point() +
    labs(title = titre, x = aes_x, y = aes_y) +
    theme_minimal()
}

visualiser_clusters(catalogue_pretraite$data, "puissance", "prix", "Clustering des voitures")

# visualiser_clusters(catalogue_pretraite$data, "longueur", "prix", "Clustering des voitures")
#
# visualiser_clusters(catalogue_pretraite$data, "nbPlaces", "prix", "Clustering des voitures")
#
# visualiser_clusters(catalogue_pretraite$data, "nbPortes", "prix", "Clustering des voitures")

# Construction de l'arbre de décision
data$cluster <- as.factor(data$cluster)
set.seed(123) # Pour reproductibilité
fitControl <- trainControl(method = "cv", number = 10)
arbre_decision_cv <- train(cluster ~ puissance + longueur + nbPlaces + nbPortes + prix, data = data, method = "rpart", trControl = fitControl, tuneLength = 10)

# Meilleur modèle
bestModel <- arbre_decision_cv$finalModel

# Visualiser l'arbre élagué
rpart.plot(bestModel, extra = 102, under = TRUE, cex = 0.8, tweak = 1.5,
           fallen.leaves = TRUE, type = 3, shadow.col = "gray", nn = TRUE,
           yesno = 2, box.palette = "RdYlGn")

predictions <- predict(bestModel, data, type = "class")
data$predictedCluster <- predictions

# Matrice de confusion
confusionMatrix(data$cluster, data$predictedCluster)

# Extraction de l'importance des variables
var_importance <- bestModel$variable.importance

# Transformation en data frame pour ggplot2
var_importance_df <- as.data.frame(var_importance)
var_importance_df$Variable <- rownames(var_importance_df)
var_importance_df$Importance <- var_importance_df$var_importance
var_importance_df <- var_importance_df[ , -1] # Enlever la colonne originale

# Création du graphique d'importance des variables avec ggplot2
library(ggplot2)
ggplot(var_importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + # Pour une meilleure lisibilité
  labs(title = "Importance des Variables dans l'Arbre de Decision",
       x = "Variables",
       y = "Importance") +
  theme_minimal()