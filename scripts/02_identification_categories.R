# Chargement des packages
library(cluster)
library(ggplot2)
library(dplyr)

# Chargement des données
data <- read.csv("data/cleaned/Catalogue_sans_accents_clean.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")

# Conversion de la variable "longueur" en numérique
data$longueur <- factor(data$longueur, levels = c("courte", "moyenne", "longue", "tres longue"), labels = c(1, 2, 3, 4))
data$longueur <- as.numeric(as.character(data$longueur))

# Exclusion des variables non pertinentes
variables_a_exclure <- c("marque", "nom", "couleur", "occasion")
data <- select(data, -variables_a_exclure)

# Sélection des variables restantes
selected_data <- data[, c("puissance", "longueur", "nbPlaces", "nbPortes", "prix")]

# Normalisation des données
normalized_data <- scale(selected_data)

# Application de l'algorithme de clustering (K-means)
k <- 5
clusters <- kmeans(normalized_data, centers=k)

# Ajout des clusters à nos données
data$cluster <- clusters$cluster

# Analyse des clusters
cluster_summary <- data %>%
  group_by(cluster) %>%
  summarise_all(mean)

print(cluster_summary)

# Enregistrement des résultats dans un fichier CSV
write.csv(data_clustered, "data/resultats_clustering.csv", row.names = FALSE)

# Visualisation des clusters
ggplot(data, aes(x = puissance, y = prix, color = factor(cluster))) + geom_point() + labs(title = "Clustering des voitures", x = "Puissance", y = "Prix") + theme_minimal()

ggplot(data, aes(x = longueur, y = prix, color = factor(cluster))) + geom_point() + labs(title = "Clustering des voitures", x = "Longueur", y = "Prix") + theme_minimal()

ggplot(data, aes(x = nbPlaces, y = prix, color = factor(cluster))) + geom_point() + labs(title = "Clustering des voitures", x = "Nombre de places", y = "Prix") + theme_minimal()

ggplot(data, aes(x = nbPortes, y = prix, color = factor(cluster))) + geom_point() + labs(title = "Clustering des voitures", x = "Nombre de portes", y = "Prix") + theme_minimal()
