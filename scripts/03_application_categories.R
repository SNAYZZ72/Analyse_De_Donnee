# On souhaite attrbuer à chaque immatriculation une catégorie en fonction de ses caractéristiques. Pour cela, on va utiliser les cluster qu'on a déjà créé présent dans le fichier resultats_clustering.csv. On va donc charger ces données et les utiliser pour attribuer une catégorie à chaque immatriculation.

# Chargement des packages
library(cluster)
library(ggplot2)
library(dplyr)
library(tidyverse)

# Chargement des données
data_clustered <- read.csv("data/resultats_clustering.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")
data_new <- read.csv("data/cleaned/Immatriculations_sans_accents_clean.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")

# Conversion de la variable "longueur" en numérique
data_new$longueur <- factor(data_new$longueur, levels = c("courte", "moyenne", "longue", "tres longue"), labels = c(1, 2, 3, 4))
data_new$longueur <- as.numeric(as.character(data_new$longueur))

# Exclusion des variables non pertinentes
variables_a_exclure <- c("immatriculation", "marque", "nom", "couleur", "occasion")
data_new <- select(data_new, -variables_a_exclure)

# Sélection des variables restantes
selected_data <- data_new[, c("puissance", "longueur", "nbPlaces", "nbPortes", "prix")]

# Normalisation des données
normalized_data <- scale(selected_data)


# Afficher la proportion de valeurs manquantes pour chaque variable
summary(data_new)

# Identifier les lignes avec des valeurs manquantes
data_new %>%
  filter(any(is.na(data_new)))

# Supprimer les lignes avec des valeurs manquantes
data_new <- na.omit(data_new)



# Application de l'algorithme de clustering (K-means)
k <- 5
# clusters <- kmeans(normalized_data, centers=k)
#
# # Ajout des clusters à nos données
# data_new$cluster <- clusters$cluster
#
# # Enregistrement des résultats dans un fichier CSV
# write.csv(data_new, "data/resultats_clustering_immatriculations.csv", row.names = FALSE)
