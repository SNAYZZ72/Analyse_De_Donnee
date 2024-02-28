# Chargement des bibliothèques nécessaires
library(tidyverse)
library(readr)

# Chargement des données
donnees <- read_csv(paste0(chemin_data, "catalogue.csv"))

# Affichage des premières lignes des données
head(donnees)

# Résumé statistique des données
summary(donnees)

# Nombre de valeurs manquantes par colonne
colSums(is.na(donnees))

# Visualisation des distributions (Exemple avec ggplot2)
ggplot(donnees, aes(x = variable_a_visualiser)) +
  geom_histogram(binwidth = taille_bin, fill = "blue", color = "black") +
  theme_minimal() +
    labs(title = "Distribution de la variable", x = "Valeur", y = "Fréquence")

# Boîtes à moustaches pour détecter les valeurs aberrantes
ggplot(donnees, aes(x = factor(1), y = variable_a_examiner)) +
  geom_boxplot() +
    theme_minimal() +
    labs(title = "Boîte à moustaches de la variable", x = "", y = "Valeur")


# Nuage de points pour examiner les relations entre deux variables
ggplot(donnees, aes(x = variable_x, y = variable_y)) +
  geom_point() +
    theme_minimal() +
    labs(title = "Nuage de points", x = "Variable X", y = "Variable Y")
