
# Activation des librairies
library(cluster)
library(ggplot2)
library(stringr)

# Chemin vers les données nettoyées
chemin_data <- "data/"

# Charger les données nettoyées
immatriculations <- read.csv(file.path(chemin_data, "Immatriculations_clean.csv"), header = TRUE)
catalogue <- read.csv(file.path(chemin_data, "Catalogue_clean.csv"), header = TRUE)
marketing <- read.csv(file.path(chemin_data, "Marketing_clean.csv"), header = TRUE)

# Statistiques descriptives
summary(immatriculations)
summary(catalogue)
summary(marketing)

ombre de clients")