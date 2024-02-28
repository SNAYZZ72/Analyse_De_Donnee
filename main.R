#Chargement des bibliothèques nécessaires
library(tidyverse) # Pour la manipulation et la visualisation des données
library(readr)     # Pour lire les fichiers CSV
library(dplyr)     # Pour la manipulation des données
library(ggplot2)   # Pour la visualisation des données

# Chemin vers les scripts
chemin_scripts <- "scripts/"
chemin_data <- "data/"

# Fonction de nettoyage générique
source(paste0(chemin_scripts, "00_cleaning_data.R"))

# 1. Analyse exploratoire des données
source(paste0(chemin_scripts, "01_analyse_exploratoire.R"))

# 2. Identification des catégories de véhicules
source(paste0(chemin_scripts, "02_identification_categories.R"))

# 3. Application des catégories aux données d’immatriculations
source(paste0(chemin_scripts, "03_application_categories.R"))

# 4. Fusion des données Clients et Immatriculations
source(paste0(chemin_scripts, "04_fusion_donnees.R"))

# 5. Modélisation - Création du modèle de classification
source(paste0(chemin_scripts, "05_model_classification.R"))

# 6. Application du modèle aux données Marketing
source(paste0(chemin_scripts, "06_application_modele_marketing.R"))
