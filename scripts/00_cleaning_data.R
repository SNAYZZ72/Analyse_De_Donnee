# Vérification et installation des packages nécessaires
packages_needed <- c("cluster", "ggplot2", "stringr")
packages_to_install <- packages_needed[!(packages_needed %in% installed.packages()[,"Package"])]
if(length(packages_to_install)) install.packages(packages_to_install)

# Activation des librairies
library(cluster)
library(ggplot2)
library(stringr)

# Chemin vers données
chemin_data <- "data/"

# Fonction de nettoyage générique
clean_data <- function(file_name) {
  data_path <- file.path(chemin_data, file_name)
  if(!file.exists(data_path)) {
    message("Le fichier ", data_path, " n'existe pas.")
    return(NULL)
  }
  data <- read.csv(data_path, header = TRUE, sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")

  # Supprimer les lignes contenant aucun caractère dans la colonne sexe
  data <- data[nchar(data$sexe) > 0, ]

  # Transformer "Homme" et "Masculin" en "H" dans la colonne sexe
  data$sexe <- gsub("Homme|Masculin", "M", data$sexe)

  # Transformer "Femme" et "Feminin" en "F" dans la colonne sexe
  data$sexe <- gsub("Femme|Feminin", "F", data$sexe)

  # Supprimer les lignes qui ne contiennent pas F ou H dans la colonne sexe
  # data <- data[data$sexe %in% c("F", "M"), ]

  # Supprimer les ligne  avec des valeurs aberrantes dans la colonne Age
  data <- data[data$age >= 18 & data$age <= 84, ]

  # Supprimer les lignes avec des valeurs aberrantes dans la colonne Taux  (544 et 74185)
  data <- data[data$taux >= 544 & data$taux <= 74185, ]

  # Supprimer les lignes avec des valeurs aberrantes dans la colonne NbEnfantsAcharge [0, 4]
  data <- data[data$nbEnfantsAcharge >= 0 & data$nbEnfantsAcharge <= 4, ]

  # Supprimer les lignes avec des valeurs ne contenant pas de caractères dans toutes la colonne 2emeVoiture
  data <- data[nchar(data$`X2emeVoiture`) > 0, ]

  # Supprimer les lignes avec des valeurs Immatriculation qui ne corresponde pas à un format valide au format « 9999 AA 99 »
  data <- data[str_detect(data$immatriculation, "^[0-9]{4} [A-Z]{2} [0-9]{2}$"), ]

  # Supprimer les lignes avec des valeurs ne contenant pas un prix 7500, 101300] dans la colonne Prix
  data <- data[data$prix >= 7500 & data$prix <= 101300, ]

  # Supprimer les lignes avec des valeurs ne contenant pas une Puissance  en chevaux Din [55, 507]
  data <- data[data$puissance >= 55 & data$puissance <= 507, ]

  # changement de <e8> par e  pour (tres longue) dans la colonne longueur
  data$longueur <- gsub("<e8>", "e", data$longueur)


  # Sauvegarde du fichier nettoyé
  cleaned_file_name <- gsub(".csv", "_clean.csv", file_name)
  write.csv(data, file.path(chemin_data, cleaned_file_name), row.names = FALSE)

  message("Nettoyage terminé pour ", file_name)
}

# Liste des fichiers à nettoyer
files_to_clean <- c("Clients_7.csv", "Clients_12.csv", "Immatriculations.csv", "Marketing.csv","Catalogue.csv")

# Application du nettoyage
lapply(files_to_clean, clean_data)

message("Nettoyage terminé pour tous les fichiers spécifiques.")

