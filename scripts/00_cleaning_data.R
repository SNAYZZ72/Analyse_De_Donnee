# Vérification et installation des packages nécessaires
packages_needed <- c("cluster", "ggplot2", "stringr")
packages_to_install <- packages_needed[!(packages_needed %in% installed.packages()[,"Package"])]
if(length(packages_to_install)) install.packages(packages_to_install)

# Activation des librairies
library(cluster)
library(ggplot2)
library(stringr)

# Chemin vers les scripts et données
chemin_scripts <- "scripts/"
chemin_data <- "data/"

# Fonction de nettoyage générique
clean_data <- function(file_name) {
  data_path <- file.path(chemin_data, file_name)
  if(!file.exists(data_path)) {
    message("Le fichier ", data_path, " n'existe pas.")
    return(NULL)
  }
  data <- read.csv(data_path, header = TRUE, sep = ",", stringsAsFactors = FALSE)

  # Backup du fichier original
  file.copy(data_path, gsub(".csv", "_backup.csv", data_path))

  # Normalisation des chaînes de caractères et suppression des espaces superflus
  data <- data.frame(lapply(data, function(x) if (is.character(x)) trimws(tolower(x)) else x))

  # Application des corrections spécifiques
  data <- validate_and_correct_data(data, file_name)

  # Suppression des duplicatas
  data <- data %>% distinct()

  # Sauvegarde du fichier nettoyé
  cleaned_file_name <- gsub(".csv", "_clean.csv", file_name)
  write.csv(data, file.path(chemin_data, cleaned_file_name), row.names = FALSE)

  message("Nettoyage terminé pour ", file_name)
}

# Fonction pour valider et corriger les données
validate_and_correct_data <- function(data, file_name) {
  if (file_name %in% names(specifications)) {
    spec <- specifications[[file_name]]
    for (col in names(spec)) {
      if (is.function(spec[[col]])) {
        data <- data[spec[[col]](data[[col]]), ]
      } else {
        data <- data[data[[col]] %in% spec[[col]], ]
      }
    }
  } else {
    message("Aucune spécification de nettoyage spécifique pour ", file_name)
  }
  return(data)
}

  # Spécifications pour chaque fichier
  specifications <- list(
    "Catalogue.csv" = list(
      Marque = c("Audi", "BMW", "Dacia", "Daihatsu", "Fiat", "Ford", "Honda", "Hyundaï", "Jaguar", "Kia", "Lancia", "Mercedes", "Mini", "Nissan", "Peugeot", "Renault", "Saab", "Seat", "Skoda", "Volkswagen", "Volvo"),
      Puissance = c(55, 507),
      Longueur = c("courte", "moyenne", "longue", "très longue"),
      NbPlaces = c(5, 7),
      NbPortes = c(3, 5),
      Couleur = c("blanc", "bleu", "gris", "noir", "rouge"),
      Occasion = c(TRUE, FALSE),
      Prix = c(7500, 101300)
    ),
    "Immatriculations.csv" = list(
      Immatriculation = function(x) grepl("^\\d{4} [A-Z]{2} \\d{2}$", x),
      Marque = c("Audi", "BMW", "Dacia", "Daihatsu", "Fiat", "Ford", "Honda", "Hyundaï", "Jaguar", "Kia", "Lancia", "Mercedes", "Mini", "Nissan", "Peugeot", "Renault", "Saab", "Seat", "Skoda", "Volkswagen", "Volvo"),
      Nom = "Définir selon les noms valides de modèles de véhicules",
      Puissance = c(55, 507),
      Longueur = c("courte", "moyenne", "longue", "très longue"),
      NbPlaces = c(5, 7),
      NbPortes = c(3, 5),
      Couleur = c("blanc", "bleu", "gris", "noir", "rouge"),
      Occasion = c(TRUE, FALSE),
      Prix = c(7500, 101300)
    ),
    "Marketing.csv" = list(
      Age = c(18, 84),
      Sexe = c("M", "F"),
      Taux = c(544, 74185),
      SituationFamiliale = c("Célibataire", "Divorcée", "En Couple", "Marié(e)", "Seul", "Seule"),
      NbEnfantsAcharge = c(0, 4),
      "2eme voiture" = c(TRUE, FALSE)
    ),
    "Clients_7.csv" = list(
      Age = c(18, 84),
      Sexe = c("M", "F"),
      Taux = c(544, 74185),
      SituationFamiliale = c("Célibataire", "Divorcée", "En Couple", "Marié(e)", "Seul", "Seule"),
      NbEnfantsAcharge = c(0, 4),
      "2eme voiture" = c(TRUE, FALSE)
    ),
    "Clients_12.csv" = list(
      Age = c(18, 84),
      Sexe = c("M", "F"),
      Taux = c(544, 74185),
      SituationFamiliale = c("Célibataire", "Divorcée", "En Couple", "Marié(e)", "Seul", "Seule"),
      NbEnfantsAcharge = c(0, 4),
      "2eme voiture" = c(TRUE, FALSE)
    ),
    )


# Liste des fichiers à nettoyer
files_to_clean <- c("Catalogue.csv", "Immatriculations.csv", "Marketing.csv", "Clients_7.csv", "Clients_12.csv", "CO2.csv")

# Application du nettoyage
lapply(files_to_clean, clean_data)

message("Nettoyage terminé pour tous les fichiers spécifiques.")

