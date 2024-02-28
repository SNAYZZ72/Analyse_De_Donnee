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

  # Corrections spécifiques
  corrections <- list(
    "Catalogue.csv" = list(
      longueur = c("tr<e8>s longue", "tres longue"),
      marque = c("Hyunda<ef>", "Hyundai")
    ),
    "Immatriculations.csv" = list(
      longueur = c("tr<e8>s longue", "tres longue"),
      marque = c("Hyunda<ef>", "Hyundai")
    ),
    "Marketing.csv" = list(
      situationFamiliale = c("C<e9>libataire", "Celibataire", "Seule", "Celibataire", "Marié(e)", "Marie")
    ),
    "client_7.csv" = list(
      sexe = c("Masculin|Homme", "M", "Féminin|Femme", "F", "\\?|N/D", "Indéterminé")
    ),
    "client_12.csv" = list(
      sexe = c("Masculin|Homme", "M", "Féminin|Femme", "F", "\\?|N/D", "Indéterminé")
    )
  )

  # Mise à jour des corrections pour la colonne sexe avec une expression régulière
  if (file_name %in% names(corrections)) {
    corrections_sexe <- c("Masculin|Homme" = "M", "Féminin|Femme" = "F", "\\?|N/D" = "Indéterminé")
    for (pattern in names(corrections_sexe)) {
      data$sexe <- ifelse(grepl(pattern, data$sexe), corrections_sexe[pattern], data$sexe)
    }
  }

  # Autres corrections spécifiques
  for (var in names(corrections[[file_name]])) {
    if (var != "sexe") {
      data[[var]] <- gsub(corrections[[file_name]][[var]][1], corrections[[file_name]][[var]][2], data[[var]])
    }
  }

  # Suppression des lignes avec des valeurs manquantes ou des points d'interrogation
  data <- data[complete.cases(data), ]
  data <- data[!apply(data, 1, function(row) any(row == "?")), ]
  # Suppression des lignes contenant uniquement des tirets
  data <- data[!apply(data, 1, function(row) all(row == "-")), ]
  # Suppression des duplicatas
  data <- unique(data)

  # Modification de la partie qui sauvegarde le fichier nettoyé
  cleaned_file_name <- gsub(".csv", "_cleaner_file.csv", file_name)
  write.csv(data, file.path(chemin_data, cleaned_file_name), row.names = FALSE)

  message("Nettoyage terminé pour ", file_name)
}

# Liste des fichiers à nettoyer
files_to_clean <- c("Catalogue.csv", "Immatriculations.csv", "Marketing.csv", "Clients_7.csv", "Clients_12.csv", "CO2.csv")

# Application du nettoyage
lapply(files_to_clean, clean_data)

message("Nettoyage terminé pour tous les fichiers spécifiques.")
