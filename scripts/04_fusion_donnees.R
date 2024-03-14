# Vérification et installation des packages nécessaires
packages_needed <- c("dplyr", "readr")
new_packages <- packages_needed[!packages_needed %in% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)

# Chargement des packages avec lapply
lapply(packages_needed, require, character.only = TRUE)

# Chemin vers les données
chemin_data <- "data/cleaned"

# Fonction pour charger des données
load_data <- function(file_name) {
  data_path <- file.path(chemin_data, file_name)
  if(!file.exists(data_path)) {
    message("Le fichier ", data_path, " n'existe pas.")
    return(NULL)
  }
  data <- fread(data_path, encoding = "UTF-8")
  return(data)
}

# Charger les données d'immatriculations
immatriculations <- load_data("Immatriculations_categorie.csv")

# Charger les données clients
clients <- load_data("Clients_7_sans_accents_clean.csv")

doublons <- which(duplicated(immatriculations$immatriculation))
immatri_doublons <- immatriculations[doublons,]

# Assurez-vous que la colonne immatriculation est une chaîne de caractères dans les deux data frames
immatriculations$immatriculation <- as.character(immatriculations$immatriculation)
clients$immatriculation <- as.character(clients$immatriculation)

# Supprimer les espaces potentiels avant ou après les immatriculations
immatriculations$immatriculation <- trimws(immatriculations$immatriculation)
clients$immatriculation <- trimws(clients$immatriculation)

# Vérifier s'il y a des immatriculations uniques dans chaque jeu de données
unique_immatriculations <- unique(immatriculations$immatriculation)
unique_clients <- unique(clients$immatriculation)

# Vérifier les immatriculations qui sont dans un ensemble mais pas dans l'autre
missing_in_clients <- setdiff(unique_immatriculations, unique_clients)
missing_in_immatriculations <- setdiff(unique_clients, unique_immatriculations)

# Afficher les immatriculations manquantes
print(missing_in_clients)
print(missing_in_immatriculations)


# Vérification des noms de colonnes pour trouver une clé de fusion commune
# Remplacez 'immatriculation' par le nom de colonne réel qui représente la clé commune
print(names(immatriculations))
print(names(clients))

# Fusion des données
# 'immatriculation' est le nom de colonne commun utilisé pour la fusion
donnees_fusionnees <- merge(immatriculations, clients, by = "immatriculation", all.x = TRUE)

# Exploration des données fusionnées
head(donnees_fusionnees)

# Enregistrement des données fusionnées
enregistrer_dataframe <- function(dataframe, file_name) {
  data_path <- file.path(chemin_data, file_name)
  write.csv(dataframe, file = data_path, row.names = FALSE, quote = FALSE)
}

# Enregistrer les données fusionnées
enregistrer_dataframe(donnees_fusionnees, "Donnees_Fusionnees.csv")
