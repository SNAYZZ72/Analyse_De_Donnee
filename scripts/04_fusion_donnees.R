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
clients7 <- load_data("Clients_7_sans_accents_clean.csv")
clients12 <- load_data("Clients_12_sans_accents_clean.csv")

# Fusionner les données clients
clients <- rbind(clients7, clients12)

# Fusionner les données mais on garde que les immatriculations qui ont des clients
merged_data <- merge(clients, immatriculations, by = "immatriculation")

# on enregistre les données
write.csv(merged_data, file = "data/cleaned/Donnees_Fusionnees.csv", row.names = FALSE)