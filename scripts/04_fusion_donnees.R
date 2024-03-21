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
immatriculations <- load_data("Immatriculations_categorie.csv") %>%
  select(immatriculation, Categorie)

# Charger les données clients
clients7 <- load_data("Clients_7_sans_accents_clean.csv")
clients12 <- load_data("Clients_12_sans_accents_clean.csv")

# Vérification rapide pour s'assurer de la compatibilité des colonnes
if (!identical(names(clients_7), names(clients_12))) {
  stop("Les colonnes des deux ensembles de données clients ne correspondent pas.")
}

# Fusionner les données clients
clients <- rbind(clients7, clients12)

# Vérification des noms de colonnes pour trouver une clé de fusion commune
print(names(immatriculations))
print(names(clients))

# Fusionner les données mais on garde que les immatriculations qui ont des clients
merged_data <- merge(clients, immatriculations, by = "immatriculation")

# Afficher les 6 premières lignes
head(merged_data)

# Enregistrer les données fusionnées
enregistrer_dataframe <- function(dataframe, file_name) {
  data_path <- file.path(chemin_data, file_name)
  write.csv(dataframe, file = data_path, row.names = FALSE, quote = FALSE)
}

# on enregistre les données
enregistrer_dataframe(merged_data, "Donnees_Fusionnees.csv")