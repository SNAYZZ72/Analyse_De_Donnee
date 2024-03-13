# Application des catégorie identifier dans le script 02_identification_categories.R
# Vérification et installation des packages nécessaires
packages_needed <- c("cluster", "ggplot2", "dplyr", "readr", "data.table", "scales")
packages_to_install <- packages_needed[!(packages_needed %in% installed.packages()[,"Package"])]
if(length(packages_to_install)) install.packages(packages_to_install)

# Chargement des packages
# library(cluster)
# library(ggplot2)
# library(dplyr)
# library(readr)
# library(data.table)
# library(scales)
lapply(packages_needed, library, character.only = TRUE)

# Chemin vers les données nettoyées
chemin_data <- "data/cleaned"

# Fonction pour charger
load_data <- function(file_name) {
  data_path <- file.path(chemin_data, file_name)
  if(!file.exists(data_path)) {
    message("Le fichier ", data_path, " n'existe pas.")
    return(NULL)
  }
  data <- fread(data_path, encoding = "UTF-8")
  return(data)
}

# Charger les données nettoyées
catalogue <- load_data("Catalogue_sans_accents_clean.csv")
immatriculations <- load_data("Immatriculations_sans_accents_clean.csv")

# Conversion de la variable "longueur" en numérique
data$longueur <- factor(data$longueur, levels = c("courte", "moyenne", "longue", "tres longue"), labels = c(1, 2, 3, 4))
data$longueur <- as.numeric(as.character(data$longueur))

# Conversion des variables au format correct (si nécessaire)
data$puissance <- as.numeric(as.character(data$puissance))
data$nbPlaces <- as.numeric(as.character(data$nbPlaces))
data$nbPortes <- as.numeric(as.character(data$nbPortes))
data$prix <- as.numeric(as.character(data$prix))

# Fonction pour attribuer une catégorie
attribuer_categorie_complexe <- function(data) {
  # Conversion des variables au format correct (si nécessaire)
  data$puissance <- as.numeric(as.character(data$puissance))
  data$nbPlaces <- as.numeric(as.character(data$nbPlaces))
  data$nbPortes <- as.numeric(as.character(data$nbPortes))

  # Application des catégories en fonction des règles de l'arbre de décision
  data <- data %>%
    mutate(Categorie = case_when(
      puissance < 100 & prix <= 15000 ~ "Citadines compactes et abordables",
      puissance >= 100 & puissance < 200 & prix > 15000 & prix <= 30000 ~ "Berlines familiales puissantes et confortables",
      puissance >= 100 & puissance < 200 & prix <= 20000 ~ "Voitures compactes sportives et economiques",
      puissance >= 200 & prix > 30000 ~ "Voitures de sport puissantes et luxueuses",
      nbPlaces >= 5 & prix <= 25000 ~ "Monospaces familiaux spacieux et abordables",
      TRUE ~ "Autre"
    ))
  return(data)
}


# Appliquer la fonction étendue pour attribuer les catégories
catalogue <- attribuer_categorie_complexe(catalogue)
immatriculations <- attribuer_categorie_complexe(immatriculations)

# Affichez ou enregistrez votre catalogue avec les catégories attribuées
head(catalogue, 50)
head(immatriculations, 50)

# Affichez la distribution des catégories
ggplot(catalogue, aes(x = factor(Categorie), fill = Categorie)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = "black") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 64, hjust = 1)) +
  labs(title = "Distribution des Categories de Vehicules catalogue", x = "Categorie", y = "Nombre de Vehicules")

# Affichez la distribution des catégories
ggplot(immatriculations, aes(x = factor(Categorie), fill = Categorie)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = "black") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 64, hjust = 1)) +
  labs(title = "Distribution des Categories de Vehicules Immatriculations", x = "Categorie", y = "Nombre de Vehicules")

# Fonction pour afficher les noms de véhicules par catégorie
afficher_vehicules_par_categorie <- function(catalogue) {
  vehicules_par_categorie <- catalogue %>%
    group_by(Categorie) %>%
    summarise(NomsVehicules = paste(unique(nom), collapse = ", ")) %>%
    arrange(Categorie)

  writeLines(vehicules_par_categorie$NomsVehicules)

  print(vehicules_par_categorie)
}

# Appliquer la fonction pour afficher les noms de véhicules par catégorie
afficher_vehicules_par_categorie(catalogue)
afficher_vehicules_par_categorie(immatriculations)

catalogue_sampled <- catalogue %>%
  group_by(Categorie) %>%
  sample_n(min(5, n()), replace = TRUE) %>%
  ungroup()

immatriculations_sampled <- immatriculations %>%
  group_by(Categorie) %>%
  sample_n(min(5, n()), replace = TRUE) %>%
  ungroup()


# Créer le graphique
ggplot(catalogue_sampled, aes(x = Categorie, y = reorder(nom, Categorie))) +
  geom_text(aes(label = nom), check_overlap = TRUE, hjust = 1, size = 3) +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Noms des vehicules par categorie", x = "Categorie", y = "Nom du vehicule")

ggplot(immatriculations, aes(x = Categorie, y = reorder(nom, Categorie))) +
  geom_text(aes(label = nom), check_overlap = TRUE, hjust = 1, size = 3) +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution des Categories de Vehicules immatriculations", x = "Categorie", y = "Nom du Vehicule")


# Enregistrer fichier mis à jour dans un nouveau fichier CSV
write.csv(catalogue, file = "data/cleaned/Catalogue_sans_accents_clean.csv", row.names = FALSE)
write.csv(immatriculations, file = "data/cleaned/Immatriculations_sans_accents_clean.csv", row.names = FALSE)


