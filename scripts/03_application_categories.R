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

# Convertir la variable "longueur" en numérique
# catalogue$longueur <- factor(data$longueur, levels = c("courte", "moyenne", "longue", "tres longue"), labels = c(1, 2, 3, 4))
# catalogue$longueur <- as.numeric(as.character(data$longueur))

# Fonction pour attribuer une catégorie
attribuer_categorie_complexe <- function(catalogue) {
  # Conversion des variables au format correct (si nécessaire)
  catalogue$puissance <- as.numeric(as.character(catalogue$puissance))
  catalogue$nbPlaces <- as.numeric(as.character(catalogue$nbPlaces))
  catalogue$nbPortes <- as.numeric(as.character(catalogue$nbPortes))

  # Application des catégories en fonction des règles de l'arbre de décision
  catalogue <- catalogue %>%
    mutate(Categorie = case_when(
      puissance >= 223 ~ "Voitures de sport puissantes et luxueuses",  # Catégorie
      puissance < 223 & nbPortes >= 4 ~ "Monospaces familiaux spacieux et abordables",  # Catégorie 2
      puissance < 223 & nbPortes < 4 & nbPlaces < 6 ~ "Voitures compactes sportives et economiques",  # Catégorie 5
      puissance < 223 & nbPortes < 4 & nbPlaces >= 6 & puissance >= 138 ~ "Berlines familiales puissantes et confortables",  # Catégorie 4
      puissance < 223 & nbPortes < 4 & nbPlaces >= 6 & puissance < 138 ~ "Citadines compactes et abordables",  # Catégorie 3
      TRUE ~ "Autres"  # Pour tout ce qui ne correspond pas aux règles ci-dessus
    ))
  return(catalogue)
}

# Appliquer la fonction étendue pour attribuer les catégories
catalogue <- attribuer_categorie_complexe(catalogue)

# Affichez ou enregistrez votre catalogue avec les catégories attribuées
head(catalogue, 50)

# Affichez la distribution des catégories
ggplot(catalogue, aes(x = factor(Categorie), fill = Categorie)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = "black") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 64, hjust = 1)) +
  labs(title = "Distribution des Categories de Vehicules", x = "Categorie", y = "Nombre de Vehicules")

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

catalogue_sampled <- catalogue %>%
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

# ggplot(catalogue, aes(x = Categorie, y = reorder(nom, Categorie))) +
#   geom_point(stat = "identity", aes(color = Categorie)) +
#   scale_fill_viridis_d() +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   labs(title = "Distribution des Categories de Vehicules", x = "Categorie", y = "Nom du Vehicule")


# Enregistrer le catalogue mis à jour dans un nouveau fichier CSV
write.csv(catalogue, "data/cleaned/Catalogue_avec_categories.csv", row.names = FALSE)

