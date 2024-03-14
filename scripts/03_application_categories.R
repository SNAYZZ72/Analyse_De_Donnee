# Vérification et installation des packages nécessaires
packages_needed <- c("cluster", "ggplot2", "dplyr", "readr", "data.table", "scales")
new_packages <- packages_needed[!packages_needed %in% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)

# Chargement des packages avec lapply
lapply(packages_needed, require, character.only = TRUE)

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
#catalogue <- load_data("Catalogue_sans_accents_clean.csv")
immatriculations <- load_data("Immatriculations_sans_accents_clean.csv")

# Conversion de la variable "longueur" en numérique
immatriculations$longueur <- factor(immatriculations$longueur, levels = c("courte", "moyenne", "longue", "tres longue"), labels = c(1, 2, 3, 4))
immatriculations$longueur <- as.numeric(as.character(immatriculations$longueur))


immatriculations$prix <- as.numeric(as.character(immatriculations$prix))

# Valeur test
immatriculations <- rbind(immatriculations, data.table(immatriculation = "9999 AZ 99", marque = "Audi",
nom = "test", puissance = 75, longueur = 3, nbPlaces = 7, nbPortes = 7, couleur = "gris", occasion = FALSE, prix = 30000))


# Fonction pour attribuer une catégorie plus efficacement
attribuer_categorie_complexe <- function(data) {

    data <- data %>%
        mutate(
        Categorie = case_when(
            longueur < 3 & nbPortes >= 4 ~ "Petites Voitures Urbaines",
            longueur < 3 & nbPortes < 4 ~ "Citadines Economiques",
            longueur >= 3 & longueur < 4 & nbPlaces >= 6 ~ "Monospaces familiaux spacieux et abordables",
            longueur >= 3 & longueur < 4 & nbPlaces < 6 ~ "Berlines Moyennes",
            longueur >= 3 & longueur >= 4 & puissance < 289 ~ "Sportive et premium",
            longueur >= 3 & longueur >= 4 & puissance >= 289 ~ "Luxueuses",
            TRUE ~ "non classe"
        )
        )
}

# Appliquer la fonction étendue pour attribuer les catégories
# catalogue <- attribuer_categorie_complexe(catalogue)
immatriculations <- attribuer_categorie_complexe(immatriculations)

# Affichez ou enregistrez votre catalogue avec les catégories attribuées
# head(catalogue, 50)
head(immatriculations, 50)

# Fonction pour afficher la distribution des catégories
afficher_distribution_categories <- function(data, titre_graphique) {
  ggplot(data, aes(x = factor(Categorie), fill = Categorie)) +
    geom_bar() +
    geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = "black") +
    scale_fill_viridis_d() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 64, hjust = 1)) +
    labs(title = titre_graphique, x = "Categorie", y = "Nombre de Vehicules")
}

# Appliquer la fonction pour chaque dataframe
# afficher_distribution_categories(catalogue, "Distribution des Categories de Vehicules - Catalogue")
afficher_distribution_categories(immatriculations, "Distribution des Categories de Vehicules - Immatriculations")


echantillonner_et_visualiser <- function(data, titre) {
  data_sampled <- data %>%
    group_by(Categorie) %>%
    sample_n(min(5, n()), replace = TRUE) %>%
    ungroup()

  ggplot(data_sampled, aes(x = Categorie, y = reorder(nom, Categorie))) +
    geom_text(aes(label = nom), check_overlap = TRUE, hjust = 1, size = 3) +
    scale_fill_viridis_d() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = titre, x = "Categorie", y = "Nom du vehicule")
}

# Appliquer la fonction pour chaque ensemble de données
# echantillonner_et_visualiser(catalogue, "Noms des vehicules par categorie (Catalogue)")
echantillonner_et_visualiser(immatriculations, "Noms des vehicules par categorie (Immatriculations)")

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
# afficher_vehicules_par_categorie(catalogue)
afficher_vehicules_par_categorie(immatriculations)


# Fonction pour enregistrer un dataframe dans un fichier CSV
enregistrer_dataframe <- function(dataframe, file_name) {
  data_path <- file.path(chemin_data, file_name)
  write.csv(dataframe, file = data_path, row.names = FALSE)
}

# Utiliser la fonction pour enregistrer les dataframes
# enregistrer_dataframe(catalogue, "Catalogue_categorie.csv")
enregistrer_dataframe(immatriculations, "Immatriculations_categorie.csv")


