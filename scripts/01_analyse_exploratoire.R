# Vérification et installation des packages nécessaires
packages_needed <- c("cluster", "ggplot2", "stringr", "dplyr", "readr")
packages_to_install <- packages_needed[!(packages_needed %in% installed.packages()[,"Package"])]
if(length(packages_to_install)) install.packages(packages_to_install)


# Activation des librairies
library(cluster)
library(ggplot2)
library(stringr)
library(dplyr)
library(readr)
library(rlang)


# Chemin vers les données nettoyées
chemin_data <- "data/sans_accents/"

# Fonction pour charger et prévisualiser les données (glimpse)
load_data <- function(file_name) {
  data_path <- file.path(chemin_data, file_name)
  if(!file.exists(data_path)) {
    message("Le fichier ", data_path, " n'existe pas.")
    return(NULL)
  }
  data <- read.csv(data_path, header = TRUE, sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")

  glimpse(data)
  return(data)
}

# Charger les données
clients_7 <- load_data("Clients_7_sans_accents.csv")
clients_12 <- load_data("Clients_12_sans_accents.csv")
catalogue <- load_data("Catalogue_sans_accents.csv")
marketing <- load_data("Marketing_sans_accents.csv")



# Fonction pour afficher des histogrammes
afficher_histogrammes <- function(donnees, variables) {
  for (variable in variables) {
    if (variable %in% names(donnees)) {
      p <- ggplot(donnees, aes_string(x = variable)) + geom_bar() + theme_minimal() + ggtitle(paste("Distribution de", variable))
      print(p)
    } else {
      message("La variable '", variable, "' n'existe pas dans les données fournies.")
    }
  }
}


# Afficher des histogrammes pour chaque fichier de données
afficher_histogrammes(clients_7, c("age", "sexe", "taux", "situationFamiliale", "nbEnfantsAcharge", "X2eme.voiture"))
afficher_histogrammes(clients_12, c("age", "sexe", "taux", "situationFamiliale", "nbEnfantsAcharge", "X2eme.voiture"))
afficher_histogrammes(catalogue, c("marque", "nom", "puissance", "longueur", "nbPlaces", "nbPortes", "couleur", "occasion", "prix"))
afficher_histogrammes(marketing, c("age", "sexe", "taux", "situationFamiliale", "nbEnfantsAcharge", "X2eme.voiture"))

# Afficher un résumé des données
summary(clients_7)
summary(clients_12)
summary(catalogue)
summary(marketing)

# Fonction pour des boites à moustaches
afficher_boxplot <- function(donnees, variables) {
  for (variable in variables) {
    p <- ggplot(donnees, aes(x = factor(1), y = !!sym(variable))) +
      geom_boxplot() +
      theme_minimal() +
      ggtitle(paste("Distribution de", variable))
    print(p)
  }
}

# Afficher des boites à moustaches pour chaque fichier de données
afficher_boxplot(clients_7, c("age", "taux", "nbEnfantsAcharge"))
afficher_boxplot(clients_12, c("age", "taux", "nbEnfantsAcharge"))
afficher_boxplot(catalogue, c("puissance", "nbPlaces", "nbPortes", "prix"))
afficher_boxplot(marketing, c("age", "taux", "nbEnfantsAcharge"))

# Fonction pour afficher des nuages de points
afficher_nuage_points <- function(data, variable_y) {
  for (variable in variable_y) {
    p <- ggplot(data, aes(x = age, y = !!sym(variable))) +
      geom_point() +
      theme_minimal() +
      ggtitle(paste("Nuage de points entre age et", variable))
    print(p)
  }
}


# Afficher des nuages de points pour chaque fichier de données
afficher_nuage_points(clients_7, c("taux", "nbEnfantsAcharge"))
afficher_nuage_points(clients_12, c("taux", "nbEnfantsAcharge"))
afficher_nuage_points(catalogue, c("puissance", "nbPlaces", "nbPortes", "prix"))
afficher_nuage_points(marketing, c("taux", "nbEnfantsAcharge"))


# Fonction pour afficher des diagrammes circulaires
afficher_piechart <- function(data, variables) {
  for (variable in variables) {
    # Vérifier si la variable existe dans le dataframe
    if (variable %in% names(data)) {
      # Créer le diagramme circulaire
      p <- ggplot(data, aes(x = "", fill = !!sym(variable))) +
        geom_bar(width = 1, stat = "count") +
        coord_polar("y", start = 0) +
        theme_void() +
        labs(title = paste("Diagramme circulaire de", variable), fill = variable)

      print(p)
    } else {
      message("La variable '", variable, "' n'existe pas dans les données fournies.")
    }
  }
}

# Afficher des diagrammes circulaires pour chaque fichier de données
afficher_piechart(clients_7, c("sexe", "situationFamiliale", "X2eme.voiture"))
afficher_piechart(clients_12, c("sexe", "situationFamiliale", "X2eme.voiture"))
afficher_piechart(catalogue, c("marque", "nbPlaces", "nbPortes", "occasion"))
afficher_piechart(marketing, c("sexe", "situationFamiliale", "X2eme.voiture"))

# Fonction pour afficher des densités
afficher_densite <- function(donnees, variables) {
  for (variable in variables) {
    p <- ggplot(donnees, aes_string(x = variable)) + geom_density() + theme_minimal() + ggtitle(paste("Densité de", variable))
    print(p)
  }
}

# Afficher des densités pour chaque fichier de données
afficher_densite(clients_7, c("age", "taux", "nbEnfantsAcharge"))
afficher_densite(clients_12, c("age", "taux", "nbEnfantsAcharge"))
afficher_densite(catalogue, c("puissance", "nbPlaces", "nbPortes", "prix"))
afficher_densite(marketing, c("age", "taux", "nbEnfantsAcharge"))

# Fonction pour afficher des statistiques descriptives
affichier_statistiques_descriptives <- function(donnees) {
  p <- skim(donnees)
  print(p)
}

# Afficher des statistiques descriptives pour chaque fichier de données
affichier_statistiques_descriptives(clients_7)
affichier_statistiques_descriptives(clients_12)
affichier_statistiques_descriptives(catalogue)
affichier_statistiques_descriptives(marketing)

