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

  # remplacer tr<e8>s longue par tres longue dans la colonne longueur
  if("longueur" %in% names(data)) {
    data <- data %>% mutate(
      longueur = str_replace(longueur, "très longue", "tres longue")
    )
  }

  #remplacer Hyunda<ef> par Hyundai dans la colonne marque
  if("marque" %in% names(data)) {
    data <- data %>% mutate(
      marque = str_replace(marque, "Hyundaï", "Hyundai")
    )
  }

  # remplacer célibatire par celibatire dans la colonne situationFamiliale
  # remplacer Marié(e) par Marie(e) dans la colonne situationFamiliale
  # remplacer Divorcée par Divorcee dans la colonne situationFamiliale
    if("situationFamiliale" %in% names(data)) {
        data <- data %>% mutate(
            situationFamiliale = str_replace(situationFamiliale, "Célibataire", "celibatire"),
            situationFamiliale = str_replace(situationFamiliale, "Marié(e)", "Marie(e)"),
            situationFamiliale = str_replace(situationFamiliale, "Divorcée", "Divorcee")
        )
    }


    if("age" %in% names(data)) {
      data <- data %>% filter(age >= 18 & age <= 84)
    }

    if("taux" %in% names(data)) {
        data <- data %>% filter(taux >= 544 & taux <= 74185)
    }

    if("nbEnfantsAcharge" %in% names(data)) {
        data <- data %>% filter(nbEnfantsAcharge >= 0 & nbEnfantsAcharge <= 4)
    }


    if("2eme voiture" %in% names(data)) {
        data <- data %>% filter(nchar("X2eme.voiture") > 0)
    }

    if("immatriculation" %in% names(data)) {
        data <- data %>% filter(str_detect(immatriculation, "^[0-9]{4} [A-Z]{2} [0-9]{2}$"))
    }

    if("marque" %in% names(data)) {
        data <- data %>% filter(nchar(marque) > 0)
    }

    if("puissance" %in% names(data)) {
        data <- data %>% filter(puissance >= 55 & puissance <= 507)
    }

    if("prix" %in% names(data)) {
        data <- data %>% filter(prix >= 7500 & prix <= 101300)
    }


    if("sexe" %in% names(data)) {
      data <- data %>% mutate(
        sexe = case_when(
          sexe %in% c("Homme", "Masculin") ~ "M",
          sexe %in% c("Femme", "Féminin") ~ "F",
          TRUE ~ sexe
        )
      ) %>%
        filter(nchar(sexe) > 0, sexe %in% c("F", "M"))
    }


  # Sauvegarde du fichier nettoyé
  cleaned_file_name <- gsub(".csv", "_clean.csv", file_name)
  write.csv(data, file.path(chemin_data, cleaned_file_name), row.names = FALSE)

  message("Nettoyage termine pour ", file_name)
}

# Liste des fichiers à nettoyer
files_to_clean <- c("Catalogue.csv", "Immatriculations.csv", "Clients_7.csv", "Clients_12.csv", "Marketing.csv","Catalogue.csv")

# Application du nettoyage
lapply(files_to_clean, clean_data)

message("Nettoyage termine pour tous les fichiers specifiques.")

