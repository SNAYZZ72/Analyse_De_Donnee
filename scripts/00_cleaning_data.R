# Vérification et installation des packages nécessaires
packages_needed <- c("cluster", "ggplot2", "stringr")
packages_to_install <- packages_needed[!(packages_needed %in% installed.packages()[,"Package"])]
if(length(packages_to_install)) install.packages(packages_to_install)

# Activation des librairies
library(cluster)
library(ggplot2)
library(stringr)

clean_text_columns <- function(df) {
  # Convertir toutes les colonnes texte en utilisant l'encodage UTF-8
  text_cols <- sapply(df, is.character)
  df[text_cols] <- lapply(df[text_cols], function(x) iconv(x, "latin1", "ASCII//TRANSLIT"))

  df
}

# Chemin vers données
chemin_data <- "data/sans_accents"
chemin_data_cleaned <- "data/cleaned/"

# Fonction de nettoyage générique
clean_data <- function(file_name) {
  data_path <- file.path(chemin_data, file_name)
  if(!file.exists(data_path)) {
    message("Le fichier ", data_path, " n'existe pas.")
    return(NULL)
  }
  data <- read.csv(data_path, header = TRUE, sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")

  # Nettoyage des colonnes texte avec des caractères spéciaux
  data <- clean_text_columns(data)


  # # remplacer tr<e8>s longue par tres longue dans la colonne longueur
  # if("longueur" %in% names(data)) {
  #   data <- data %>% mutate(
  #     longueur = str_replace(longueur, "très longue", "tres longue")
  #   )
  # }
  #
  # #remplacer Hyunda<ef> par Hyundai dans la colonne marque
  # if("marque" %in% names(data)) {
  #   data <- data %>% mutate(
  #     marque = str_replace(marque, "Hyundaï", "Hyundai")
  #   )
  # }
  #

  #################Client#################

  #On garde que les lignes avec des ages entre 18 et 84
    if("age" %in% names(data)) {
        data <- data %>% filter(age >= 18 & age <= 84)
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

  #On convertie le taux écrit en chaine de caractère en integer, puis on garde que les lignes avec des taux entre 544 et 74185
    if("taux" %in% names(data)) {
        data <- data %>% mutate(taux = as.integer(taux)) %>%
            filter(taux >= 544 & taux <= 74185)
    }

  if("situationFamiliale" %in% names(data)) {
    data <- data %>% filter(situationFamiliale %in% c("Celibataire", "Divorcee", "En Couple", "Marie(e)", "Seul", "Seule"))
  }

  #On garde que les lignes avec des nombres d'enfants à charge entre 0 et 4
    if("nbEnfantsAcharge" %in% names(data)) {
        data <- data %>% filter(nbEnfantsAcharge >= 0 & nbEnfantsAcharge <= 4)
    }

  #On garde que les lignes qui on comme valeurs true ou false. Puis on convertie en booléen
    if("X2eme.voiture" %in% names(data)) {
        data <- data %>% filter(X2eme.voiture %in% c("true", "false")) %>%
            mutate(X2eme.voiture = X2eme.voiture == "true")
    }

  if("immatriculation" %in% names(data)) {
    data <- data %>% filter(str_detect(immatriculation, "^[0-9]{4} [A-Z]{2} [0-9]{2}$"))
  }

  ##########################Immatriculation################################

  #On garde que les lignes où la marque est : Audi, BMW, Dacia, Daihatsu, Fiat, Ford, Honda, Hyundai, Jaguar, Kia, Lancia, Mercedes, Mini, Nissan, Peugeot, Renault, Saab, Seat, Skoda, Volkswagen, Volvo
    if("marque" %in% names(data)) {
        data <- data %>% filter(marque %in% c("Audi", "BMW", "Dacia", "Daihatsu", "Fiat", "Ford", "Honda", "Hyundai", "Jaguar", "Kia", "Lancia", "Mercedes", "Mini", "Nissan", "Peugeot", "Renault", "Saab", "Seat", "Skoda", "Volkswagen", "Volvo"))
    }

  #On garde que les lignes avec des puissances entre 55 et 507
    if("puissance" %in% names(data)) {
        data <- data %>% filter(puissance >= 55 & puissance <= 507)
    }

  #On garde que les lignes avec une longueur qui est soit : courte, moyenne, longue, tres longue
    if("longueur" %in% names(data)) {
        data <- data %>% filter(longueur %in% c("courte", "moyenne", "longue", "tres longue"))
    }

  #On garde que  les lignes avec des nombres de places entre 5 et 7
    if("nbPlaces" %in% names(data)) {
        data <- data %>% filter(nbPlaces >= 5 & nbPlaces <= 7)
    }

    #On garde que les lignes avec des nombres de portes entre 3 et 5
    if("nbPortes" %in% names(data)) {
        data <- data %>% filter(nbPortes >= 3 & nbPortes <= 5)
    }

    #On garde que les lignes avec des couleurs qui sont soit : blanc, bleu, gris, noir, rouge
    if("couleur" %in% names(data)) {
        data <- data %>% filter(couleur %in% c("blanc", "bleu", "gris", "noir", "rouge"))
    }

    #On garde que les lignes avec des valeurs true ou false. Puis on convertie les valeurs de cette ligne de chaines de caractères à booléen donc la chaine de caractère "true" devient TRUE et la chaine de caractère "false" devient FALSE
    if("occasion" %in% names(data)) {
        data <- data %>% filter(occasion %in% c("true", "false")) %>%
            mutate(occasion = occasion == "true")
    }

    #On garde que les lignes avec des prix entre 7500 et 101300
    if("prix" %in% names(data)) {
        data <- data %>% filter(prix >= 7500 & prix <= 101300)
    }

#################################Marketing######################################



  # Sauvegarde du fichier nettoyé
  cleaned_file_name <- gsub(".csv", "_clean.csv", file_name)
  write.csv(data, file.path(chemin_data_cleaned, cleaned_file_name), row.names = FALSE)

  message("Nettoyage termine pour ", file_name)
}

# Liste des fichiers à nettoyer
files_to_clean <- c("Catalogue_sans_accents.csv", "Immatriculations_sans_accents.csv")

# Application du nettoyage
lapply(files_to_clean, clean_data)

message("Nettoyage termine pour tous les fichiers specifiques.")