# Vérification et installation des packages nécessaires de manière plus concise
packages_needed <- c("RODBC", "stringr", "ggplot2", "C50", "randomForest", "naivebayes", "e1071", "nnet", "kknn", "pROC")
new_packages <- packages_needed[!packages_needed %in% installed.packages()[, "Package"]]
if (length(new_packages)) install.packages(new_packages)

# Activation des librairies avec lapply pour plus de concision
lapply(packages_needed, library, character.only = TRUE)



# Connexion à la base de données Oracle
connexion <- odbcConnect("ORCLPROJETDB_DNS", uid="PROJET", pwd="123", believeNRows=FALSE)

# Fonction pour charger et nettoyer les données
charger_et_nettoyer <- function(nom_table) {
  requete <- sprintf("SELECT * FROM %s", nom_table)
  df <- sqlQuery(connexion, requete, stringsAsFactors = FALSE)

  if("longueur" %in% names(df)) {
    df$longueur <- str_replace_all(df$longueur, "très longue", "tres longue")
  }

  if("marque" %in% names(df)) {
    df$marque <- str_replace_all(df$marque, "Hyundaï", "Hyundai")
  }

  if("situationFamiliale" %in% names(df)) {
    df$situationFamiliale <- str_replace_all(df$situationFamiliale, "Célibataire", "celibatire")
    df$situationFamiliale <- str_replace_all(df$situationFamiliale, "Marié(e)", "Marie(e)")
    df$situationFamiliale <- str_replace_all(df$situationFamiliale, "Divorcée", "Divorcee")
    df <- df[df$situationFamiliale != "N/D" & nchar(df$situationFamiliale) > 0, ]
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

  if("sexe" %in% names(df)) {
    df$sexe <- ifelse(df$sexe %in% c("Homme", "Masculin"), "M",
                      ifelse(df$sexe %in% c("Femme", "Féminin"), "F", df$sexe))
  }


  # Convertir les colonnes selon un mapping prédéfini
  mappings <- list(
    age = as.integer,
    taux = as.integer,
    situationFamiliale = as.factor,
    nbEnfantsAcharge = as.integer,
    `X2eme.voiture` = as.logical,
    sexe = as.factor,
    marque = as.character,
    nom = as.character,
    puissance = as.integer,
    longueur = as.character,
    nbplaces = as.integer,
    nbportes = as.integer,
    couleur = as.character,
    occasion = as.character,
    prix = as.integer,
    immatriculation = as.character
  )

  for (col in names(mappings)) {
    if (col %in% names(df)) {
      df[[col]] <- mappings[[col]](df[[col]])
    }
  }

  na.omit(df)
}

# Utilisation de la fonction pour charger et nettoyer les données
immatriculation <- charger_et_nettoyer("IMMATRICULATION")
marketing <- charger_et_nettoyer("MARKETING")
catalogue <- charger_et_nettoyer("CATALOGUE")
clients <- charger_et_nettoyer("CLIENTS")
