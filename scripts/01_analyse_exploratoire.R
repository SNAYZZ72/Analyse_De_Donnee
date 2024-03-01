
# Activation des librairies
library(cluster)
library(ggplot2)
library(stringr)

# # Chemin vers les données nettoyées
# chemin_data <- "data/"
#

# Charger le fichier CSV en spécifiant l'encodage si nécessaire
clients_7 <- read.csv("data/Clients_7_sans_accents.csv", header = TRUE, fileEncoding = "UTF-8")
clients_12 <- read.csv("data/Clients_12_sans_accents.csv", header = TRUE, fileEncoding = "UTF-8")
catalogue <- read.csv("data/Catalogue_sans_accents.csv", header = TRUE, fileEncoding = "UTF-8")
# immatriculation <- read.csv("data/Immatriculation.csv", header = TRUE, fileEncoding = "UTF-8")
marketing <- read.csv("data/Marketing_sans_accents.csv", header = TRUE, fileEncoding = "UTF-8")


# Statistiques descriptives
# skim(catalogue)
# skim(clients_7)
# skim(clients_12)
# skim(immatriculation)
# skim(marketing)

# Afficher un histogramme des données du fichier Client_7

ggplot(clients_7, aes(x = age)) + geom_bar()
ggplot(clients_7, aes(x = sexe)) + geom_bar()
ggplot(clients_7, aes(x = taux)) + geom_bar()
ggplot(clients_7, aes(x = situationFamiliale)) + geom_bar()
ggplot(clients_7, aes(x = nbEnfantsAcharge)) + geom_bar()
ggplot(clients_7, aes(x = X2eme.voiture)) + geom_bar()
#ggplot(clients_7, aes(x = immatriculation)) + geom_bar()  # tourne en boucle

print("Done client7")

# Afficher un histogramme des données du fichier Client_12

ggplot(clients_12, aes(x = age)) + geom_bar()
ggplot(clients_12, aes(x = sexe)) + geom_bar()
ggplot(clients_12, aes(x = taux)) + geom_bar()
ggplot(clients_12, aes(x = situationFamiliale)) + geom_bar()
ggplot(clients_12, aes(x = nbEnfantsAcharge)) + geom_bar()
ggplot(clients_12, aes(x = X2eme.voiture)) + geom_bar()
# ggplot(clients_12, aes(x = immatriculation)) + geom_bar()

print("Done client12")

# Afficher un histogramme des données du fichier Catalogue (marque,nom,puissance,longueur,nbPlaces,nbPortes,couleur,occasion,prix)

ggplot(catalogue, aes(x = marque)) + geom_bar()
ggplot(catalogue, aes(x = nom)) + geom_bar()
ggplot(catalogue, aes(x = puissance)) + geom_histogram()
ggplot(catalogue, aes(x = longueur)) + geom_bar()
ggplot(catalogue, aes(x = nbPlaces)) + geom_bar()
ggplot(catalogue, aes(x = nbPortes)) + geom_bar()
ggplot(catalogue, aes(x = couleur)) + geom_bar()
ggplot(catalogue, aes(x = occasion)) + geom_bar()
ggplot(catalogue, aes(x = prix)) + geom_histogram()

print("Done catalogue")

# Afficher un histogramme des données du fichier Marketing (age,sexe,taux,situationFamiliale,nbEnfantsAcharge,2eme voiture)

ggplot(marketing, aes(x = age)) + geom_histogram()
ggplot(marketing, aes(x = sexe)) + geom_bar()
ggplot(marketing, aes(x = taux)) + geom_histogram( )
ggplot(marketing, aes(x = situationFamiliale)) + geom_bar()
ggplot(marketing, aes(x = nbEnfantsAcharge)) + geom_bar()
ggplot(marketing, aes(x = X2eme.voiture)) + geom_bar()

print("Done marketing")

# Afficher un histogramme des données du fichier Immatriculation (immatriculation, marque,nom,puissance,longueur,nbPlaces,nbPortes,couleur,occasion,prix

# ggplot(immatriculation, aes(x = immatriculation)) + geom_bar()
# ggplot(immatriculation, aes(x = marque)) + geom_bar()
# ggplot(immatriculation, aes(x = nom)) + geom_bar()
# ggplot(immatriculation, aes(x = puissance)) + geom_bar()
# ggplot(immatriculation, aes(x = longueur)) + geom_bar()
# ggplot(immatriculation, aes(x = nbPlaces)) + geom_bar()
# ggplot(immatriculation, aes(x = nbPortes)) + geom_bar()
# ggplot(immatriculation, aes(x = couleur)) + geom_bar()
# ggplot(immatriculation, aes(x = occasion)) + geom_bar()
# ggplot(immatriculation, aes(x = prix)) + geom_bar()


