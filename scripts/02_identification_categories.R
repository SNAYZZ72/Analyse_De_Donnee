# #--creation de 6 categories (citadine, compact, routière, familiale, sportive, berline)
#
# catalogue$categories <- ifelse(catalogue$longueur=="courte","citadine",
#                                ifelse(catalogue$longueur=="moyenne","compacte",
#                                       ifelse(catalogue$longueur=="longue"& catalogue$nbPlaces== 5& catalogue$puissance<180,"routi?re",
#                                              ifelse(catalogue$longueur=="longue"&catalogue$nbPlaces== 7&catalogue$puissance<180,"familiale",
#                                                     ifelse(catalogue$longueur=="longue" | catalogue$longueur=="très longue" & catalogue$puissance >180 & catalogue$puissance <300,"sportive",
#                                                            ifelse(catalogue$longueur == "très longue" & catalogue$puissance >300, "berline","rien"))))))
#
#
# #------ applications au fichiers Immatriculation.csv
#
#
#
# immatriculation$categories <- ifelse(immatriculation$longueur=="courte","citadine",
#                                      ifelse(immatriculation$longueur=="moyenne","compacte",
#                                             ifelse(immatriculation$longueur=="longue"& immatriculation$nbPlaces== 5& immatriculation$puissance<180,"routière",
#                                                    ifelse(immatriculation$longueur=="longue"&immatriculation$nbPlaces== 7&immatriculation$puissance<180,"familiale",
#                                                           ifelse(immatriculation$longueur=="longue" | immatriculation$longueur=="très longue" & immatriculation$puissance >180 & immatriculation$puissance <300,"sportive",
#                                                                  ifelse(immatriculation$longueur == "très longue" & immatriculation$puissance >300, "berline","rien"))))))

# Chargement des packages
library(cluster)
library(ggplot2)
library(dplyr)

# Chargement des données
data <- read.csv("data/cleaned/Catalogue_clean.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")

# Sélection des variables pertinentes
selected_data <- data[, c("puissance", "longueur", "nbPlaces", "nbPortes", "prix")]

# Normalisation des données
normalized_data <- scale(selected_data)

# Choix du nombre de clusters
# Par exemple, utilisons la méthode du coude pour déterminer le nombre optimal de clusters
wss <- (nrow(normalized_data)-1)*sum(apply(normalized_data,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Nombre de clusters", ylab="Within groups sum of squares")

# À partir du graphique, choisissons un nombre de clusters (par exemple, 4)

# Application de l'algorithme de clustering (K-means)
k <- 4
clusters <- kmeans(normalized_data, centers=k)

# Ajout des clusters à nos données
data$cluster <- clusters$cluster

# Analyse des clusters
cluster_summary <- data %>% group_by(cluster) %>% summarize_all(mean)

# Visualisation des clusters
ggplot(data, aes(x = puissance, y = prix, color = factor(cluster))) + geom_point() + labs(title = "Clustering des voitures", x = "Puissance", y = "Prix") + theme_minimal()
