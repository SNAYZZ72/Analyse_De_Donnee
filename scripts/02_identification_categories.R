# Importation des données depuis le fichier catalogue_clean.csv
catalogue <- read.csv("C:\\data_analytics\\data\\cleaned\\Catalogue_clean.csv")

# Code pour identifier les catégories de véhicules à partir du catalogue
catalogue$categories <- ifelse(catalogue$longueur=="courte","citadine",
                               ifelse(catalogue$longueur=="moyenne","compacte",
                                      ifelse(catalogue$longueur=="longue"& catalogue$nbPlaces== 5& catalogue$puissance<180,"routière",
                                             ifelse(catalogue$longueur=="longue"&catalogue$nbPlaces== 7&catalogue$puissance<180,"familiale",
                                                    ifelse(catalogue$longueur=="longue" | catalogue$longueur=="très longue" & catalogue$puissance >180 & catalogue$puissance <300,"sportive",
                                                           ifelse(catalogue$longueur == "très longue" & catalogue$puissance >300, "berline","rien"))))))

# Affichage des premières lignes du catalogue avec les catégories ajoutées
head(catalogue)
