# Charger le fichier CSV
data <- read.csv("data/Clients_7.csv", header = TRUE, stringsAsFactors = FALSE)

# Supprimer les lignes contenant aucun caractère dans la colonne sexe
data <- data[nchar(data$sexe) > 0, ]

# Transformer "Homme" et "Masculin" en "H" dans la colonne sexe
data$sexe <- gsub("Homme|Masculin", "M", data$sexe)

# Transformer "Femme" et "Feminin" en "F" dans la colonne sexe
data$sexe <- gsub("Femme|Feminin", "F", data$sexe)

# Supprimer les lignes qui ne contiennent pas F ou H dans la colonne sexe
data <- data[data$sexe %in% c("F", "M"), ]

# Écrire le résultat dans un nouveau fichier CSV
write.csv(data, file = "data/Clients_7_filtered.csv", row.names = FALSE)
