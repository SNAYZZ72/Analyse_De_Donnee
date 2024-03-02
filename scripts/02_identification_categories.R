#--creation de 6 categories (citadine, compact, routière, familiale, sportive, berline)

catalogue$categories <- ifelse(catalogue$longueur=="courte","citadine",
                               ifelse(catalogue$longueur=="moyenne","compacte",
                                      ifelse(catalogue$longueur=="longue"& catalogue$nbPlaces== 5& catalogue$puissance<180,"routi?re",
                                             ifelse(catalogue$longueur=="longue"&catalogue$nbPlaces== 7&catalogue$puissance<180,"familiale",
                                                    ifelse(catalogue$longueur=="longue" | catalogue$longueur=="très longue" & catalogue$puissance >180 & catalogue$puissance <300,"sportive",
                                                           ifelse(catalogue$longueur == "très longue" & catalogue$puissance >300, "berline","rien"))))))


#------ applications au fichiers Immatriculation.csv



immatriculation$categories <- ifelse(immatriculation$longueur=="courte","citadine",
                                     ifelse(immatriculation$longueur=="moyenne","compacte",
                                            ifelse(immatriculation$longueur=="longue"& immatriculation$nbPlaces== 5& immatriculation$puissance<180,"routière",
                                                   ifelse(immatriculation$longueur=="longue"&immatriculation$nbPlaces== 7&immatriculation$puissance<180,"familiale",
                                                          ifelse(immatriculation$longueur=="longue" | immatriculation$longueur=="très longue" & immatriculation$puissance >180 & immatriculation$puissance <300,"sportive",
                                                                 ifelse(immatriculation$longueur == "très longue" & immatriculation$puissance >300, "berline","rien"))))))
