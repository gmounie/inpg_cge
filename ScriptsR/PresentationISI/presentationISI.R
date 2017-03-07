source("../DataReader/dataReader.R")
data2013 = data2013[data2013$AnneeEnquete == 2013,]

dataISI = data2017[data2017$X247..Option_ScolariteFiliereFormation == "ISI – ingénierie des systèmes d’information",]
sink(file="sortie.txt", append=TRUE, split=TRUE)
print("Nb réponses en ISI à 6, 18 et 30 mois")
length(dataISI$X166..Nom)

                                        # version 2017
                                        #Postes
postes = data.frame(poste=dataISI$X245..Option_EmploiPosteListe, activité=dataISI$X26..ActiviteActuelle, secteur=dataISI$X58..EmploiEntrepriseSecteurActivite)
levels(postes$poste) = c(levels(postes$poste), "doctorant")
postes[postes$activité == "Studying for a PhD",]$poste = "doctorant"
print("Postes occupés")
table(postes$poste)

                                        # situation emploi
print("Situation")
table(postes$activité)

                                        # secteurs
levels(postes$secteur) = c(levels(postes$secteur), "Recherche (doctorat)")
postes[postes$activité == "Studying for a PhD",]$secteur = "Recherche (doctorat)"
print("Secteur")
table(postes$secteur)
sink()

