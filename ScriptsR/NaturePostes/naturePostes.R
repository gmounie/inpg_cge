## nature des postes: comme pour les secteurs ?
source("../DataReader/dataReader.R")
data2013 = data2013[data2013$AnneeEnquete == 2013,]

library(ggplot2)
library(plyr)

postes = data2013$NaturePosteOccupeINPG[data2013$AnneeEnquete == 2013]
a = data.frame(postes = data2013$NaturePosteOccupeINPG, poids= 1, situation=data2013$ActiviteActuelle, situationVolontariat = data2013$SecteurActiviteEntrepriseVolontariat, apprentissage= data2013$ApprentissageFormationContinueV2013)
a$postes = factor(a$postes, levels=c(levels(a$postes), "Chercheur (doctorant)"))
a[a$situation == "En thèse",]$postes = "Chercheur (doctorant)"
nbp = length(a$postes[a$postes != ""])
nbp
ddply(a, .(postes), summarize, nb=round(100*sum(poids)/nbp, digits=1))

b = a[a$apprentissage == "En apprentissage",] 
nbpb = length(b$postes[b$postes != ""])
nbpb
ddply(b, .(postes), summarize, nb=round(100*sum(poids)/nbpb, digits=0)) 









