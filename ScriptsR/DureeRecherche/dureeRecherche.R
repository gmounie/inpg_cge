source("../DataReader/dataReader.R")

library(ggplot2)
library(plyr)


## Durée de la recherche d'emploi
# l'info n'est pas obligatoire pour les 2014, peu remplit !

r15promo = data.frame(durée = as.factor(data2015$DureePourTrouverEmploiApresSortie), promo = data2015$Promo,  enquete=2015, filiere=data2015$Option_FiliereFormation)
length(r15promo[r15promo$promo == 2014,]$durée)
summary(r15promo[r15promo$promo == 2014,]$durée)
g = ggplot(data=r15promo[r15promo$promo == 2014,], aes(x=durée)) + geom_histogram()
g
length(r15promo[r15promo$promo == 2014,]$durée)

r15 = data.frame(durée = as.character(data2015$DureePourTrouverEmploiApresSortie), enquete=2015)
r14 = data.frame(durée = as.character(data2014$DureeRecherchePremierEmploi), enquete=2014)
r13 = data.frame(durée = as.character(data2013$DureeRecherchePremierEmploi), enquete=2013)
r12 = data.frame(durée = as.character(data2012$DureeRecherchePremierEmploi), enquete=2012)
r11 = data.frame(durée = as.character(data2011$TempsTrouverEmploiINPG), enquete=2011)
r10 = data.frame(durée = c(as.character(data2010_2009$emp.actuel.duree.recherche), as.character(data2010_2008$emp.actuel.duree.recherche)), enquete=2010)

# pas d'information dans l'enquete 2014
r = rbind(r15,r14,r13,r12,r11,r10)
r$enquete = factor(r$enquete)
r= r[r$durée != "",]
levels(r$durée)
levels(r$durée) = c("","6 mois ou plus","Contrat avant la sortie de l'école","De 2 à moins de 4 mois","De 4 à moins de 6 mois", "Moins de 2 mois de recherche",  "Contrat avant la sortie de l'école", "Moins de 2 mois de recherche", "Moins de 2 mois de recherche" )

r$durée = relevel(r$durée, "6 mois ou plus")
r$durée = relevel(r$durée, "De 4 à moins de 6 mois")
r$durée = relevel(r$durée, "De 2 à moins de 4 mois")
r$durée = relevel(r$durée, "Moins de 2 mois de recherche")
r$durée = relevel(r$durée, "Contrat avant la sortie de l'école")


length(r$durée)
summary(r)
qplot(r)
ggplot(r, aes(x=enquete, fill=durée)) + geom_bar() + xlab("Année enquête") + ylab("Nombre de diplômés")
ggsave("../../Output/ensimag_duree_recherche.png", width=2*par("din")[1])


## avec la moyenne et la médiane ? boxplot ?










