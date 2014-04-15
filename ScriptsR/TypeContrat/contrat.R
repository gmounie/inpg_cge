source("../DataReader/dataReader.R")

library(ggplot2)
library(plyr)


## Durée de la recherche d'emploi
# l'info n'est pas obligatoire pour les 2014, peu remplit !
r14 = data.frame(contrat = as.character(data2014$ContratTravailActuelV2014), enquete=2014)
r13 = data.frame(contrat = as.character(data2013$ContratTravailActuelEnquete2012), enquete=2013)
r12 = data.frame(contrat = as.character(data2012$ContratTravailActuelEnquete2012), enquete=2012)
r11 = data.frame(contrat = as.character(data2011$ContratTravailActuel), enquete=2011)
r10 = data.frame(contrat = c(as.character(data2010_2009$emp.actuel.contrat), as.character(data2010_2008$emp.actuel.contrat)), enquete=2010)

# pas d'information dans l'enquete 2014
r = rbind(r14, r13,r12,r11,r10)
r$enquete = factor(r$enquete)
r= r[r$contrat != "",]
#levels(r$durée)
#levels(r$durée) = c("","6 mois ou plus","Contrat avant la sortie de l'école","De 2 à moins de 4 mois","De 4 à moins de 6 mois", "Moins de 2 mois de recherche",  "Contrat avant la sortie de l'école", "Moins de 2 mois de recherche", "Moins de 2 mois de recherche" )

## r$durée = relevel(r$durée, "6 mois ou plus")
## r$durée = relevel(r$durée, "De 4 à moins de 6 mois")
## r$durée = relevel(r$durée, "De 2 à moins de 4 mois")
## r$durée = relevel(r$durée, "Moins de 2 mois de recherche")
## r$durée = relevel(r$durée, "Contrat avant la sortie de l'école")


length(r$contrat)
summary(r)
qplot(r)
ggplot(r, aes(x=enquete, fill=contrat)) + geom_bar() + xlab("Année enquête") + ylab("Nombre de diplômés")
ggsave("../../Output/ensimag_duree_recherche.png", width=2*par("din")[1])


## avec la moyenne et la médiane ? boxplot ?










