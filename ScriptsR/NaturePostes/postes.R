## nature des postes: comme pour les secteurs ?

source("../DataReader/dataReader.R")

library(ggplot2)
library(plyr)

postes = data2013$NaturePosteOccupeINPG
a = data.frame(postes = data2013$NaturePosteOccupeINPG, poids= 1, situation=data2013$ActiviteActuelle, situationVolontariat = data2013$NatureMissionVolontariatINPG, apprentissage= data2013$ApprentissageFormationContinueV2013)
a$postes = factor(a$postes, levels=c(levels(a$postes), "Doctorant", "Volontaire"))
a[a$situation == "En thèse",]$postes = "Doctorant"
a[a$situation == "Volontariat",]$postes = "Volontaire"
#a[a$situation == "Volontariat",]$postes = a[a$situation == "Volontariat",]$situationVolontariat
length(a$postes[a$postes != ""])
ddply(a, .(postes), summarize, nb=round(100*sum(poids)/274, digits=1))



val = count(data2013)
val2 = val
val2$freq = val$freq / sum(val$freq)
val3 = val2
p = ggplot(a, aes(x=factor(postes), weight=poids/(length(a$postes)))) + geom_bar(fill="lightgreen", colour="darkgreen") + coord_flip() + opts(title="Nature des postes") + xlab("") + ylab("Pourcentage") 
ggsave("../../Output/ensimag_2013_postes.svg", width=2*par("din")[1])
