## nature des postes: comme pour les secteurs ?

source("../DataReader/dataReader.R")

library(ggplot2)
library(plyr)
                                        # 2016
postes = data2016$Option_PosteSurListe
a = data.frame(postes = data2016$Option_PosteSurListe, poids= 1, situation=data2016$ActiviteActuelleV2016)
a$postes = factor(a$postes, levels=c(levels(a$postes), "Doctorant", "Volontaire"))
a[a$situation == "En thèse",]$postes = "Doctorant"
a[a$situation == "En volontariat (VIE, VIA, Volontariat civil)",]$postes = "Volontaire"

p = ggplot(a, aes(x=factor(postes), weight=poids/(length(a$postes)))) + geom_bar(fill="lightgreen", colour="darkgreen") + coord_flip() + theme(title=element_text("Nature des postes")) + xlab("") + ylab("Pourcentage") 
p
ggsave("../../Output/ensimag_2016_postes.svg", width=2*par("din")[1])
ggsave("../../Output/ensimag_2016_postes.png", width=2*par("din")[1])

                                        # 2015
postes = data2015$NaturePosteOccupeINPG
a = data.frame(postes = data2015$Option_PosteSurListe, poids= 1, situation=data2015$ActiviteActuelleV2015)
a$postes = factor(a$postes, levels=c(levels(a$postes), "Doctorant", "Volontaire"))
a[a$situation == "En thèse",]$postes = "Doctorant"
a[a$situation == "Volontariat",]$postes = "Volontaire"

p = ggplot(a, aes(x=factor(postes), weight=poids/(length(a$postes)))) + geom_bar(fill="lightgreen", colour="darkgreen") + coord_flip() + theme(title=element_text("Nature des postes")) + xlab("") + ylab("Pourcentage") 
p
ggsave("../../Output/ensimag_2015_postes.svg", width=2*par("din")[1])
ggsave("../../Output/ensimag_2015_postes.png", width=2*par("din")[1])


                                        # 2013

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
p = ggplot(a, aes(x=factor(postes), weight=poids/(length(a$postes)))) + geom_bar(fill="lightgreen", colour="darkgreen") + coord_flip() + theme(title=element_text("Nature des postes")) + xlab("") + ylab("Pourcentage") 
p
ggsave("../../Output/ensimag_2013_postes.svg", width=2*par("din")[1])

postes2008_2013 = factor(c( as.character(data2010_2008$emp.actuel.poste), as.character(data2011$NaturePosteOccupeINPG[data2011$Promo == 2009]), as.character(data2012$NaturePosteOccupeINPG[data2012$AnneeDiplome == 2010]), as.character(data2013$NaturePosteOccupeINPG[data2013$AnneeEnquete == 2013 && data2013$AnneeDiplome == 2011]), as.character(data2014$NaturePosteOccupeINPG[data2014$AnneeEnquete == 2014 && data2014$AnneeDiplome == 2012]) ) )

length(postes2008_2013)
postesALL = postes2008_2013[postes2008_2013 != ""]
qplot(postesALL) + coord_flip()

length(postesALL)
summary(postesALL)












