source("../DataReader/dataReader.R")
data2013 = data2013[data2013$AnneeEnquete == 2013,]


taux_emplois = data.frame(situation=data2013$ActiviteActuelle, activitesOLD=data2013$ActiviteActuelleV2010, promo=data2013$PromoEnquete2013, poids=100, nb=1)
taux_emplois[taux_emplois$promo == 2011,"nb"] = length(taux_emplois[taux_emplois$promo == 2011,"situation"])
taux_emplois[taux_emplois$promo == 2012,"nb"] = length(taux_emplois[taux_emplois$promo == 2012,"situation"])
taux_emplois[,"poids"] = 100/taux_emplois[,"nb"]
taux_emplois$situation = relevel(taux_emplois$situation, "En recherche d'emploi")
taux_emplois$situation = relevel(taux_emplois$situation, "Volontariat")
taux_emplois$situation = relevel(taux_emplois$situation, "En thèse")
taux_emplois$situation = relevel(taux_emplois$situation, "Création d'entreprise (en projet)")
taux_emplois$situation = relevel(taux_emplois$situation, "En activité professionnelle")
taux_emplois$situation = relevel(taux_emplois$situation, "")
svg("../../Output/ensimag_2013_situation.svg")
library(ggplot2)
p = ggplot(data=taux_emplois, aes(x=as.factor(promo), fill=situation, weight=poids), colour=black)  + geom_bar()
p + scale_fill_manual(values=c("blue","green","SlateBlue4","chartreuse4","red","yellow", "orange")) + opts(title="Situation des diplômés début 2O13, à 6 et 18 mois") + xlab("Promo Ensimag") + ylab("Pourcentage")
dev.off()   



tempsrecherche = data.frame(situation=data2013$ActiviteActuelle, activitesOLD=data2013$ActiviteActuelleV2010, promo=data2013$PromoEnquete2013, duree=data2013$DureeRechercheEmploiINPG.2, poids=100, nb=1)
length(tempsrecherche[tempsrecherche$situation == "En activité professionnelle" & tempsrecherche$promo == 2012 & tempsrecherche$duree > 0 & ! is.na(tempsrecherche$duree),"duree"])
tempsrecherche$situation








