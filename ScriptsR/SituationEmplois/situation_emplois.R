source("script_data.R")
data2013 = data2013[data2013$AnneeEnquete == 2013,]


taux_emplois = data.frame(situation=data2013$ActiviteActuelle, activitesOLD=data2013$ActiviteActuelleV2010, promo=data2013$PromoEnquete2013, poids=100, nb=1)
taux_emplois[taux_emplois$promo == 2011,"nb"] = length(taux_emplois[taux_emplois$promo == 2011,"situation"])
taux_emplois[taux_emplois$promo == 2012,"nb"] = length(taux_emplois[taux_emplois$promo == 2012,"situation"])
taux_emplois$situation = relevel(taux_emplois$situation, "En recherche d'emploi")
taux_emplois$situation = relevel(taux_emplois$situation, "En thèse")
taux_emplois$situation = relevel(taux_emplois$situation, "Création d'entreprise (en projet)")
taux_emplois$situation = relevel(taux_emplois$situation, "En activité professionnelle")
taux_emplois$situation = relevel(taux_emplois$situation, "")
svg("ensimag_2013_situation.svg")
library(ggplot2)
p = ggplot(data=taux_emplois, aes(x=as.factor(promo), fill=situation, weigth=poids/nb), colour=black)  + geom_bar()
p + scale_fill_manual(values=c("white", "blue","green","blue3","red","yellow", "orange")) + opts(title="Situation des diplômés début 2O13, 6 et 18 mois") + xlab("Promo Ensimag") + ylab("Pourcentage")
dev.off()   






