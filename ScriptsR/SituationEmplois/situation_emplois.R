source("../DataReader/dataReader.R")
data2013 = data2013[data2013$AnneeEnquete == 2013,]


# version 2013

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
library(ggplot2)
p = ggplot(data=taux_emplois, aes(x=as.factor(promo), fill=situation, weight=poids), colour=black)  + geom_bar()
p + scale_fill_manual(values=c("blue","green","SlateBlue4","chartreuse4","red","yellow", "orange")) + opts(title="Situation des diplômés début 2O13, à 6 et 18 mois") + xlab("Promo Ensimag") + ylab("Pourcentage")
ggsave("../../Output/ensimag_2013_situation.svg")   
ggsave("../../Output/ensimag_2013_situation.pdf") 

# version 2012
taux_emplois12 = data.frame(situation=data2012$ActiviteActuelle, activitesOLD=data2012$ActiviteActuelleV2010, promo=data2012$PromoEnquete2012, poids=100, nb=1)
taux_emplois12[taux_emplois12$activitesOLD == "En création d'entreprise","situation"] = "En création d'entreprise"
taux_emplois12 = taux_emplois12[taux_emplois12$situation != "",]
taux_emplois12[taux_emplois12$promo == 2010,"nb"] = length(taux_emplois12[taux_emplois12$promo == 2010,"situation"])
taux_emplois12[taux_emplois12$promo == 2011,"nb"] = length(taux_emplois12[taux_emplois12$promo == 2011,"situation"])
taux_emplois12[,"poids"] = 100/taux_emplois12[,"nb"]
taux_emplois12$situation = relevel(taux_emplois12$situation, "En recherche d'emploi")
taux_emplois12$situation = relevel(taux_emplois12$situation, "Volontariat")
taux_emplois12$situation = relevel(taux_emplois12$situation, "En thèse")
taux_emplois12$situation = relevel(taux_emplois12$situation, "En création d'entreprise")
taux_emplois12$situation = relevel(taux_emplois12$situation, "En activité professionnelle")
p = ggplot(data=taux_emplois12, aes(x=as.factor(promo), fill=situation, weight=poids), colour=black)  + geom_bar()
p + scale_fill_manual(values=c("blue","green","SlateBlue4","chartreuse4","red","yellow", "orange")) + opts(title="Situation des diplômés début 2O12, à 6 et 18 mois") + xlab("Promo Ensimag") + ylab("Pourcentage")
ggsave("../../Output/ensimag_2012_situation.svg")   
ggsave("../../Output/ensimag_2012_situation.pdf")








