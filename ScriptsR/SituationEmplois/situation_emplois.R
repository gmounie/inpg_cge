source("../DataReader/dataReader.R")
data2013 = data2013[data2013$AnneeEnquete == 2013,]

library("dplyr")
library("ggplot2")

## version 2019
df = data.frame(situation=data2019$X15..ActiviteActuelle, promo=data2019$X14..AnneeDiplomeVerifieParLeDiplome, filiere=data2019$X264..Option_ScolariteFiliereFormation)


df = df %>% group_by(promo) %>% mutate(nbrep=n())
df %>% select(promo, situation, nbrep) %>% group_by(promo, situation) %>% summarize(activite=round(100* n()/min(nbrep)))

p = ggplot(data=df, aes(x=as.factor(promo), fill=situation, weight=1/nbrep), colour=black)  + geom_bar() + coord_flip()
p + scale_fill_manual(values=c("white", "yellow","red","SlateBlue4","green","chartreuse4","blue", "orange")) + ggtitle("Situation des diplômés début 2O19, à 6, 18 et 30 mois") + xlab("Promo Ensimag") + ylab("Pourcentage")
ggsave("../../Output/ensimag_2019_situation.svg")   
ggsave("../../Output/ensimag_2019_situation.png", width=2*par("din")[1]) 


## version 2018
df = data.frame(situation=data2018$X20..ActiviteActuelle, promo=data2018$X14..AnneeDiplomeVerifieParLeDiplome, filiere=data2018$X258..Option_ScolariteFiliereFormation)

df = df %>% group_by(promo) %>% mutate(nbrep=n())
df %>% select(promo, situation, nbrep) %>% group_by(promo, situation) %>% summarize(activite=round(100* n()/min(nbrep)))

p = ggplot(data=df, aes(x=as.factor(promo), fill=situation, weight=1/nbrep), colour=black)  + geom_bar() + coord_flip()
p + scale_fill_manual(values=c("yellow","red","SlateBlue4","green","chartreuse4","blue", "orange")) + ggtitle("Situation des diplômés début 2O18, à 6, 18 et 30 mois") + xlab("Promo Ensimag") + ylab("Pourcentage")
ggsave("../../Output/ensimag_2018_situation.svg")   
ggsave("../../Output/ensimag_2018_situation.png", width=2*par("din")[1]) 


## version 2017
taux_emplois = data.frame(situation=data2017$X26..ActiviteActuelle, promo=data2017$X21..AnneeDiplomeVerifieParLeDiplome, poids=100, nb=1, poidsTotal=1, filiere=data2017$X247..Option_ScolariteFiliereFormation, poidsFiliere=1)
levels(taux_emplois$filiere) = c("Non renseigné" , "Master" , "Master" , "IF" , "ISI" , "ISSC" , "Master" , "Master" , "Master" , "Master" , "MMIS" , "SLE")
taux_emplois$filiere = factor(taux_emplois$filiere)
taux_emplois = taux_emplois[taux_emplois$situation != "",]
taux_emplois = taux_emplois[! is.na(taux_emplois$promo),]
taux_emplois$poidsTotal = 100./length(taux_emplois$situation)
taux_emplois$poidsFiliere[taux_emplois$filiere == "Master"] = 100./ length(taux_emplois[taux_emplois$filiere == "Master","situation"])
taux_emplois$poidsFiliere[taux_emplois$filiere == "IF"] = 100./ length(taux_emplois[taux_emplois$filiere == "IF","situation"])
taux_emplois$poidsFiliere[taux_emplois$filiere == "MMIS"] = 100. / length(taux_emplois[taux_emplois$filiere == "MMIS","situation"])
taux_emplois$poidsFiliere[taux_emplois$filiere == "ISSC"] = 100. / length(taux_emplois[taux_emplois$filiere == "ISSC","situation"])
taux_emplois$poidsFiliere[taux_emplois$filiere == "SLE"] = 100. / length(taux_emplois[taux_emplois$filiere == "SLE","situation"])
taux_emplois$poidsFiliere[taux_emplois$filiere == "ISI"] = 100. / length(taux_emplois[taux_emplois$filiere == "ISI","situation"])


taux_emplois[taux_emplois$promo == 2014,"nb"] = length(taux_emplois[taux_emplois$promo == 2014,"situation"])
taux_emplois[taux_emplois$promo == 2015,"nb"] = length(taux_emplois[taux_emplois$promo == 2015,"situation"])
taux_emplois[taux_emplois$promo == 2016,"nb"] = length(taux_emplois[taux_emplois$promo == 2016,"situation"])
taux_emplois[,"poids"] = 100./taux_emplois[,"nb"]
levels(taux_emplois$situation)
levels(taux_emplois$situation) = c("Non renseigné" , "Furthering studies" , "Job-hunting" , "Not in activity out of choice" , "Studying for a PhD" , "Voluntary work" , "Working")

library(ggplot2)
p = ggplot(data=taux_emplois, aes(x=as.factor(promo), fill=situation, weight=poids), colour=black)  + geom_bar() + coord_flip()
p + scale_fill_manual(values=c("yellow","red","SlateBlue4","green","chartreuse4","blue", "orange")) + ggtitle("Situation des diplômés début 2O17, à 6, 18 et 30 mois") + xlab("Promo Ensimag") + ylab("Pourcentage")
ggsave("../../Output/ensimag_2017_situation.svg")   
ggsave("../../Output/ensimag_2017_situation.png", width=2*par("din")[1]) 

library("dplyr")
select(taux_emplois , situation, promo, poids) %>% group_by(promo, situation) %>% summarize(nombre = round(sum(poids),1))

# statistiques par filière
a = select(taux_emplois , situation, promo, poidsFiliere, filiere) %>% group_by(filiere, situation) %>% summarize(nombre = round(sum(poidsFiliere),1))
a[1:15,]
a[16:31,]

# version 2016
taux_emplois = data.frame(situation=data2016$ActiviteActuelleV2016, promo=data2016$Promo, poids=100, nb=1)
taux_emplois = taux_emplois[taux_emplois$situation != "",]
taux_emplois[taux_emplois$promo == 2013,"nb"] = length(taux_emplois[taux_emplois$promo == 2013,"situation"])
taux_emplois[taux_emplois$promo == 2014,"nb"] = length(taux_emplois[taux_emplois$promo == 2014,"situation"])
taux_emplois[taux_emplois$promo == 2015,"nb"] = length(taux_emplois[taux_emplois$promo == 2015,"situation"])
taux_emplois[,"poids"] = 100./taux_emplois[,"nb"]
levels(taux_emplois$situation)
taux_emplois$situation = relevel(taux_emplois$situation, 6)
levels(taux_emplois$situation)
taux_emplois$situation = relevel(taux_emplois$situation, "En recherche d'emploi")
levels(taux_emplois$situation)
# poursuite d'étude
taux_emplois$situation = relevel(taux_emplois$situation, 4)

taux_emplois$situation = relevel(taux_emplois$situation, "En volontariat (VIE, VIA, Volontariat civil)")

# accent utf8 vs latin 1
# thèse est le numéro 5
# taux_emplois$situation = relevel(taux_emplois$situation, "En thèse")
levels(taux_emplois$situation)
taux_emplois$situation = relevel(taux_emplois$situation, 6)
                                        #taux_emplois$situation = relevel(taux_emplois$situation, "En création d'entreprise")

# accent utf8 vs latin1
                                        #taux_emplois$situation = relevel(taux_emplois$situation, "En activité professionnelle")
levels(taux_emplois$situation)
taux_emplois$situation = relevel(taux_emplois$situation, 6)
#taux_emplois$situation = relevel(taux_emplois$situation, "")
library(ggplot2)
p = ggplot(data=taux_emplois, aes(x=as.factor(promo), fill=situation, weight=poids), colour=black)  + geom_bar() + coord_flip()
p + scale_fill_manual(values=c("blue","green","SlateBlue4","chartreuse4","red","yellow", "orange")) + ggtitle("Situation des diplômés début 2O16, à 6, 18 et 30 mois") + xlab("Promo Ensimag") + ylab("Pourcentage")
ggsave("../../Output/ensimag_2016_situation.svg")   
ggsave("../../Output/ensimag_2016_situation.png", width=2*par("din")[1]) 



# version 2015
taux_emplois = data.frame(situation=data2015$ActiviteActuelleV2015, promo=data2015$Promo, poids=100, nb=1)
taux_emplois = taux_emplois[taux_emplois$situation != "",]
taux_emplois[taux_emplois$promo == 2012,"nb"] = length(taux_emplois[taux_emplois$promo == 2012,"situation"])
taux_emplois[taux_emplois$promo == 2013,"nb"] = length(taux_emplois[taux_emplois$promo == 2013,"situation"])
taux_emplois[taux_emplois$promo == 2014,"nb"] = length(taux_emplois[taux_emplois$promo == 2014,"situation"])
taux_emplois[,"poids"] = 100/taux_emplois[,"nb"]
taux_emplois$situation = relevel(taux_emplois$situation, "En recherche d'emploi")
taux_emplois$situation = relevel(taux_emplois$situation, "En volontariat")
taux_emplois$situation = relevel(taux_emplois$situation, "En thèse")
taux_emplois$situation = relevel(taux_emplois$situation, "En création d'entreprise")
taux_emplois$situation = relevel(taux_emplois$situation, "En activité professionnelle")
#taux_emplois$situation = relevel(taux_emplois$situation, "")
library(ggplot2)
p = ggplot(data=taux_emplois, aes(x=as.factor(promo), fill=situation, weight=poids), colour=black)  + geom_bar() + coord_flip()
p + scale_fill_manual(values=c("blue","green","SlateBlue4","chartreuse4","red","yellow", "orange")) + ggtitle("Situation des diplômés début 2O15, à 6, 18 et 30 mois") + xlab("Promo Ensimag") + ylab("Pourcentage")
ggsave("../../Output/ensimag_2015_situation.svg")   
ggsave("../../Output/ensimag_2015_situation.png", width=2*par("din")[1]) 


# version 2014
taux_emplois = data.frame(situation=data2014$ActiviteActuelle, activitesOLD=data2014$ActiviteActuelleV2010, promo=data2014$Promo, poids=100, nb=1)
taux_emplois = taux_emplois[taux_emplois$situation != ""]
taux_emplois[taux_emplois$promo == 2012,"nb"] = length(taux_emplois[taux_emplois$promo == 2012,"situation"])
taux_emplois[taux_emplois$promo == 2013,"nb"] = length(taux_emplois[taux_emplois$promo == 2013,"situation"])
taux_emplois[,"poids"] = 100/taux_emplois[,"nb"]
taux_emplois$situation = relevel(taux_emplois$situation, "En recherche d'emploi")
taux_emplois$situation = relevel(taux_emplois$situation, "Volontariat")
taux_emplois$situation = relevel(taux_emplois$situation, "En thèse")
taux_emplois$situation = relevel(taux_emplois$situation, "En création d'entreprise")
taux_emplois$situation = relevel(taux_emplois$situation, "En activité professionnelle")
taux_emplois$situation = relevel(taux_emplois$situation, "")
library(ggplot2)
p = ggplot(data=taux_emplois, aes(x=as.factor(promo), fill=situation, weight=poids), colour=black)  + geom_bar()
p + scale_fill_manual(values=c("blue","green","SlateBlue4","chartreuse4","red","yellow", "orange")) + opts(title="Situation des diplômés début 2O14, à 6 et 18 mois") + xlab("Promo Ensimag") + ylab("Pourcentage")
ggsave("../../Output/ensimag_2014_situation.svg")   
ggsave("../../Output/ensimag_2014_situation.png") 


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

## tempsrecherche = data.frame(situation=data2013$ActiviteActuelle, activitesOLD=data2013$ActiviteActuelleV2010, promo=data2013$PromoEnquete2013, duree=data2013$DureeRechercheEmploiINPG.2, poids=100, nb=1)
## length(tempsrecherche[tempsrecherche$situation == "En activité professionnelle" & tempsrecherche$promo == 2012 & tempsrecherche$duree > 0 & ! is.na(tempsrecherche$duree),"duree"])
## tempsrecherche$situation



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


# promo 2008-2012 a 18 mois

sit2008 = as.character(data2010_2008$situation.actuelle)
sit2008[data2010_2008$Type.d.études == "Doctorat"] = "En thèse"
sit2008[sit2008 == "A - Vous êtes en activité professionnelle"] = "En activité professionnelle"
sit2008[sit2008 == "C - Vous êtes en Volontariat (VIE ou VIA)"] = "Volontariat"
sit2008[sit2008 == "B - Vous êtes en recherche d'emploi"] = "En recherche d'emploi"
sit2008[sit2008 == "D - Vous êtes en poursuite d'études"] = "En poursuite d'études (Hors thèse)"
promo2008= sit2008
promo2008[]= "2008"

situationALL= factor( c(as.character(sit2008), as.character(data2011$ActiviteActuelle[data2011$Promo == 2009]) , as.character(data2012$ActiviteActuelle[data2012$AnneeDiplome == 2010]), as.character(data2013$ActiviteActuelle[data2013$AnneeDiplome == 2011]), as.character(data2014$ActiviteActuelle[data2014$AnneeDiplome == 2012]) ))
length(situationALL)
promoALL= factor(c( as.character(promo2008), as.character(data2011$Promo[data2011$Promo == 2009]) , as.character(data2012$AnneeDiplome[data2012$AnneeDiplome == 2010]), as.character(data2013$AnneeDiplome[data2013$AnneeDiplome == 2011]), as.character(data2014$AnneeDiplome[data2014$AnneeDiplome == 2012]) ))

situation2008_2012 = data.frame(situation=situationALL, promo=promoALL, poids=1, nb=1)
situation2008_2012 = situation2008_2012[situation2008_2012$situation != "",]
situation2008_2012 = situation2008_2012[! is.na(situation2008_2012$situation),]

situation2008_2012$situation = relevel(situation2008_2012$situation, "En recherche d'emploi")
situation2008_2012$situation = relevel(situation2008_2012$situation, "Volontariat")
situation2008_2012$situation = relevel(situation2008_2012$situation, "En thèse")
situation2008_2012$situation = relevel(situation2008_2012$situation, "En création d'entreprise")
situation2008_2012$situation = relevel(situation2008_2012$situation, "En activité professionnelle")

situation2008_2012[situation2008_2012$promo == 2008,"nb"] = length(situation2008_2012[situation2008_2012$promo == 2008,"situation"])
situation2008_2012[situation2008_2012$promo == 2009,"nb"] = length(situation2008_2012[situation2008_2012$promo == 2009,"situation"])
situation2008_2012[situation2008_2012$promo == 2010,"nb"] = length(situation2008_2012[situation2008_2012$promo == 2010,"situation"])
situation2008_2012[situation2008_2012$promo == 2011,"nb"] = length(situation2008_2012[situation2008_2012$promo == 2011,"situation"])
situation2008_2012[situation2008_2012$promo == 2012,"nb"] = length(situation2008_2012[situation2008_2012$promo == 2012,"situation"])
situation2008_2012[,"poids"] = 100/situation2008_2012[,"nb"]


ggplot(situation2008_2012, aes(x=promo,fill=situation, weight=poids)) + geom_bar() +scale_fill_manual(values=c("blue","green","SlateBlue4","chartreuse4","red","yellow", "orange")) + ylab("Pourcentage de réponses")
ggsave("../../Output/ensimag_2008_2012_situation.png", width=2*par("din")[1])


# promo 2009-2013 à 6 mois

sit2009 = as.character(data2010_2009$situation.actuelle)
sit2009[data2010_2009$Type.d.études == "Doctorat"] = "En thèse"
sit2009[sit2009 == "A - Vous êtes en activité professionnelle"] = "En activité professionnelle"
sit2009[sit2009 == "C - Vous êtes en Volontariat (VIE ou VIA)"] = "Volontariat"
sit2009[sit2009 == "B - Vous êtes en recherche d'emploi"] = "En recherche d'emploi"
sit2009[sit2009 == "D - Vous êtes en poursuite d'études"] = "En poursuite d'études (Hors thèse)"
sit2009[sit2009 == "E - Vous êtes sans activité professionnelle volontairement sans emploi (ex : année sabbatique...)"] = "Sans activité volontairement"
promo2009= sit2009
promo2009[]= "2009"

situationALL6= factor( gsub("Création d'entreprise (en projet)", "En création d'entreprise", fixed=T, c(as.character(sit2009), as.character(data2011$ActiviteActuelle[data2011$Promo == 2010]) , as.character(data2012$ActiviteActuelle[data2012$AnneeDiplome == 2011]), as.character(data2013$ActiviteActuelle[data2013$AnneeDiplome == 2012]), as.character(data2014$ActiviteActuelle[data2014$AnneeDiplome == 2013]) )))
length(situationALL6)
promoALL6= factor(c( as.character(promo2009), as.character(data2011$Promo[data2011$Promo == 2010]) , as.character(data2012$AnneeDiplome[data2012$AnneeDiplome == 2011]), as.character(data2013$AnneeDiplome[data2013$AnneeDiplome == 2012]), as.character(data2014$AnneeDiplome[data2014$AnneeDiplome == 2013]) ))

situation2009_2013 = data.frame(situation=situationALL6, promo=promoALL6, poids=1, nb=1)

situation2009_2013 = situation2009_2013[situation2009_2013$situation != "",]
situation2009_2013 = situation2009_2013[! is.na(situation2009_2013$situation),]

situation2009_2013$situation = relevel(situation2009_2013$situation, "En recherche d'emploi")
situation2009_2013$situation = relevel(situation2009_2013$situation, "Volontariat")
situation2009_2013$situation = relevel(situation2009_2013$situation, "En thèse")
situation2009_2013$situation = relevel(situation2009_2013$situation, "En création d'entreprise")
situation2009_2013$situation = relevel(situation2009_2013$situation, "En activité professionnelle")

situation2009_2013[situation2009_2013$promo == 2008,"nb"] = length(situation2009_2013[situation2009_2013$promo == 2008,"situation"])
situation2009_2013[situation2009_2013$promo == 2009,"nb"] = length(situation2009_2013[situation2009_2013$promo == 2009,"situation"])
situation2009_2013[situation2009_2013$promo == 2010,"nb"] = length(situation2009_2013[situation2009_2013$promo == 2010,"situation"])
situation2009_2013[situation2009_2013$promo == 2011,"nb"] = length(situation2009_2013[situation2009_2013$promo == 2011,"situation"])
situation2009_2013[situation2009_2013$promo == 2012,"nb"] = length(situation2009_2013[situation2009_2013$promo == 2012,"situation"])
situation2009_2013[situation2009_2013$promo == 2013,"nb"] = length(situation2009_2013[situation2009_2013$promo == 2013,"situation"])
situation2009_2013[,"poids"] = 100/situation2009_2013[,"nb"]

length(situation2009_2013$situation)
summary(situation2009_2013$situation)
length(situation2008_2012$situation)
summary(situation2008_2012$situation)

ggplot(situation2009_2013, aes(x=promo,fill=situation, weight=poids)) + geom_bar() +scale_fill_manual(values=c("blue","green","SlateBlue4","chartreuse4","red","yellow", "orange")) + ylab("Pourcentage de réponses")
ggsave("../../Output/ensimag_2009_2013_situation.png", width=2*par("din")[1])


summary(situation2009_2013)
dim(situation2009_2013)
head(situation2009_2013)
library("dplyr")
select(situation2009_2013, situation, promo, poids) %>% group_by(promo, situation) %>% summarize(nombre = round(sum(poids),1))

summary(situation2008_2012)
dim(situation2008_2012)
head(situation2008_2012)
select(situation2008_2012, situation, promo, poids) %>% group_by(promo, situation) %>% summarize(nombre = round(sum(poids),1))

