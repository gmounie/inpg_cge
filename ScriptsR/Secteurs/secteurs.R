source("../DataReader/dataReader.R")

library(ggplot2)
library(plyr)
library(dplyr)
# version 2018 ISI
a = data.frame(secteurs = data2018$X55..EmploiEntrepriseSecteurActivite, poids= 1, situation=data2018$X20..ActiviteActuelle, promo=data2018$X14..AnneeDiplomeVerifieParLeDiplome, filiere=data2018$X258..Option_ScolariteFiliereFormation)
isipro = a[a$filiere == "ISI – ingénierie des systèmes d’information" & a$situation == "En activité professionnelle",]
totalpro = sum(isipro$poids)
ddply(isipro, .(secteurs), summarize, nb=round(100*sum(poids)/totalpro, digits=1))

isi = a[a$filiere == "ISI – ingénierie des systèmes d’information",]
total = sum(isi$poids)
ddply(isi, .(situation), summarize, nb=round(100*sum(poids)/total, digits=1))



                                        # version 2017
a = data.frame(secteurs = data2017$X58..EmploiEntrepriseSecteurActivite, poids= 1, situation=data2017$X26..ActiviteActuelle, promo=data2017$X21..AnneeDiplomeVerifieParLeDiplome, filiere=data2017$X247..Option_ScolariteFiliereFormation)
# pour avoir juste la promo 2016
                                        # a=a[a$promo == 2016,]

a$secteurs = factor(a$secteurs, levels=c(levels(a$secteurs), "PhD", "Voluntary work"))
a[a$situation == "Studying for a PhD",]$secteurs = "PhD"
a[a$situation == "Voluntary work",]$secteurs = "Voluntary work"
levels(a$situation)
levels(a$secteurs)

levels(a$filiere) = c("Non renseigné" , "Master" , "Master" , "IF" , "ISI" , "ISSC" , "Master" , "Master" , "Master" , "Master" , "MMIS" , "SLE")
a$filiere = factor(a$filiere)
a$promo = factor(a$promo)


levels(a$secteurs) = c("" , "15 Autres" , "15 Autres" , "15 Autres" , "15 Autres" , "15 Autres" , "Financial and insurance activities" , "15 Autres" , "IT and other information services" , "IT industries" , "15 Autres" , "15 Autres" , "15 Autres" , "15 Autres" , "15 Autres" , "15 Autres" , "15 Autres" , "15 Autres" , "Scientific research and development" , "Telecommunications" , "15 Autres" , "15 Autres" , "PhD" , "Voluntary work")
a$secteurs = factor(a$secteurs)              


a = a[a$filiere != "",]
a = a[a$filiere != "Non renseigné",]
a$filiere = factor(a$filiere)

#a = a[as.character(a$secteurs) != "",]
# total= length(a$secteurs[a$secteurs != ""])
total = length(a$secteurs)
ddply(a, .(secteurs), summarize, nb=round(100*sum(poids)/total, digits=1))

b = a[a$apprentissage == "En apprentissage",] 
totala = length(b$secteurs[b$secteurs != ""])
ddply(b, .(secteurs), summarize, nb=round(100*sum(poids)/totala, digits=0)) 

val = count(data2017)
val2 = val
val2$freq = val$freq / sum(val$freq)
val3 = val2
val3$agglosect = val3$SecteurActivite
#for(i in 1:length(val3$freq)) { if (val3$freq[i] < 0.1) val3$agglosect[i] = "Autres secteurs" }



#p = ggplot(val3, aes(x=factor(agglosect), weight=freq)) + geom_bar(fill="lightgreen", colour="darkgreen") + coord_flip() + opts(title="Secteurs d'activité") + xlab("") + ylab("Pourcentage") 


p = ggplot(a, aes(x=factor(secteurs), weight=poids/(length(a$secteurs)),fill=filiere)) + geom_bar(colour="white") + coord_flip() + ggtitle("Secteurs d'activité en 2017, par filière, 6, 18 et 30 mois après la sortie") + xlab("") + ylab("Pourcentage")


#p + geom_text(x=1, y=0.11, label="secteurs < 10%", size=16) + opts(plot.title = theme_text(size=32, lineheight=.8, face="bold"), axis.text.x = theme_text(size=28, lineheight=.8, face="bold"), axis.text.y = theme_text(size=28, lineheight=.8, face="bold"),  axis.title.x = theme_text(size=28, lineheight=.8)) 

p
#p + geom_text(x=1, y=0.11, label="15 secteurs < 10%")
ggsave("../../Output/ensimag_2017_secteurs_filiere.png", width=1.5*par("din")[1])

p = ggplot(a, aes(x=factor(filiere), weight=poids/(length(a$filiere)),fill=secteurs)) + geom_bar(colour="white") + coord_flip() + ggtitle("Filière en 2017, par secteur d'activité, 6, 18 et 30 mois après la sortie") + xlab("") + ylab("Pourcentage")


#p + geom_text(x=1, y=0.11, label="secteurs < 10%", size=16) + opts(plot.title = theme_text(size=32, lineheight=.8, face="bold"), axis.text.x = theme_text(size=28, lineheight=.8, face="bold"), axis.text.y = theme_text(size=28, lineheight=.8, face="bold"),  axis.title.x = theme_text(size=28, lineheight=.8)) 

p
#p + geom_text(x=1, y=0.11, label="15 secteurs < 10%")
ggsave("../../Output/ensimag_2017_filiere_secteurs.png", width=1.5*par("din")[1])


p = ggplot(a, aes(x=factor(filiere), weight=poids/(length(a$filiere)),fill=situation)) + geom_bar(colour="white") + coord_flip() + ggtitle("Filière en 2017, par situation, 6, 18 et 30 mois après la sortie") + xlab("") + ylab("Pourcentage")


#p + geom_text(x=1, y=0.11, label="secteurs < 10%", size=16) + opts(plot.title = theme_text(size=32, lineheight=.8, face="bold"), axis.text.x = theme_text(size=28, lineheight=.8, face="bold"), axis.text.y = theme_text(size=28, lineheight=.8, face="bold"),  axis.title.x = theme_text(size=28, lineheight=.8)) 

p
#p + geom_text(x=1, y=0.11, label="15 secteurs < 10%")
ggsave("../../Output/ensimag_2017_filiere_situation.png", width=1.5*par("din")[1])



# version 2016
secteurs = data2016$SecteurActiviteFinale[data2016$AnneeEnquete == 2016]
a = data.frame(secteurs = data2016$SecteurActivite, poids= 1, situation=data2016$ActiviteActuelleV2016, filiere=data2016$Option_FiliereFormation, promo=data2016$Promo)
# pour avoir juste la promo 2015
a=a[a$promo == 2015,]
a$secteurs = factor(a$secteurs, levels=c(levels(a$secteurs), "Recherche (doctorat)", "Volontariat"))
a[a$situation == "En thèse",]$secteurs = "Recherche (doctorat)"
a[a$situation == "En volontariat (VIE, VIA, Volontariat civil)",]$secteurs = "Volontariat"
levels(a$situation)
levels(a$secteurs)
levels(a$filiere) = c( "Master" , "Master" , "IF" , "ISI" , "Tcom/ISSC" , "Master" , "Master" , "Master" , "MMIS" , "SLE" , "Tcom/ISSC")


levels(a$secteurs) = c("" , "Activités financières et d'assurance" , "Activités informatiques et services d'information (TIC Services)" , "15 Autres" , "15 Autres" , "15 Autres" , "15 Autres" , "Autres activités spécialisées, scientifiques et techniques" , "Autres secteurs" , "15 Autres" , "15 Autres" , "15 Autres" , "Énergie" , "Enseignement, recherche" , "15 Autres" , "Industrie automobile, aéronautique, navale, ferroviaire" , "15 Autres" , "Industrie des TIC" , "15 Autres" , "15 Autres" , "Recherche-développement scientifique" , "15 Autres" , "Sociétés de conseil, Bureaux d'études, Ingénierie" , "Télécommunications" , "15 Autres" , "15 Autres" , "Recherche (doctorat)" , "Volontariat") 
#levels(a$secteurs) = c("", "Activités financières et d'assurance", "Activités informatiques et services d'information", "Arts, spectacles et activités récréatives", "Autres activités spécialisées, scientifiques et techniques", "Autres industries", "Autres secteurs", "Commerce", "Édition, audiovisuel et diffusion", "Énergie", "Enseignement, recherche", "Industrie automobile, aéronautique, navale, ferroviaire", "Métallurgie", "Recherche-développement scientifique", "Santé humaine et action sociale", "Sociétés de conseil, bureaux d'études", "Télécommunications", "Tourisme", "Transports (Services)", "Recherche (doctorat)", "Volontariat")


a = a[a$filiere != "",]
a = a[as.character(a$secteurs) != "",]


total= length(a$secteurs[a$secteurs != ""])
ddply(a, .(secteurs), summarize, nb=round(100*sum(poids)/total, digits=1))

b = a[a$apprentissage == "En apprentissage",] 
totala = length(b$secteurs[b$secteurs != ""])
ddply(b, .(secteurs), summarize, nb=round(100*sum(poids)/totala, digits=0)) 

val = count(data2016)
val2 = val
val2$freq = val$freq / sum(val$freq)
val3 = val2
val3$agglosect = val3$SecteurActivite
#for(i in 1:length(val3$freq)) { if (val3$freq[i] < 0.1) val3$agglosect[i] = "Autres secteurs" }



#p = ggplot(val3, aes(x=factor(agglosect), weight=freq)) + geom_bar(fill="lightgreen", colour="darkgreen") + coord_flip() + opts(title="Secteurs d'activité") + xlab("") + ylab("Pourcentage") 


p = ggplot(a, aes(x=factor(secteurs), weight=poids/(length(a$secteurs)),fill=filiere)) + geom_bar(colour="white") + coord_flip() + ggtitle("Secteurs d'activité en 2016, par filière, 6, 18 et 30 mois après la sortie") + xlab("") + ylab("Pourcentage") + geom_text(label="FILIÈRE FINANCE sous représenté !!!", color="red", x=1, y=0.25)


#p + geom_text(x=1, y=0.11, label="secteurs < 10%", size=16) + opts(plot.title = theme_text(size=32, lineheight=.8, face="bold"), axis.text.x = theme_text(size=28, lineheight=.8, face="bold"), axis.text.y = theme_text(size=28, lineheight=.8, face="bold"),  axis.title.x = theme_text(size=28, lineheight=.8)) 

p
#p + geom_text(x=1, y=0.11, label="15 secteurs < 10%")
ggsave("../../Output/ensimag_2016_secteurs_filiere.png", width=0.8*par("din")[1])

# version 2015
secteurs = data2015$SecteurActiviteFinale[data2015$AnneeEnquete == 2015]
a = data.frame(secteurs = data2015$SecteurActivite, poids= 1, situation=data2015$ActiviteActuelleV2015, filiere=data2015$Option_FiliereFormation)
a$secteurs = factor(a$secteurs, levels=c(levels(a$secteurs), "Recherche (doctorat)", "Volontariat"))
a[a$situation == "En thèse",]$secteurs = "Recherche (doctorat)"
a[a$situation == "En volontariat",]$secteurs = "Volontariat"


levels(a$secteurs) = c("", "Activités financières et d'assurance", "Activités informatiques et services d'information", "Arts, spectacles et activités récréatives", "Autres activités spécialisées, scientifiques et techniques", "Autres industries", "Autres secteurs", "Commerce", "Édition, audiovisuel et diffusion", "Énergie", "Enseignement, recherche", "Industrie automobile, aéronautique, navale, ferroviaire", "Métallurgie", "Recherche-développement scientifique", "Santé humaine et action sociale", "Sociétés de conseil, bureaux d'études", "Télécommunications", "Tourisme", "Transports (Services)", "Recherche (doctorat)", "Volontariat")


a = a[a$filiere != "",]
a = a[as.character(a$secteurs) != "",]


total= length(a$secteurs[a$secteurs != ""])
ddply(a, .(secteurs), summarize, nb=round(100*sum(poids)/total, digits=1))

b = a[a$apprentissage == "En apprentissage",] 
totala = length(b$secteurs[b$secteurs != ""])
ddply(b, .(secteurs), summarize, nb=round(100*sum(poids)/totala, digits=0)) 

val = count(data2015)
val2 = val
val2$freq = val$freq / sum(val$freq)
val3 = val2
val3$agglosect = val3$SecteurActivite
#for(i in 1:length(val3$freq)) { if (val3$freq[i] < 0.1) val3$agglosect[i] = "Autres secteurs" }



#p = ggplot(val3, aes(x=factor(agglosect), weight=freq)) + geom_bar(fill="lightgreen", colour="darkgreen") + coord_flip() + opts(title="Secteurs d'activité") + xlab("") + ylab("Pourcentage") 


p = ggplot(a, aes(x=factor(secteurs), weight=poids/(length(a$secteurs)),fill=filiere)) + geom_bar(colour="white",guide=FALSE) + coord_flip() + ggtitle("Secteurs d'activité en 2015, par filière, 6, 18 et 30 mois après la sortie") + xlab("") + ylab("Pourcentage") + geom_text(label="FILIÈRE FINANCE décalé de 6 MOIS (sous-représenté: 2/3)!!!", color="red", x=10, y=0.2)


#p + geom_text(x=1, y=0.11, label="secteurs < 10%", size=16) + opts(plot.title = theme_text(size=32, lineheight=.8, face="bold"), axis.text.x = theme_text(size=28, lineheight=.8, face="bold"), axis.text.y = theme_text(size=28, lineheight=.8, face="bold"),  axis.title.x = theme_text(size=28, lineheight=.8)) 

p
#p + geom_text(x=1, y=0.11, label="15 secteurs < 10%")
ggsave("../../Output/ensimag_2015_secteurs_filiere.png", width=1.2*par("din")[1])



# version 2014
secteurs = data2014$SecteurActiviteFinale[data2014$AnneeEnquete == 2014]
a = data.frame(secteurs = data2014$SecteurActivite, poids= 1, situation=data2014$ActiviteActuelle, situationVolontariat = data2014$SecteurActiviteEntrepriseVolontariat, apprentissage= data2014$ApprentissageFormationContinueV2013, filiere=data2014$FiliereFormation)
a$secteurs = factor(a$secteurs, levels=c(levels(a$secteurs), "Recherche (doctorat)"))
a[a$situation == "En thèse",]$secteurs = "Recherche (doctorat)"
a[a$situation == "Volontariat",]$secteurs = a[a$situation == "Volontariat",]$situationVolontariat

a = a[a$filiere != "",]
a = a[as.character(a$secteurs) != "",]


total= length(a$secteurs[a$secteurs != ""])
ddply(a, .(secteurs), summarize, nb=round(100*sum(poids)/total, digits=1))

b = a[a$apprentissage == "En apprentissage",] 
totala = length(b$secteurs[b$secteurs != ""])
ddply(b, .(secteurs), summarize, nb=round(100*sum(poids)/totala, digits=0)) 

val = count(data2014)
val2 = val
val2$freq = val$freq / sum(val$freq)
val3 = val2
val3$agglosect = val3$SecteurActivite
#for(i in 1:length(val3$freq)) { if (val3$freq[i] < 0.1) val3$agglosect[i] = "Autres secteurs" }



#p = ggplot(val3, aes(x=factor(agglosect), weight=freq)) + geom_bar(fill="lightgreen", colour="darkgreen") + coord_flip() + opts(title="Secteurs d'activité") + xlab("") + ylab("Pourcentage") 


p = ggplot(a, aes(x=factor(secteurs), weight=poids/(length(a$secteurs)),fill=filiere)) + geom_bar(colour="white") + coord_flip() + ggtitle("Secteurs d'activité en 2014, par filière, 6 et 18 mois après la sortie") + xlab("") + ylab("Pourcentage") + geom_text(label="PAS DE FILIÈRE FINANCE À 6 MOIS !!!", color="red", x=10, y=0.2)


#p + geom_text(x=1, y=0.11, label="secteurs < 10%", size=16) + opts(plot.title = theme_text(size=32, lineheight=.8, face="bold"), axis.text.x = theme_text(size=28, lineheight=.8, face="bold"), axis.text.y = theme_text(size=28, lineheight=.8, face="bold"),  axis.title.x = theme_text(size=28, lineheight=.8)) 

p
#p + geom_text(x=1, y=0.11, label="15 secteurs < 10%")
ggsave("../../Output/ensimag_2014_secteurs_filiere.png", width=2*par("din")[1])



# version 2013

secteurs = data2013$SecteurActiviteFinale[data2013$AnneeEnquete == 2013]
a = data.frame(secteurs = data2013$SecteurActivite, poids= 1, situation=data2013$ActiviteActuelle, situationVolontariat = data2013$SecteurActiviteEntrepriseVolontariat, apprentissage= data2013$ApprentissageFormationContinueV2013, filiere=data2013$FiliereFormation)
a$secteurs = factor(a$secteurs, levels=c(levels(a$secteurs), "Recherche (doctorat)"))
a[a$situation == "En thèse",]$secteurs = "Recherche (doctorat)"
a[a$situation == "Volontariat",]$secteurs = a[a$situation == "Volontariat",]$situationVolontariat

a = a[a$filiere != "",]
a = a[as.character(a$secteurs) != "",]


length(a$secteurs[a$secteurs != ""])
ddply(a, .(secteurs), summarize, nb=round(100*sum(poids)/274, digits=1))

b = a[a$apprentissage == "En apprentissage",] 
length(b$secteurs[b$secteurs != ""])
ddply(b, .(secteurs), summarize, nb=round(100*sum(poids)/9, digits=0)) 

val = count(data2013)
val2 = val
val2$freq = val$freq / sum(val$freq)
val3 = val2
val3$agglosect = val3$SecteurActivite
#for(i in 1:length(val3$freq)) { if (val3$freq[i] < 0.1) val3$agglosect[i] = "Autres secteurs" }



#p = ggplot(val3, aes(x=factor(agglosect), weight=freq)) + geom_bar(fill="lightgreen", colour="darkgreen") + coord_flip() + opts(title="Secteurs d'activité") + xlab("") + ylab("Pourcentage") 



p = ggplot(a, aes(x=factor(secteurs), weight=poids/(length(a$secteurs)),fill=filiere)) + geom_bar(colour="white") + coord_flip()  + xlab("") + ylab("Pourcentage") 

#p = ggplot(a, aes(x=factor(secteurs), weight=poids/(length(a$secteurs)),fill=filiere)) + geom_bar(colour="white") + coord_flip() + ggtitle("Secteurs d'activité en 2013, par filière, 6 et 12 mois") + xlab("") + ylab("Pourcentage") 



#p + geom_text(x=1, y=0.11, label="secteurs < 10%", size=16) + opts(plot.title = theme_text(size=32, lineheight=.8, face="bold"), axis.text.x = theme_text(size=28, lineheight=.8, face="bold"), axis.text.y = theme_text(size=28, lineheight=.8, face="bold"),  axis.title.x = theme_text(size=28, lineheight=.8)) 

p
#p + geom_text(x=1, y=0.11, label="15 secteurs < 10%")
ggsave("../../Output/ensimag_2013_secteurs_filiere.png", width=2*par("din")[1])

p = ggplot(a, aes(x=factor(filiere), weight=poids/(length(a$filiere)),fill=secteurs)) + geom_bar(colour="white") + coord_flip()  + xlab("") + ylab("Pourcentage") 

p
ggsave("../../Output/ensimag_2013_secteursfiliere.png", width=2*par("din")[1])

secteurs2008_20013_18mois = factor(c( sub("[0-9]+. *","",as.character(data2010_2008$emp.actuel.Secteur)), as.character(data2011$SecteurActiviteINPG[data2011$Promo == 2009]), as.character(data2012$SecteurActiviteINPG[data2012$AnneeDiplome == 2010]), as.character(data2013$SecteurActiviteFinale[data2013$AnneeEnquete == 2013 && data2013$AnneeDiplome == 2011]), as.character(data2014$SecteurActiviteFinale[data2014$AnneeEnquete == 2014 && data2014$AnneeDiplome == 2012]) ) )

secteurs2008_20013_18mois = factor(secteurs2008_20013_18mois[as.character(secteurs2008_20013_18mois) != ""]) 
length(secteurs2008_20013_18mois)
summary(secteurs2008_20013_18mois)

levels(secteurs2008_20013_18mois)
secteursAll = secteurs2008_20013_18mois[secteurs2008_20013_18mois != ""]

qplot(secteursAll) + coord_flip()
summary(secteursAll)
















