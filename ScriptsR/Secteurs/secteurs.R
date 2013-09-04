source("../DataReader/dataReader.R")
data2013 = data2013[data2013$AnneeEnquete == 2013,]

library(ggplot2)
library(plyr)

secteurs = data2013$SecteurActiviteFinale[data2013$AnneeEnquete == 2013]
a = data.frame(secteurs = data2013$SecteurActivite, poids= 1, situation=data2013$ActiviteActuelle, situationVolontariat = data2013$SecteurActiviteEntrepriseVolontariat, apprentissage= data2013$ApprentissageFormationContinueV2013)
a$secteurs = factor(a$secteurs, levels=c(levels(a$secteurs), "Recherche (doctorat)"))
a[a$situation == "En thèse",]$secteurs = "Recherche (doctorat)"
a[a$situation == "Volontariat",]$secteurs = a[a$situation == "Volontariat",]$situationVolontariat
length(a$secteurs[a$secteurs != ""])
ddply(a, .(secteurs), summarize, nb=round(100*sum(poids)/274, digits=1))

b = a[a$apprentissage == "En apprentissage",] 
length(b$secteurs[b$secteurs != ""])
ddply(b, .(secteurs), summarize, nb=round(100*sum(poids)/9, digits=0)) 

val = count(data2013)
val2 = val
val2$freq = val$freq / sum(val$freq)
val3 = val2
val3$agglosect = val3$SecteurActiviteINPG
for(i in 1:length(val3$freq)) { if (val3$freq[i] < 0.01) val3$agglosect[i] = "Autres secteurs" }



p = ggplot(val3, aes(x=factor(agglosect), weight=freq)) + geom_bar(fill="lightgreen", colour="darkgreen") + coord_flip() + opts(title="Secteurs d'activité") + xlab("") + ylab("Pourcentage") 
## png("secteurs2012.png", 1600, 1200)
p + geom_text(x=1, y=0.11, label="15 secteurs < 10%", size=16) + opts(plot.title = theme_text(size=32, lineheight=.8, face="bold"), axis.text.x = theme_text(size=28, lineheight=.8, face="bold"), axis.text.y = theme_text(size=28, lineheight=.8, face="bold"),  axis.title.x = theme_text(size=28, lineheight=.8)) 
## dev.off()
## svg("secteurs2012.svg")
p + geom_text(x=1, y=0.11, label="15 secteurs < 10%")
## dev.off()



