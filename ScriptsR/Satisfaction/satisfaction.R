## satisfaction sur la formation et la filière

source("../DataReader/dataReader.R")

library(ggplot2)

                                        # 2016
a = data.frame(Emploi=data2016$SatisfactionEmploi, Formation=data2016$SatisfactionFormation, filiere=data2016$Option_FiliereFormation)

aE = data.frame(type="Emploi", val=data2016$SatisfactionEmploi, filiere=data2016$Option_FiliereFormation)
aF = data.frame(type="Formation", val=data2016$SatisfactionFormation, filiere=data2016$Option_FiliereFormation)
aCT = data.frame(type="Condition de Travail", val=data2016$SatisfactionConditionsTravail, filiere=data2016$Option_FiliereFormation)
aRC = data.frame(type="Relation Collegues", val=data2016$SatisfactionRelationCollegues, filiere=data2016$Option_FiliereFormation)
aR = data.frame(type="Remunération", val=data2016$SatisfactionRemuneration, filiere=data2016$Option_FiliereFormation)
aL = data.frame(type="Localisation", val=data2016$SatisfactionLocalisationEntreprise, filiere=data2016$Option_FiliereFormation)


b = rbind(aE,aF, aCT,aRC, aR, aL)
str(b)
levels(b$filiere) = c( "Master" , "Master" , "IF" , "ISI" , "Tcom/ISSC" , "Master" , "Master" , "Master" , "MMIS" , "SLE" , "Tcom/ISSC")

ggplot(b,aes(val, fill=filiere)) + geom_bar(colour="white") + facet_wrap(c("type"))
ggsave("../../Output/ensimag_2016_satisfaction.png", width=1.5*par("din")[1])

## qplot(data2016$SatisfactionFormation,data2016$SatisfactionEmploi) + geom_jitter(width=0.6,height=0.6)
## qplot(data2016$SatisfactionConditionsTravail)
## qplot(data2016$SatisfactionSatisfactionRelationCollegues)
## qplot(data2016$SatisfactionRemuneration)
## qplot(data2016$SatisfactionLocalisationEntreprise)
## qplot(data2016$SatisfactionRemuneration, data2016$SatisfactionLocalisationEntreprise) + geom_jitter(width=0.6,height=0.6)
## qplot(data2016$SatisfactionAutonomie)
## qplot(data2016$Ecole_EmploiCorrespondFormation)



data2013$RessentiFormationINPG = relevel(data2013$RessentiFormationINPG, "Très satisfaisante")
data2013$RessentiFormationINPG = relevel(data2013$RessentiFormationINPG, "Satisfaisante")
data2013$RessentiFormationINPG = relevel(data2013$RessentiFormationINPG, "")
data2013$RessentiFormationINPG = relevel(data2013$RessentiFormationINPG, "Insatisfaisante")
data2013$RessentiFormationINPG = relevel(data2013$RessentiFormationINPG, "Très insatisfaisante")

p = ggplot(data2013, aes(x= SatisfactionEmploiActuel, y=RessentiFormationINPG, color=FiliereFormation)) + geom_point() + geom_jitter(width=.2,height=0.25) + scale_color_brewer(palette="Set1") + labs("Qualité de l'emploi (de 1 à 5) versus le ressenti de la formation") + xlab("Qualité de l'emploi (de 1 à 5)") + ylab("Ressenti de la formation")
p
ggsave("../../Output/satisfaction.svg", width=2*par("din")[1])

r14 = data.frame(Ressenti_formation=data2013$RessentiFormationINPG, enquete=2014)
r13 = data.frame(Ressenti_formation=data2013$RessentiFormationINPG, enquete=2013)
r12 = data.frame(Ressenti_formation=data2012$RessentiFormationINPG, enquete=2012)
r11 = data.frame(Ressenti_formation=data2011$RessentiFormationINPG, enquete=2011)
r= rbind(r14, r13, r12, r11)
r = r[r$Ressenti_formation != "",]
r$enquete = factor(r$enquete)

r$Ressenti_formation = relevel(r$Ressenti_formation, "Très insatisfaisante")
r$Ressenti_formation = relevel(r$Ressenti_formation, "Insatisfaisante")
r$Ressenti_formation = relevel(r$Ressenti_formation, "Satisfaisante")
r$Ressenti_formation = relevel(r$Ressenti_formation, "Très satisfaisante")

ggplot(r, aes(x=enquete, fill=Ressenti_formation)) + geom_bar() + ylab("Nombre de réponses") + xlab("Année d'enquête")
ggsave("../../Output/ensimag_satisfaction.png", width=2*par("din")[1])



















