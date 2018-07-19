## satisfaction sur la formation et la filière

source("../DataReader/dataReader.R")

library(ggplot2)
library(dplyr)
                                        #2018
a = data.frame(Emploi=data2018$X86..EmploiSatisfaction, Conditions=data2018$X87..EmploiSatisfactionConditions, Collegues=data2018$X88..EmploiSatisfactionCollegues, Remuneration=data2018$X89..EmploiSatisfactionRemuneration, Autonomie=data2018$X90..EmploiSatisfactionAutonomie, Collegues=data2018$X91..EmploiSatisfactionLocalisation, Formation=data2018$X141..EcoleSatisfactionFormation, noteFormation=NA, Filiere=data2018$X258..Option_ScolariteFiliereFormation)

levels(a$Filiere) = c("NA", "IF" , "ISI" , "ISSC" , "Master" , "Master" , "Master" , "MMIS" , "SLE")
a$Filiere = factor(a$Filiere)

a$noteFormation[a$Formation == "Très satisfait(e)"] = 20
a$noteFormation[a$Formation == "Satisfait(e)"] = 15
a$noteFormation[a$Formation == "Ni satisfait(e) ni insatisfait(e)"] = 10
a$noteFormation[a$Formation == "Insatisfait(e)"] = 5
a$noteFormation[a$Formation == "Très insatisfait(e)"] = 0

b = select(a, Filiere) %>% group_by(Filiere) %>% summarize(n())
b
write.csv(file="rep18.csv", b)
b= select(a, Emploi, Filiere) %>% group_by(Filiere, Emploi) %>% summarize(n())
b
write.csv(file="emploi18.csv", b)
b= select(a, Formation, Filiere) %>% group_by(Filiere, Formation) %>% summarize(n())
b
write.csv(file="formation18.csv", b)
b= select(a, noteFormation, Filiere) %>% group_by(Filiere) %>% summarize(mean(noteFormation, na.rm = T))
b
write.csv(file="noteformation18.csv", b)

ggplot(a,aes(Formation, fill=Filiere)) + geom_bar(colour="white") + coord_flip() + facet_wrap(c("Filiere")) + ggtitle("Satisfaction des diplômés de la formation")
ggsave("../../Output/ensimag_2018_satisfaction_filière.png", width=1.5*par("din")[1])


                                        # 2017
a = data.frame(Emploi=data2017$X87..EmploiSatisfaction, Formation=data2017$X142..EcoleSatisfactionFormation, filiere=data2017$X247..Option_ScolariteFiliereFormation)

aE = data.frame(type="Emploi", val=data2017$X87..EmploiSatisfaction, filiere=data2017$X247..Option_ScolariteFiliereFormation)
aF = data.frame(type="Formation", val=data2017$X142..EcoleSatisfactionFormation, filiere=data2017$X247..Option_ScolariteFiliereFormation)
aCT = data.frame(type="Condition de Travail", val=data2017$X88..Option_EmploiSatisfactionConditions, filiere=data2017$X247..Option_ScolariteFiliereFormation)
aRC = data.frame(type="Relation Collegues", val=data2017$X89..Option_EmploiSatisfactionCollegues, filiere=data2017$X247..Option_ScolariteFiliereFormation)
aR = data.frame(type="Remunération", val=data2017$X90..Option_EmploiSatisfactionRemuneration, filiere=data2017$X247..Option_ScolariteFiliereFormation)
aL = data.frame(type="Localisation", val=data2017$X92..Option_EmploiSatisfactionLocalisation, filiere=data2017$X247..Option_ScolariteFiliereFormation)


b = rbind(aE,aF, aCT,aRC, aR, aL)
str(b)
levels(b$filiere)
levels(b$filiere) = c("Non renseigné" , "Master" , "Master" , "IF" , "ISI" , "ISSC" , "Master" , "Master" , "Master" , "Master" , "MMIS" , "SLE")
levels(b$val) = c("", "Ni satisfait ni insatisfait" , "Satisfait"     , "Insatisfait" , "Très satisfait", "Très insatisfait" , "Ni satisfait ni insatisfait" , "Satisfait" , "Très satisfait", "Très insatisfait" , "Insatisfait"   , "Ni satisfait ni insatisfait" , "Satisfait", "Très insatisfait" , "Très satisfait")
b$val = factor(b$val)
b$val = relevel(b$val, "Très satisfait")
b$val = relevel(b$val, "Satisfait")
b$val = relevel(b$val, "Insatisfait")
b$val = relevel(b$val, "Ni satisfait ni insatisfait")
b$val = relevel(b$val, "Insatisfait")
b$val = relevel(b$val, "Très insatisfait")
b$val = relevel(b$val, "")


ggplot(b,aes(val, fill=filiere)) + geom_bar(colour="white") + coord_flip() + facet_wrap(c("type")) + ggtitle("Satisfaction des diplômés")
ggsave("../../Output/ensimag_2017_satisfaction.png", width=1.5*par("din")[1])

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



















