## competences

## difficile à représenter ?
## juste citer les mots clefs présentés avec des histogrammes ?

source("../DataReader/dataReader.R")

library(stringr)
library(plyr)
library(ggplot2)

## 2017 calculer le taux de présence de certaines compétences par promo
## envisagés: Info, Math, Finance, Autres, (Autres/Physique ?)

# Obtenir la liste des secteurs
a = str_split(data2017$X250..Ecole_ActivitesTechniquesFonction,";")
a = factor(unlist(a))
a = a[a != ""]
a = factor(unlist(a))
levels(a)

matchinfo = "Automation|Electronic networks|Embedded electronics|Information systems, management information systems, databases|Microtechnologies : microelectronics & microsystems|Multimedia, graphics, audiovisual software|Real time or embedded software, industrial IT|Signal and imaging processing|Software, IT, networks|Telecommunication systems"

matchmath = "Mathematics, modeling, simulation|Modeling - scientific computing|Multimedia, graphics, audiovisual software|Signal and imaging processing"

matchfi = "Economy - Finance|Financial engineering"

matchtout = "Aerospace|Automation|Aviation industry|Biology - Biotechnology|Civil & urban engineering|Economy - Finance|Electricity networks|Electromagnetism|Electronic networks|Embedded electronics|Financial engineering|Information systems, management information systems, databases|Instrumentation|Land transport|Management - Organization|Marketing - Business|Mathematics, modeling, simulation|Microtechnologies : microelectronics & microsystems|Modeling - scientific computing|Multimedia, graphics, audiovisual software|Optics, optoelectronics|Others|Power electronics|Power engineering|Printing|Process engineering|Quality - Safety|Real time or embedded software, industrial IT|Signal and imaging processing|Software, IT, networks|Telecommunication systems|Theoretical physics"

matchmultimed = "Multimedia, graphics, audiovisual software"
matchtelecom = "Telecommunication systems"
matchembed = "Embedded electronics|Real time or embedded software, industrial IT"
matchbd = "Information systems, management information systems, databases"

## Info, math, fi, tout
info = ! is.na(str_match(data2017$X250..Ecole_ActivitesTechniquesFonction,matchinfo))
math = ! is.na(str_match(data2017$X250..Ecole_ActivitesTechniquesFonction,matchmath))
fi = ! is.na(str_match(data2017$X250..Ecole_ActivitesTechniquesFonction,matchfi))

bd = ! is.na(str_match(data2017$X250..Ecole_ActivitesTechniquesFonction,matchbd))

multimed = ! is.na(str_match(data2017$X250..Ecole_ActivitesTechniquesFonction,matchmultimed))

telecom = ! is.na(str_match(data2017$X250..Ecole_ActivitesTechniquesFonction,matchtelecom))

embed = ! is.na(str_match(data2017$X250..Ecole_ActivitesTechniquesFonction,matchembed))

bd = ! is.na(str_match(data2017$X250..Ecole_ActivitesTechniquesFonction,matchtbd))

tout = ! is.na(str_match(data2017$X250..Ecole_ActivitesTechniquesFonction,matchtout))




compet17 = data.frame(situation=data2017$X26..ActiviteActuelle,
                      filiere=data2017$X247..Option_ScolariteFiliereFormation,
                      info=info, math=math, fi=fi, bd=bd, multimed=multimed,
                      embed=embed, telecom=telecom, tout=tout)

nb17 = sum(compet17$tout)
nbfi17 = sum(compet17$fi)

## All

cat("% of info:", sum(compet17$info)/nb17, "\n")
cat("% of math:", sum(compet17$math)/nb17, "\n")
cat("% of mathinfo:", sum(compet17$math & compet17$info)/nb17, "\n")
cat("% of math or info", sum(compet17$math | compet17$info)/nb17, "\n")
cat("% of ! math info:", sum(compet17$tout & !(compet17$math | compet17$info))/nb17, "\n")

cat("nbfi:", nbfi17, "\n")
cat("% of fi with info:", sum(compet17$fi & compet17$info)/nbfi17, "\n")
cat("% of fi with math:", sum(compet17$fi & compet17$math)/nbfi17, "\n")
cat("% of fi without math or info:", sum((compet17$fi & !(compet17$math | compet17$info))/nbfi17), "\n")


levels(compet17$filiere) = c("Non renseigné" , "Master" , "Master" , "IF" , "ISI" , "ISSC" , "Master" , "Master" , "Master" , "Master" , "MMIS" , "SLE")


## !IF doing FI
nb17finotif = sum(compet17$fi & compet17$filiere != "IF")
cat("% !IF Doing FI", nb17finotif, "\n")
cat("% of fi not IF info:", sum(compet17$info[compet17$fi & compet17$filiere != "IF"])/nb17finotif, "\n")
cat("% of Master math:", sum(compet17$math[compet17$fi & compet17$filiere != "IF"])/nb17finotif, "\n")
cat("% of Master math and info:", sum(compet17$math[compet17$fi & compet17$filiere != "IF"] & compet17$info[compet17$fi & compet17$filiere != "IF"])/nb17finotif, "\n")
cat("% of Master ! math or info:", sum(compet17$tout[compet17$fi & compet17$filiere != "IF"] & !(compet17$math[compet17$fi & compet17$filiere != "IF"] | compet17$info[compet17$fi & compet17$filiere != "IF"]))/nb17finotif, "\n")

## IF doing FI
nb17finotif = sum(compet17$fi & compet17$filiere == "IF")
cat("% IF Doing FI", nb17finotif, "\n")
cat("% of fi IF info:", sum(compet17$info[compet17$fi & compet17$filiere == "IF"])/nb17finotif, "\n")
cat("% of fi IF math:", sum(compet17$math[compet17$fi & compet17$filiere == "IF"])/nb17finotif, "\n")
cat("% of fi IF math and info:", sum(compet17$math[compet17$fi & compet17$filiere == "IF"] & compet17$info[compet17$fi & compet17$filiere == "IF"])/nb17finotif, "\n")
cat("% of fi IF! math or info:", sum(compet17$tout[compet17$fi & compet17$filiere == "IF"] & !(compet17$math[compet17$fi & compet17$filiere == "IF"] | compet17$info[compet17$fi & compet17$filiere == "IF"]))/nb17finotif, "\n")


## Master
nb17master = sum(compet17$tout & compet17$filiere == "Master")
cat("% Master", nb17master, "\n")
cat("% of Master info:", sum(compet17$info[compet17$filiere == "Master"])/nb17master, "\n")
cat("% of Master math:", sum(compet17$math[compet17$filiere == "Master"])/nb17master, "\n")
cat("% of Master math and info:", sum(compet17$math[compet17$filiere == "Master"] & compet17$info[compet17$filiere == "Master"])/nb17master, "\n")
cat("% of Master ! math or info:", sum(compet17$tout[compet17$filiere == "Master"] & !(compet17$math[compet17$filiere == "Master"] | compet17$info[compet17$filiere == "Master"]))/nb17master, "\n")

## IF
nb17if = sum(compet17$tout & compet17$filiere == "IF")
cat("% IF", nb17if, "\n")
cat("% of IF info:", sum(compet17$info[compet17$filiere == "IF"])/nb17if, "\n")
cat("% of IF math:", sum(compet17$math[compet17$filiere == "IF"])/nb17if, "\n")
cat("% of IF math and info:", sum(compet17$math[compet17$filiere == "IF"] & compet17$info[compet17$filiere == "IF"])/nb17if, "\n")
cat("% of IF ! math or info:", sum(compet17$tout[compet17$filiere == "IF"] & !(compet17$math[compet17$filiere == "IF"] | compet17$info[compet17$filiere == "IF"]))/nb17if, "\n")
cat("% of IF spe fi:", sum(compet17$fi[compet17$filiere == "IF"])/nb17if, "\n")


## ISI
nb17isi = sum(compet17$tout & compet17$filiere == "ISI")
cat("% ISI", nb17isi, "\n")
cat("% of ISI info:", sum(compet17$info[compet17$filiere == "ISI"])/nb17isi, "\n")
cat("% of ISI math:", sum(compet17$math[compet17$filiere == "ISI"])/nb17isi, "\n")
cat("% of ISI math and info:", sum(compet17$math[compet17$filiere == "ISI"] & compet17$info[compet17$filiere == "ISI"])/nb17isi, "\n")
cat("% of ISI ! math or info:", sum(compet17$tout[compet17$filiere == "ISI"] & !(compet17$math[compet17$filiere == "ISI"] | compet17$info[compet17$filiere == "ISI"]))/nb17isi, "\n")
cat("% of ISI spe bd:", sum(compet17$bd[compet17$filiere == "ISI"])/nb17isi, "\n")

## ISSC
nb17issc = sum(compet17$tout & compet17$filiere == "ISSC")
cat("% ISSC", nb17issc, "\n")
cat("% of ISSC info:", sum(compet17$info[compet17$filiere == "ISSC"])/nb17issc, "\n")
cat("% of ISSC math:", sum(compet17$math[compet17$filiere == "ISSC"])/nb17issc, "\n")
cat("% of ISSC math and info:", sum(compet17$math[compet17$filiere == "ISSC"] & compet17$info[compet17$filiere == "ISSC"])/nb17issc, "\n")
cat("% of ISSC ! math or info:", sum(compet17$tout[compet17$filiere == "ISSC"] & !(compet17$math[compet17$filiere == "ISSC"] | compet17$info[compet17$filiere == "ISSC"]))/nb17issc, "\n")
cat("% of ISSC spe telecom:", sum(compet17$telecom[compet17$filiere == "ISSC"])/nb17issc, "\n")


## MMIS
nb17mmis = sum(compet17$tout & compet17$filiere == "MMIS")
cat("% MMIS", nb17mmis, "\n")
cat("% of MMIS info:", sum(compet17$info[compet17$filiere == "MMIS"])/nb17mmis, "\n")
cat("% of MMIS math:", sum(compet17$math[compet17$filiere == "MMIS"])/nb17mmis, "\n")
cat("% of MMIS math and info:", sum(compet17$math[compet17$filiere == "MMIS"] & compet17$info[compet17$filiere == "MMIS"])/nb17mmis, "\n")
cat("% of MMIS ! math or info:", sum(compet17$tout[compet17$filiere == "MMIS"] & !(compet17$math[compet17$filiere == "MMIS"] | compet17$info[compet17$filiere == "MMIS"]))/nb17mmis, "\n")
cat("% of MMIS spe mmis:", sum(compet17$multimed[compet17$filiere == "MMIS"])/nb17mmis, "\n")

## SLE
nb17sle = sum(compet17$tout & compet17$filiere == "SLE")
cat("% SLE", nb17sle, "\n")
cat("% of SLE info:", sum(compet17$info[compet17$filiere == "SLE"])/nb17sle, "\n")
cat("% of SLE math:", sum(compet17$math[compet17$filiere == "SLE"])/nb17sle, "\n")
cat("% of SLE math and info:", sum(compet17$math[compet17$filiere == "SLE"] & compet17$info[compet17$filiere == "SLE"])/nb17sle, "\n")
cat("% of SLE ! math or info:", sum(compet17$tout[compet17$filiere == "SLE"] & !(compet17$math[compet17$filiere == "SLE"] | compet17$info[compet17$filiere == "SLE"]))/nb17sle, "\n")
cat("% of SLE spe embed:", sum(compet17$embed[compet17$filiere == "SLE"])/nb17sle, "\n")


## specialities for not doing info or math or fi
a = str_split(data2017$X250..Ecole_ActivitesTechniquesFonction,";")
a = a[compet17$tout & !(compet17$info | compet17$math | compet17$fi )]
a = factor(unlist(a))
a = a[a != ""]
a = factor(unlist(a))
levels(a)

## 2017 figure
a = str_split(data2017$X250..Ecole_ActivitesTechniquesFonction,";")
#situation=data2017$X26..ActiviteActuelle, promo=data2017$X21..AnneeDiplomeVerifieParLeDiplome,

filiere=data2017$X247..Option_ScolariteFiliereFormation
levels(filiere) = c("Non renseigné" , "Master" , "Master" , "IF" , "ISI" , "ISSC" , "Master" , "Master" , "Master" , "Master" , "MMIS" , "SLE")

a = a[a[] != ""]
filiere = filiere[data2017$X250..Ecole_ActivitesTechniquesFonction != ""]
taille=length(a[a[] != ""])
b = unlist(a)
c = as.factor(str_replace_all(b, "^ | $", ""))
i=1
filiereDUP=c()
for(line in a) { for(nb in seq(1, length(line))) { filiereDUP = c(filiereDUP, as.character(filiere[i]))}; i=i+1  }
str(filiereDUP)
levels(c)
## version courte
levels(c) = c("23 Autres" , "23 Autres" , "23 Autres" , "23 Autres" , "23 Autres" , "Economy - Finance" , "23 Autres" , "23 Autres" , "23 Autres" , "23 Autres" , "Financial engineering" , "Info Sys, management info. sys., DB" , "23 Autres" , "23 Autres" , "23 Autres" , "23 Autres" , "Math., modeling, simu." , "23 Autres" , "Modeling - Sci. Comput." , "Multimedia, graph., a-v software" , "23 Autres" , "23 Autres" , "23 Autres" , "23 Autres" , "23 Autres" , "23 Autres" , "23 Autres" , "RT or embedded S., industrial IT" , "23 Autres" , "Software, IT, networks" , "Telecommunication systems" , "23 Autres")

## version longue levels(c) = c("23 Autres" , "23 Autres" , "23 Autres" , "23 Autres" , "23 Autres" , "Economy - Finance" , "23 Autres" , "23 Autres" , "23 Autres" , "23 Autres" , "Financial engineering" , "Information systems, management information systems, databases" , "23 Autres" , "23 Autres" , "23 Autres" , "23 Autres" , "Mathematics, modeling, simulation" , "23 Autres" , "Modeling - scientific computing" , "Multimedia, graphics, audiovisual software" , "23 Autres" , "23 Autres" , "23 Autres" , "23 Autres" , "23 Autres" , "23 Autres" , "23 Autres" , "Real time or embedded software, industrial IT" , "23 Autres" , "Software, IT, networks" , "Telecommunication systems" , "23 Autres")

## FAUX

# levels(c) = c("Aéronautique" , "Aérospatiale" , "Automatique" , "Autres" , "Biologie - Biotechnologie" , "Conception microélectronique" , "Economie - Finance" , "Electronique de puissance" , "Electronique embarquée" , "Energétique" , "Génie civil et urbain" , "Génie des procédés" , "Génie mécanique" , "Gestion - organisation" , "Hydraulique" , "Ingénierie financière" , "Instrumentation" , "Logiciels multimédia, images, audiovisuel" , "Logiciels, systèmes informatiques, réseaux" , "Logiciels temps réel, embarqué, informatique industrielle" , "Marketing - Commerce" , "Matériaux" , "Mathématiques, modélisation, simulation" , "Mécanique des fluides industriels" , "Mécanique des matériaux" , "Microtechnologies : microélectronique et microsystèmes" , "Modélisation - Calcul scientifique" , "Physique générale" , "Productique génie industriel" , "Qualité - Sécurité" , "Réseaux électriques" , "Systèmes de télécommunications" , "Systèmes d'information, informatique de gestion, bases de données" , "Systèmes électroniques" , "Traitement du signal et des images")


data = data.frame(compétence=c, poids=1/taille, filiere=as.factor(filiereDUP))
data$filiere = relevel(data$filiere, "SLE")
data$filiere = relevel(data$filiere, "MMIS")
data$filiere = relevel(data$filiere, "ISSC")
data$filiere = relevel(data$filiere, "ISI")
data$filiere = relevel(data$filiere, "IF")
data$filiere = relevel(data$filiere, "Master")

p = ggplot(data, aes(x=compétence, weight=poids,fill=filiere))+ geom_bar(colour="white") + coord_flip() + theme(title=element_text("Compétences techniques")) + xlab("") + ylab("Pourcentage") 
p 
ggsave("../../Output/ensimag_2017_competence.png", width=1.3*par("din")[1])


# 2016
a = str_split(data2016$Ecole_ActivitesTechniquesFonction,";")
filiere = data2016$Option_FiliereFormation
levels(filiere) = c( "Master" , "Master" , "IF" , "ISI" , "Tcom/ISSC" , "Master" , "Master" , "Master" , "MMIS" , "SLE" , "Tcom/ISSC")
a = a[a[] != ""]
filiere = filiere[data2016$Ecole_ActivitesTechniquesFonction != ""]
taille=length(a[a[] != ""])
b = unlist(a)
c = as.factor(str_replace_all(b, "^ | $", ""))
i=1
filiereDUP=c()
for(line in a) { for(nb in seq(1, length(line))) { filiereDUP = c(filiereDUP, as.character(filiere[i]))}; i=i+1  }
str(filiereDUP)
levels(c) = c("24 Autres" , "24 Autres" , "24 Autres" , "24 Autres" , "24 Autres" , "24 Autres" , "Economie - Finance" , "24 Autres" , "24 Autres" , "24 Autres" , "24 Autres" , "24 Autres" , "24 Autres" , "Gestion - Organisation" , "24 Autres" , "Ingénierie financière" , "24 Autres" , "Logiciels multimédia, images, audiovisuel" , "Logiciels, systèmes informatiques, réseaux" , "Logiciels temps réel, embarqué, informatique industrielle" , "24 Autres" , "24 Autres" , "Mathématiques, modélisation, simulation" , "24 Autres" , "24 Autres" , "24 Autres" , "Modélisation - Calcul scientifique" , "24 Autres" , "24 Autres" , "24 Autres" , "24 Autres" , "Systèmes de télécommunications" , "Systèmes d'information, informatique de gestion, BD" , "24 Autres" , "Traitement du signal et des images")

# levels(c) = c("Aéronautique" , "Aérospatiale" , "Automatique" , "Autres" , "Biologie - Biotechnologie" , "Conception microélectronique" , "Economie - Finance" , "Electronique de puissance" , "Electronique embarquée" , "Energétique" , "Génie civil et urbain" , "Génie des procédés" , "Génie mécanique" , "Gestion - organisation" , "Hydraulique" , "Ingénierie financière" , "Instrumentation" , "Logiciels multimédia, images, audiovisuel" , "Logiciels, systèmes informatiques, réseaux" , "Logiciels temps réel, embarqué, informatique industrielle" , "Marketing - Commerce" , "Matériaux" , "Mathématiques, modélisation, simulation" , "Mécanique des fluides industriels" , "Mécanique des matériaux" , "Microtechnologies : microélectronique et microsystèmes" , "Modélisation - Calcul scientifique" , "Physique générale" , "Productique génie industriel" , "Qualité - Sécurité" , "Réseaux électriques" , "Systèmes de télécommunications" , "Systèmes d'information, informatique de gestion, bases de données" , "Systèmes électroniques" , "Traitement du signal et des images")


data = data.frame(compétence=c, poids=1/taille, filiere=as.factor(filiereDUP))
data$filiere = relevel(data$filiere, "SLE")
data$filiere = relevel(data$filiere, "MMIS")
data$filiere = relevel(data$filiere, "Tcom/ISSC")
data$filiere = relevel(data$filiere, "ISI")
data$filiere = relevel(data$filiere, "IF")
data$filiere = relevel(data$filiere, "Master")

p = ggplot(data, aes(x=compétence, weight=poids,fill=filiere))+ geom_bar(colour="white") + coord_flip() + theme(title=element_text("Compétences techniques")) + xlab("") + ylab("Pourcentage") 
p 
ggsave("../../Output/ensimag_2016_competence.png", width=0.8*par("din")[1])



# 2015
a = str_split(data2015$Ecole_ActivitesTechniquesFonction,";")
filiere = data2015$Option_FiliereFormation
a = a[a[] != ""]
filiere = filiere[data2015$Ecole_ActivitesTechniquesFonction != ""]
taille=length(a[a[] != ""])
b = unlist(a)
c = as.factor(str_replace_all(b, "^ | $", ""))
i=1
filiereDUP=c()
for(line in a) { for(nb in seq(1, length(line))) { filiereDUP = c(filiereDUP, as.character(filiere[i]))}; i=i+1  }
str(filiereDUP)
levels(c) = c(
"Autres" , "Autres" , "Autres" , "Autres" , "Autres" , "Autres" , "Economie - Finance" , "Electronique embarquée" , "Autres" , "Autres" , "Autres" , "Gestion - organisation" , "Autres" , "Ingénierie financière" , "Autres" , "Logiciels multimédia, images, audiovisuel" , "Logiciels, systèmes informatiques, réseaux" , "Logiciels temps réel, embarqué, informatique industrielle" , "Marketing - Commerce" , "Autres" , "Mathématiques, modélisation, simulation" , "Microtechnologies : microélectronique et microsystèmes" , "Modélisation - Calcul scientifique" , "Autres" , "Autres" , "Autres" , "Qualité - Sécurité" , "Systèmes de télécommunications" , "Systèmes d'information, informatique de gestion, bases de données" , "Systèmes électroniques" , "Traitement du signal et des images" , "Autres"           
    )
data = data.frame(compétence=c, poids=1/taille, filiere=as.factor(filiereDUP))
p = ggplot(data, aes(x=compétence, weight=poids,fill=filiere))+ geom_bar() + coord_flip() + theme(title=element_text("Compétences techniques")) + xlab("") + ylab("Pourcentage") 
p 
ggsave("../../Output/ensimag_2015_competence.png", width=1*par("din")[1])
## p = ggplot(data, aes(x=filiere, weight=poids,fill=compétence))+ geom_bar() + coord_flip() + theme(title=element_text("Compétences techniques")) + xlab("") + ylab("Pourcentage") 
## p 
## ggsave("../../Output/ensimag_2015_competence.png", width=3*par("din")[1])


# 2013
a = str_split(data2013$ActivitesTechniquesFonctionINPG,";")
filiere = data2013$FiliereFormation
a = a[a[] != ""]
filiere = filiere[data2013$ActivitesTechniquesFonctionINPG != ""]
taille=length(a[a[] != ""])
b = unlist(a)
c = as.factor(str_replace_all(b, "^ | $", ""))
i=1
filiereDUP=c()
for(line in a) { for(nb in seq(1, length(line))) { filiereDUP = c(filiereDUP, as.character(filiere[i]))}; i=i+1  }
str(filiereDUP)
data = data.frame(compétence=c, poids=1/taille, filiere=as.factor(filiereDUP))
p = ggplot(data, aes(x=compétence, weight=poids,fill=filiere))+ geom_bar() + coord_flip() + theme(title=element_text("Compétences techniques")) + xlab("") + ylab("Pourcentage") 
p 
ggsave("../../Output/ensimag_2013_competence.png", width=2*par("din")[1])

write.csv(data2013$ActivitesTechniquesFonctionINPG, file="compet13.csv", row.names= F, quote=F)
write.csv(data2014$ActivitesTechniquesFonctionINPG, file="compet14.csv", row.names= F, quote=F)















