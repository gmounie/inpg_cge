## competences

## difficile à représenter ?
## juste citer les mots clefs présentés avec des histogrammes ?

source("../DataReader/dataReader.R")

library(stringr)
library(plyr)
library(ggplot2)

# 2017
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
levels(c) = c("23 Autres" , "23 Autres" , "23 Autres" , "23 Autres" , "23 Autres" , "Economy - Finance" , "23 Autres" , "23 Autres" , "23 Autres" , "23 Autres" , "Financial engineering" , "Information systems, management information systems, databases" , "23 Autres" , "23 Autres" , "23 Autres" , "23 Autres" , "Mathematics, modeling, simulation" , "23 Autres" , "Modeling - scientific computing" , "Multimedia, graphics, audiovisual software" , "23 Autres" , "23 Autres" , "23 Autres" , "23 Autres" , "23 Autres" , "23 Autres" , "23 Autres" , "Real time or embedded software, industrial IT" , "23 Autres" , "Software, IT, networks" , "Telecommunication systems" , "23 Autres")

levels(c) = c("24 Autres" , "24 Autres" , "24 Autres" , "24 Autres" , "24 Autres" , "24 Autres" , "Economie - Finance" , "24 Autres" , "24 Autres" , "24 Autres" , "24 Autres" , "24 Autres" , "24 Autres" , "Gestion - Organisation" , "24 Autres" , "Ingénierie financière" , "24 Autres" , "Logiciels multimédia, images, audiovisuel" , "Logiciels, systèmes informatiques, réseaux" , "Logiciels temps réel, embarqué, informatique industrielle" , "24 Autres" , "24 Autres" , "Mathématiques, modélisation, simulation" , "24 Autres" , "24 Autres" , "24 Autres" , "Modélisation - Calcul scientifique" , "24 Autres" , "24 Autres" , "24 Autres" , "24 Autres" , "Systèmes de télécommunications" , "Systèmes d'information, informatique de gestion, BD" , "24 Autres" , "Traitement du signal et des images")

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















