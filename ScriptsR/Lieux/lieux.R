source("../DataReader/dataReader.R")

library(ggplot2)
library(dplyr)
                                        # 2018
                                        # très basique, juste RA, PACA et province

lieu = data.frame( lieutrav = data2018$X33..EmploiLieuRegionEtranger ,lieuvol = data2018$X100..VolontariatRegionEtranger, lieudoc = data2018$X122..TheseRegionEtranger)
lieu$lieu[lieu$lieutrav != ""] = as.character(lieu$lieutrav[lieu$lieutrav != ""])
lieu$lieu[lieu$lieudoc != ""] = as.character(lieu$lieudoc[lieu$lieudoc != ""])
lieu$lieu[lieu$lieuvol != ""] = as.character(lieu$lieuvol[lieu$lieuvol != ""])
lieu$lieu = factor(lieu$lieu)
a = data.frame(lieu = lieu$lieu, weight=1, promo=data2018$X14..AnneeDiplomeVerifieParLeDiplome, filiere=data2018$X258..Option_ScolariteFiliereFormation)

a = a %>% mutate(filiere= recode(filiere,                   
"IF – ingénierie pour la finance" = "IF",       
"ISI – ingénierie des systèmes d’information" = "ISI",
"ISSC –Internet, services et systèmes connectés" = "ISSC",
"Master Informatique" = "Master",
"Master Msiam" = "Master",
"Master SCCI (2017 ,2016)" = "Master",
"MMIS – modélisation mathématique, images, simulation" = "MMIS",
"SLE – Systèmes et logiciels embarqués" = "SLE")) %>%
    mutate(lieu, lieu=recode_factor(lieu,
                                    "Île-de-France"="IDF",
                                    "Auvergne-Rhône-Alpes"="ARA",
                                    "Étranger"="Etr",
                                    "Provence-Alpes-Côte d’Azur"="PACA",
                                    "Bourgogne-Franche-Comté"="Prov",   
                                    "Bretagne"="Prov",
                                    "Centre-Val de Loire"="Prov",       
                                    "Grand Est"="Prov",                 
                                    "Hauts-de-France"="Prov",
                                    "Nouvelle-Aquitaine"="Prov",
                                    "Occitanie"="Prov")) %>%
    filter(filiere != "")


a$promo = factor(a$promo)
a = a[! is.na(a$promo),]
a = a[! (a$filiere == "Non renseigné"),]
#a = a[! is.na(a$lieu),]
a$weight=100./length(a$promo)

a %>% group_by(lieu) %>% summarize(n())
a %>% summarize(n())


p = ggplot(data=a, aes(x=lieu, fill=filiere))  + geom_bar(colour="white",(aes(weight=weight))) + coord_flip() + facet_wrap(c("filiere"))
p = p  + theme(plot.title=element_text("Lieu de travail (doctorat, volontariat) en fonction de la filière")) + xlab("Lieu") + scale_fill_hue(l=70, c=150)  + ylab("% des diplômés")
p
ggsave("../../Output/ensimag_2018_lieu.png", width=2*par("din")[1])

                                        # 2017
                                        # très basique, juste RA, PACA et province

lieu = data.frame( lieutrav = data2017$X38..EmploiLieuRegionEtranger ,lieuvol = data2017$X101..VolontariatRegionEtranger, lieudoc = data2017$X123..TheseRegionEtranger)
lieu$lieu[lieu$lieutrav != ""] = as.character(lieu$lieutrav[lieu$lieutrav != ""])
lieu$lieu[lieu$lieudoc != ""] = as.character(lieu$lieudoc[lieu$lieudoc != ""])
lieu$lieu[lieu$lieuvol != ""] = as.character(lieu$lieuvol[lieu$lieuvol != ""])
lieu$lieu = factor(lieu$lieu)
a = data.frame(lieu = lieu$lieu, weight=1, promo=data2017$X21..AnneeDiplomeVerifieParLeDiplome, filiere=data2017$X247..Option_ScolariteFiliereFormation)


levels(a$filiere) = c("Non renseigné" , "Master" , "Master" , "IF" , "ISI" , "ISSC" , "Master" , "Master" , "Master" , "Master" , "MMIS" , "SLE")
a$filiere = factor(a$filiere)
a$promo = factor(a$promo)
a = a[! is.na(a$promo),]
a = a[! (a$filiere == "Non renseigné"),]
#a = a[! is.na(a$lieu),]
a$weight=100./length(a$promo)

levels(a$lieu)

levels(a$lieu) = c( "Abroad" , "Auvergne-Rhône-Alpes" , "Province" , "Province" , "Province" , "Province" , "Province" , "Île-de-France" , "Province" , "Province" , "Provence-Alpes-Côte d’Azur")
a$lieu = factor(a$lieu)

p = ggplot(data=a, aes(x=lieu, fill=filiere))  + geom_bar(colour="white",(aes(weight=weight))) + coord_flip() + facet_wrap(c("filiere"))
p = p  + theme(plot.title=element_text("Lieu de travail (doctorat, volontariat) en fonction de la filière")) + xlab("Lieu") + scale_fill_hue(l=70, c=150)  + ylab("% des diplômés")
p
ggsave("../../Output/ensimag_2017_lieu.png", width=2*par("din")[1])

                                        # 2016
                                        # très basique, juste RA, PACA et province
lieu = data.frame( lieutrav = data2016$LieuTravailRegionEtrangerV2015 ,lieuvol = data2016$LieuVolontariatRegion, lieudoc = data2016$LieuLaboDoctoratEnquete2012)
lieu$lieu[lieu$lieutrav != ""] = as.character(lieu$lieutrav[lieu$lieutrav != ""])
lieu$lieu[lieu$lieudoc != ""] = as.character(lieu$lieudoc[lieu$lieudoc != ""])
lieu$lieu[lieu$lieuvol != ""] = as.character(lieu$lieuvol[lieu$lieuvol != ""])
lieu$lieu = factor(lieu$lieu)
a = data.frame(promo = data2016$Promo, filiere=data2016$Option_FiliereFormation, lieu = lieu$lieu, weight=1/length(data2016$Promo))
levels(a$lieu)
                                        # niveau d'origines
                                        # levels(a$lieu) = c("Alsace", "Aquitaine" , "Auvergne" , "Bretagne" , "Centre" , "Champagne-Ardenne" , "Etranger" , "Franche-Comté" , "Île-de-France" , "Languedoc-Roussillon" , "Lorraine" , "Midi-Pyrénées" , "Nord-Pas-de-Calais", "Picardie" , "Provence-Alpes-Côte d'Azur", "Rhône-Alpes")
levels(a$lieu) = c("Province Nord", "Province Sud" , "Province Sud" , "Province Nord" , "Province Sud" , "Province Nord" , "Etranger" , "Province Nord" , "Île-de-France" , "Province Sud" , "Province Nord" , "Province Sud" , "Province Nord", "Province Nord" , "Provence-Alpes-Côte d'Azur", "Rhône-Alpes")

                                        # levels(a$lieu) = c("Province-Sud", "Rhône-Alpes-Auvergne", "Province-Nord", "Province-Nord", "Etranger", "Province-Nord", "Île-de-France", "Province-Sud", "DOM/TOM", "Province-Nord", "Province-Sud", "Province-Nord", "Province-Nord", "Provence-Alpes-Côte d'Azur", "Rhône-Alpes-Auvergne")
levels(a$filiere) = c( "Master" , "Master" , "IF" , "ISI" , "Tcom/ISSC" , "Master" , "Master" , "Master" , "MMIS" , "SLE" , "Tcom/ISSC")
p = ggplot(data=a, aes(x=lieu, fill=filiere))  + geom_bar(colour="white",(aes(weight=weight))) + coord_flip()
p = p  + theme(plot.title=element_text("Lieu de travail (doctorat, volontariat) en fonction de la filière")) + xlab("Lieu") + scale_fill_hue(l=70, c=150)  + ylab("Nb de diplômés")
p
ggsave("../../Output/ensimag_2016_lieu.png", width=1*par("din")[1])

library(dplyr)
select(a, lieu, filiere, weight)  %>% group_by(lieu) %>% summarize(nombre = round(sum(weight),1))

                                        # 2015
                                        # Très basique avec juste RA, PACA et Province pour le reste
lieu = data.frame( lieutrav = data2015$LieuTravailRegionEtrangerV2015 ,lieuvol = data2015$LieuVolontariatRegion, lieudoc = data2015$LieuLaboDoctoratEnquete2012)
lieu$lieu[lieu$lieutrav != ""] = as.character(lieu$lieutrav[lieu$lieutrav != ""])
lieu$lieu[lieu$lieudoc != ""] = as.character(lieu$lieudoc[lieu$lieudoc != ""])
lieu$lieu[lieu$lieuvol != ""] = as.character(lieu$lieuvol[lieu$lieuvol != ""])
lieu$lieu = factor(lieu$lieu)
a = data.frame(promo = data2015$Promo, filiere=data2015$Option_FiliereFormation, lieu = lieu$lieu, weight=1/length(data2015$Promo))
#level(a$lieu) = c("Aquitaine", "Auvergne", "Bretagne", "Champagne-Ardenne", "Etranger", "Franche-Comté", "Île-de-France", "Languedoc-Roussillon", "La Réunion", "Lorraine", "Midi-Pyrénées", "Nord-Pas-de-Calais", "Pays de la Loire", "Provence-Alpes-Côte d'Azur", "Rhône-Alpes")
levels(a$lieu) = c("Province-Sud", "Rhône-Alpes-Auvergne", "Province-Nord", "Province-Nord", "Etranger", "Province-Nord", "Île-de-France", "Province-Sud", "DOM/TOM", "Province-Nord", "Province-Sud", "Province-Nord", "Province-Nord", "Provence-Alpes-Côte d'Azur", "Rhône-Alpes-Auvergne")
p = ggplot(data=a, aes(x=lieu, fill=filiere))  + geom_bar((aes(weight=weight))) + coord_flip()
p = p  + theme(plot.title=element_text("Lieu de travail (doctorat, volontariat) en fonction de la filière")) + xlab("Lieu") + scale_fill_hue(l=70, c=150)  + ylab("Nb de diplômés")
p
ggsave("../../Output/ensimag_2015_lieu.png", width=1.7*par("din")[1])


for (i in 0:2) { 
  if (i == 0) {
    data = data2013
    data = data[data$PromoEnquete2013 == 2012 | data$PromoEnquete2013 == 2011,]
    fname = "../../Output/ensimag_2013_lieu.pdf"
    fnameF = "../../Output/ensimag_2013_lieu_filiere.pdf"
    titre = "Lieu de travail des diplômés, par type d'activité, début 2O13, 6 et 18 mois"
    lapromo = data$PromoEnquete2013
    lafiliere= data$FiliereFormation
    anneeE = 2013
  } else if (i == 1) {
    data = data2012
    data = data[data$PromoEnquete2012 == 2011 | data$PromoEnquete2012 == 2010,]
    fname = "../../Output/ensimag_2012_lieu.pdf"
    fnameF = "../../Output/ensimag_2012_lieu_filiere.pdf"
    titre = "Lieu de travail des diplômés, par type d'activité, début 2O12, 6 et 18 mois"
    lapromo = data$PromoEnquete2012
    lafiliere= data$FiliereFormation
    anneeE = 2012
} else if (i == 2){
-    data = data2014
    data = data[data$Promo == 2013 | data$Promo == 2012,]
    fname = "../../Output/ensimag_2014_lieu.pdf"
    fnameF = "../../Output/ensimag_2014_lieu_filiere.pdf"
    titre = "Lieu de travail des diplômés, par type d'activité, début 2O14, 6 et 18 mois"
    lapromo = data$Promo
    lafiliere= data$FiliereFormation
    anneeE = 2014
    
} else if (i == 3) {
    data = data2015
    fname = "../../Output/ensimag_2015_lieu.pdf"
    fnameF = "../../Output/ensimag_2015_lieu_filiere.pdf"
    titre = "Lieu de travail des diplômés, par type d'activité, début 2O15, 6 et 18 mois"
    lapromo = data$Promo
    lafiliere= data$Option_FiliereFormation
    anneeE = 2015
}
}


  lieu = data.frame(promo=as.factor(lapromo), lieudoc = data$LieuLaboDoctoratEnquete2012, lieuTravail = data$LieuTravailDetailEnquete2012, activites= data$ActiviteActuelle, anneeEnquete = data$AnneeEnquete, filiere=lafiliere)
  lieu = lieu[lieu$anneeEnquete == anneeE,]
  lieu$lieu[lieu$lieudoc != ""] = as.character(lieu$lieudoc[lieu$lieudoc != ""])
  lieu$lieu[lieu$lieuTravail != ""] = as.character(lieu$lieuTravail[lieu$lieuTravail != ""])
  lieu$lieu = as.factor(lieu$lieu)
  lieu$lieu = relevel(lieu$lieu, "Provence-Alpes-Côte d'Azur")
  lieu$lieu = relevel(lieu$lieu, "Etranger")
  lieu$lieu = relevel(lieu$lieu, "Rhône-Alpes")
  lieu$lieu = relevel(lieu$lieu, "Île-de-France")

  
  p = ggplot(data=lieu, aes(x=as.factor(promo), fill=lieu), colour=black)  + geom_bar() + facet_grid(promo ~ activites)
  p  + theme(plot.title=element_text(titre)) + xlab("Promo Ensimag") + scale_fill_hue(l=70, c=150)  + ylab("Nb de diplômés")
  ggsave(fname, width=2*par("din")[1])

  p = ggplot(data=lieu, aes(x=as.factor(filiere), fill=lieu), colour=black)  + geom_bar() + facet_grid(promo ~ activites)
  p  + theme(plot.title=element_text(titre)) + xlab("Filière Ensimag") + scale_fill_hue(l=70, c=150)  + ylab("Nb de diplômés")
  ggsave(fnameF, width=2*par("din")[1])

}














