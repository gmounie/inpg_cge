## premier emploi par promo

source("../DataReader/dataReader.R")

library(ggplot2)

                                        # 2018
a = data.frame(premierEmploi=data2018$X137..1erEmploiDecritPrecedemment, promo=data2018$X14..AnneeDiplomeVerifieParLeDiplome, filiere=data2018$X258..Option_ScolariteFiliereFormation)

levels(a$filiere) = c("NA", "IF" , "ISI" , "ISSC" , "Master" , "Master" , "Master" , "MMIS" , "SLE")

a$filiere = factor(a$filiere)
a$promo = factor(a$promo)
a = a[! is.na(a$promo),]
a = a[! (a$filiere == "NA"),]
ggplot(a,aes(premierEmploi, fill=promo)) + geom_bar(colour="white") + facet_wrap(c("filiere")) + ggtitle("Est-ce votre premier emploi ?")

ggsave("../../Output/ensimag_2018_premieremploi.png", width=1.5*par("din")[1])

ggplot(a,aes(premierEmploi, fill=filiere)) + geom_bar(colour="white") + facet_wrap(c("promo")) + ggtitle("Est-ce votre premier emploi ?")

ggsave("../../Output/ensimag_2018_premieremploi_promo.png", width=1.5*par("din")[1])

                                        # 2017
a = data.frame(premierEmploi=data2017$X138..1erEmploiDecritPrecedemment, promo=data2017$X21..AnneeDiplomeVerifieParLeDiplome, filiere=data2017$X247..Option_ScolariteFiliereFormation)

levels(a$filiere) = c("Non renseigné" , "Master" , "Master" , "IF" , "ISI" , "ISSC" , "Master" , "Master" , "Master" , "Master" , "MMIS" , "SLE")
a$filiere = factor(a$filiere)
a$promo = factor(a$promo)
a = a[! is.na(a$promo),]
a = a[! (a$filiere == "Non renseigné"),]
ggplot(a,aes(premierEmploi, fill=promo)) + geom_bar(colour="white") + facet_wrap(c("filiere")) + ggtitle("Est-ce votre premier emploi ?")

ggsave("../../Output/ensimag_2017_premieremploi.png", width=1.5*par("din")[1])

                                        # 2016
a = data.frame(premierEmploi=data2016$EmploiPrecedent1er, promo=data2016$AnneeDiplome, filiere=data2016$Option_FiliereFormation)


levels(a$filiere) = c( "Master" , "Master" , "IF" , "ISI" , "Tcom/ISSC" , "Master" , "Master" , "Master" , "MMIS" , "SLE" , "Tcom/ISSC")

ggplot(a,aes(premierEmploi, fill=filiere)) + geom_bar(colour="white") + facet_wrap(c("promo"))
ggsave("../../Output/ensimag_2016_premieremploi.png", width=1.5*par("din")[1])

