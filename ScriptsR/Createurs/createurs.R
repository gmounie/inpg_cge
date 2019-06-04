source("../DataReader/dataReader.R")
data2013 = data2013[data2013$AnneeEnquete == 2013,]

library("dplyr")
library("ggplot2")

df19 = data.frame(Enquete= data2019$X248..AnneeDiplome, promo= data2019$X14..AnneeDiplomeVerifieParLeDiplome,
                  nom= data2019$X3..NomVerifieParLeDiplome,
                  prenom= data2019$X2..PrenomVerifieParLeDiplome,
                  createur= data2019$X18..CreationRepriseEntreprise,
                  incubateur= data2019$X21..CreationRepriseIncubateur)


df18 = data.frame(Enquete= data2018$X244..AnneeEnquete, promo= data2018$X14..AnneeDiplomeVerifieParLeDiplome,
                  nom= data2018$X3..NomVerifieParLeDiplome,
                  prenom= data2018$X2..PrenomVerifieParLeDiplome,
                  createur= data2018$X23..EnActiviteCreationRepriseEntreprise,
                  incubateur= data2018$X26..EnActiviteCreationRepriseIncubateur)

df17 = data.frame(Enquete= data2017$X1..AnneeEnquete, promo= data2017$X21..AnneeDiplomeVerifieParLeDiplome,
                  nom= data2017$X9..NomVerifieParLeDiplome,
                  prenom= data2017$X8..PrenomVerifieParLeDiplome,
                  createur= data2017$X28..EnActiviteCreationRepriseEntreprise,
                  incubateur= data2017$X31..EnActiviteCreationRepriseIncubateur)

df16 = data.frame(Enquete=data2016$AnneeEnquete, promo=data2016$AnneeDiplomeVerifieParLeDiplome,
                  nom= data2016$NomVerifieParLeDiplome,
                  prenom= data2016$NomVerifieParLeDiplome,
                  createur= data2016$CreateurRepreneurEntreprise,
                  incubateur= data2016$SoutienIncubateur)

df15 = data.frame(Enquete=data2015$AnneeEnquete, promo=data2015$AnneeDiplome,
                  nom=data2015$NomVerifieParLeDiplome,
                  prenom=data2015$PrenomVerifieParLeDiplome,
                  createur=data2015$CreateurEntreprise,
                  incubateur=NA)


df19 %>% filter(createur == "Yes, company currently active" | createur == "Yes, company currently being set up (underway)" ) %>% arrange(incubateur, promo) %>% write.csv(file="donneesStephanie19.csv")

df18 %>% filter(createur == "Oui" | createur == "En cours de création / reprise" ) %>% arrange(incubateur, promo)%>% write.csv(file="donneesStephanie18.csv")

df17 %>% filter(createur == "Yes" | createur == "Company currently being set up" ) %>% arrange(incubateur, promo)%>% write.csv(file="donneesStephanie17.csv")

df16 %>% filter(createur == "Le/la créateur/trice" | createur == "Le/la repreneur/se") %>% arrange(incubateur, promo)%>% write.csv(file="donneesStephanie16.csv")

df15 %>% filter(createur == "Le/la créateur/trice" | createur == "Le/la repreneur/se")  %>% arrange(incubateur, promo)%>% write.csv(file="donneesStephanie15.csv")
