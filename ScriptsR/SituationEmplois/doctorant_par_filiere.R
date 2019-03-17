source("../DataReader/dataReader.R")

library(dplyr)
## enquete 2019

# nombre de doctorant sur les 3 années
data2019 %>% select(X14..AnneeDiplomeVerifieParLeDiplome, activite=X15..ActiviteActuelle) %>% filter(activite == "Studying for a PhD") %>% summarize(doctorant = n()) 
# pourcentage de doctorant sur les 3 années par filière
a = data2018 %>% select(annee=X14..AnneeDiplomeVerifieParLeDiplome, activite=X15..ActiviteActuelle, filiere=X258..Option_ScolariteFiliereFormation) %>% filter(activite == "Studying for a PhD") %>% group_by(filiere)%>% summarize(doctorant = n()) 
b = data2018 %>% select(annee=X14..AnneeDiplomeVerifieParLeDiplome, activite=X20..ActiviteActuelle, filiere=X258..Option_ScolariteFiliereFormation) %>% group_by(filiere)%>% summarize(doctorant = n()) 
d = data.frame(filiere= a$filiere, percent=(a$doctorant)/b$doctorant, nb=a$doctorant, total=b$doctorant)
d

## enquete 2018

# nombre de doctorant sur les 3 années
data2018 %>% select(X14..AnneeDiplomeVerifieParLeDiplome, activite=X20..ActiviteActuelle) %>% filter(activite == "En thèse") %>% summarize(doctorant = n()) 
# pourcentage de doctorant sur les 3 années par filière
a = data2018 %>% select(annee=X14..AnneeDiplomeVerifieParLeDiplome, activite=X20..ActiviteActuelle, filiere=X258..Option_ScolariteFiliereFormation) %>% filter(activite == "En thèse") %>% group_by(filiere)%>% summarize(doctorant = n()) 
b = data2018 %>% select(annee=X14..AnneeDiplomeVerifieParLeDiplome, activite=X20..ActiviteActuelle, filiere=X258..Option_ScolariteFiliereFormation) %>% group_by(filiere)%>% summarize(doctorant = n()) 
d = data.frame(filiere= a$filiere, percent=(a$doctorant)/b$doctorant, nb=a$doctorant, total=b$doctorant)
d
