source("../DataReader/dataReader.R")

library(dplyr)

## enquete 2018

## nb réponses promo 2017
data2018 %>% select(X14..AnneeDiplomeVerifieParLeDiplome, X20..ActiviteActuelle) %>% filter(X14..AnneeDiplomeVerifieParLeDiplome == 2017) %>% summarize(status = n())

## nb réponses promo 2016
data2018 %>% select(X14..AnneeDiplomeVerifieParLeDiplome, X20..ActiviteActuelle) %>% filter(X14..AnneeDiplomeVerifieParLeDiplome == 2016) %>% summarize(status = n())


## status promo 2017
data2018 %>% select(X14..AnneeDiplomeVerifieParLeDiplome, X20..ActiviteActuelle) %>% filter(X14..AnneeDiplomeVerifieParLeDiplome == 2017) %>% group_by(X20..ActiviteActuelle) %>% summarize(status = n())

153+19+5

## status promo 2016
data2018 %>% select(X14..AnneeDiplomeVerifieParLeDiplome, X20..ActiviteActuelle) %>% filter(X14..AnneeDiplomeVerifieParLeDiplome == 2016) %>% group_by(X20..ActiviteActuelle) %>% summarize(status = n())


## emploi trouvé en moins de 2 mois 2017
data2018 %>% select(X14..AnneeDiplomeVerifieParLeDiplome, X20..ActiviteActuelle, X140..1erEmploiLapsPourTrouverApresDiplomeMois) %>% filter(X14..AnneeDiplomeVerifieParLeDiplome == 2017, X140..1erEmploiLapsPourTrouverApresDiplomeMois <= 2) %>% group_by(X20..ActiviteActuelle) %>% summarize(status = n())

133+3+1

## emploi trouvé en moins de 2 mois 2016
data2018 %>% select(X14..AnneeDiplomeVerifieParLeDiplome, X20..ActiviteActuelle, X140..1erEmploiLapsPourTrouverApresDiplomeMois) %>% filter(X14..AnneeDiplomeVerifieParLeDiplome == 2016, X140..1erEmploiLapsPourTrouverApresDiplomeMois <= 2) %>% group_by(X20..ActiviteActuelle) %>% summarize(status = n())

97+2+1+6+2

## Nombre de CDI en France 2017
data2018 %>% select(X14..AnneeDiplomeVerifieParLeDiplome, X20..ActiviteActuelle, X38..EmploiContratFrance) %>% filter(X14..AnneeDiplomeVerifieParLeDiplome == 2017, X38..EmploiContratFrance == "CDI (Contrat de travail à durée indéterminée), y compris CDIC") %>% group_by(X20..ActiviteActuelle) %>% summarize(status = n())

## Nombre de CDI en France 2016
data2018 %>% select(X14..AnneeDiplomeVerifieParLeDiplome, X20..ActiviteActuelle, X38..EmploiContratFrance) %>% filter(X14..AnneeDiplomeVerifieParLeDiplome == 2016, X38..EmploiContratFrance == "CDI (Contrat de travail à durée indéterminée), y compris CDIC") %>% group_by(X20..ActiviteActuelle) %>% summarize(status = n())

## Nombre de diplomés 2017 employé à l'étranger
data2018 %>% select(X14..AnneeDiplomeVerifieParLeDiplome, X20..ActiviteActuelle, X33..EmploiLieuRegionEtranger, X100..VolontariatRegionEtranger, X122..TheseRegionEtranger) %>% filter(X14..AnneeDiplomeVerifieParLeDiplome == 2017, X33..EmploiLieuRegionEtranger  == "Étranger" | X100..VolontariatRegionEtranger  == "Étranger" | X122..TheseRegionEtranger == "Étranger") %>% group_by(X20..ActiviteActuelle) %>% summarize(status = n())

34+2+5

## Nombre de diplomés 2016 employé à l'étranger
data2018 %>% select(X14..AnneeDiplomeVerifieParLeDiplome, X20..ActiviteActuelle, X33..EmploiLieuRegionEtranger, X100..VolontariatRegionEtranger, X122..TheseRegionEtranger) %>% filter(X14..AnneeDiplomeVerifieParLeDiplome == 2016, X33..EmploiLieuRegionEtranger  == "Étranger" | X100..VolontariatRegionEtranger  == "Étranger" | X122..TheseRegionEtranger == "Étranger") %>% group_by(X20..ActiviteActuelle) %>% summarize(status = n())

22+4+2

## Nombre de diplomés 2017 employé en France
data2018 %>% select(X14..AnneeDiplomeVerifieParLeDiplome, X20..ActiviteActuelle, X33..EmploiLieuRegionEtranger, X100..VolontariatRegionEtranger, X122..TheseRegionEtranger) %>% filter(X14..AnneeDiplomeVerifieParLeDiplome == 2017, X33..EmploiLieuRegionEtranger  != "Étranger", X100..VolontariatRegionEtranger  != "Étranger", X122..TheseRegionEtranger != "Étranger") %>% group_by(X20..ActiviteActuelle) %>% summarize(status = n())

119 + 17 

## Nombre de diplomés 2016 employé en France
data2018 %>% select(X14..AnneeDiplomeVerifieParLeDiplome, X20..ActiviteActuelle, X33..EmploiLieuRegionEtranger, X100..VolontariatRegionEtranger, X122..TheseRegionEtranger) %>% filter(X14..AnneeDiplomeVerifieParLeDiplome == 2016, X33..EmploiLieuRegionEtranger  != "Étranger", X100..VolontariatRegionEtranger  != "Étranger", X122..TheseRegionEtranger != "Étranger") %>% group_by(X20..ActiviteActuelle) %>% summarize(status = n())

95 + 24

## Salaire brut hors thèse (mais donc avec les VIE ?) median homme/femme, France/étranger, avec/sans prime
## France hors prime HF
data2018 %>% select(X14..AnneeDiplomeVerifieParLeDiplome, X4..SexeVerifieParLeDiplome, X20..ActiviteActuelle, X33..EmploiLieuRegionEtranger, X100..VolontariatRegionEtranger, X122..TheseRegionEtranger, X183..EmploiCorrige_BrutAnnuelHorsPrimes, X184..EmploiCorrige_PrimesBrut) %>% filter(X33..EmploiLieuRegionEtranger  != "Étranger", X100..VolontariatRegionEtranger  != "Étranger", X122..TheseRegionEtranger != "Étranger") %>% filter(X20..ActiviteActuelle != "En thèse") %>% group_by(X14..AnneeDiplomeVerifieParLeDiplome, X4..SexeVerifieParLeDiplome) %>% summarize(salaire = median(X183..EmploiCorrige_BrutAnnuelHorsPrimes, na.rm=TRUE))

## France Avec prime HF
data2018 %>% select(X14..AnneeDiplomeVerifieParLeDiplome, X4..SexeVerifieParLeDiplome, X20..ActiviteActuelle, X33..EmploiLieuRegionEtranger, X100..VolontariatRegionEtranger, X122..TheseRegionEtranger, X183..EmploiCorrige_BrutAnnuelHorsPrimes, X184..EmploiCorrige_PrimesBrut) %>% mutate(avecPrime= X183..EmploiCorrige_BrutAnnuelHorsPrimes + X184..EmploiCorrige_PrimesBrut) %>% filter(X33..EmploiLieuRegionEtranger  != "Étranger", X100..VolontariatRegionEtranger  != "Étranger", X122..TheseRegionEtranger != "Étranger") %>% filter(X20..ActiviteActuelle != "En thèse") %>% group_by(X14..AnneeDiplomeVerifieParLeDiplome, X4..SexeVerifieParLeDiplome) %>% summarize(salaire = median(avecPrime, na.rm=TRUE))

## Etranger hors prime HF
data2018 %>% select(X14..AnneeDiplomeVerifieParLeDiplome, X4..SexeVerifieParLeDiplome, X20..ActiviteActuelle, X33..EmploiLieuRegionEtranger, X100..VolontariatRegionEtranger, X122..TheseRegionEtranger, X183..EmploiCorrige_BrutAnnuelHorsPrimes, X184..EmploiCorrige_PrimesBrut) %>% filter(X33..EmploiLieuRegionEtranger  == "Étranger" |  X100..VolontariatRegionEtranger  == "Étranger" | X122..TheseRegionEtranger == "Étranger") %>% filter(X20..ActiviteActuelle != "En thèse") %>% group_by(X14..AnneeDiplomeVerifieParLeDiplome, X4..SexeVerifieParLeDiplome) %>% summarize(salaire = median(X183..EmploiCorrige_BrutAnnuelHorsPrimes, na.rm=TRUE))

## Etranger Avec prime HF
data2018 %>% select(X14..AnneeDiplomeVerifieParLeDiplome, X4..SexeVerifieParLeDiplome, X20..ActiviteActuelle, X33..EmploiLieuRegionEtranger, X100..VolontariatRegionEtranger, X122..TheseRegionEtranger, X183..EmploiCorrige_BrutAnnuelHorsPrimes, X184..EmploiCorrige_PrimesBrut) %>% mutate(avecPrime= X183..EmploiCorrige_BrutAnnuelHorsPrimes + X184..EmploiCorrige_PrimesBrut) %>% filter(X33..EmploiLieuRegionEtranger  == "Étranger" |  X100..VolontariatRegionEtranger  == "Étranger" | X122..TheseRegionEtranger == "Étranger") %>% filter(X20..ActiviteActuelle != "En thèse") %>% group_by(X14..AnneeDiplomeVerifieParLeDiplome, X4..SexeVerifieParLeDiplome) %>% summarize(salaire = median(avecPrime, na.rm=TRUE))
