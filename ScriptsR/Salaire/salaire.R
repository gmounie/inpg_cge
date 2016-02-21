source("../DataReader/dataReader.R")

## Donne le salaire moyen et le salaire médian ?
salaireBrut2016 = as.data.frame(data2016[ data2016$AnneeEnquete == 2016, c("RemunerationTotaleCorrigee", "SalaireHorsPrimesCorrige", "Sexe", "AnneeDiplome", "AnneeEnquete", "RevenuBrutAnnuelAvecPrimesThesard", "RevenuBrutAnnuelHorsPrimesThesard", "ActiviteActuelleV2016", "LieuTravailRegionEtrangerV2015", "LieuLaboDoctoratEnquete2012")])
names(salaireBrut2016) = c("SalaireBrutAnnuelAvecPrimes", "SalaireBrutAnnuelHorsPrimes", "Sexe", "AnneeDiplome", "AnneeEnquete", "RevenuBrutAnnuelAvecPrimesThesard", "RevenuBrutAnnuelHorsPrimesThesard", "ActiviteActuelle", "LieuTravailDetailEnquete2012", "LieuLaboDoctoratEnquete2012");

salaireBrut2015 = as.data.frame(data2015[ data2015$AnneeEnquete == 2015, c("RemunerationTotaleCorrigee", "SalaireHorsPrimesCorrige", "Sexe", "AnneeDiplome", "AnneeEnquete", "RevenuBrutAnnuelAvecPrimesThesard", "RevenuBrutAnnuelHorsPrimesThesard", "ActiviteActuelle", "LieuTravailRegionEtrangerV2015", "LieuLaboDoctoratEnquete2012")])
names(salaireBrut2015) = c("SalaireBrutAnnuelAvecPrimes", "SalaireBrutAnnuelHorsPrimes", "Sexe", "AnneeDiplome", "AnneeEnquete", "RevenuBrutAnnuelAvecPrimesThesard", "RevenuBrutAnnuelHorsPrimesThesard", "ActiviteActuelle", "LieuTravailDetailEnquete2012", "LieuLaboDoctoratEnquete2012");

salaireBrut2014 = as.data.frame(data2014[ data2014$AnneeEnquete == 2014, c("SalaireBrutAnnuelAvecPrimes", "SalaireBrutAnnuelHorsPrimes", "Sexe", "AnneeDiplome", "AnneeEnquete", "RevenuBrutAnnuelAvecPrimesThesard", "RevenuBrutAnnuelHorsPrimesThesard", "ActiviteActuelle", "LieuTravailDetailEnquete2012", "LieuLaboDoctoratEnquete2012")])
salaireBrut2013 = as.data.frame(data2013[ data2013$AnneeEnquete == 2013, c("SalaireBrutAnnuelAvecPrimes", "SalaireBrutAnnuelHorsPrimes", "Sexe", "AnneeDiplome", "AnneeEnquete", "RevenuBrutAnnuelAvecPrimesThesard", "RevenuBrutAnnuelHorsPrimesThesard", "ActiviteActuelle", "LieuTravailDetailEnquete2012", "LieuLaboDoctoratEnquete2012")])
salaireBrut2012 = as.data.frame(data2012[ data2012$AnneeEnquete == 2012, c("SalaireBrutAnnuelAvecPrimes", "SalaireBrutAnnuelHorsPrimes", "Sexe", "AnneeDiplome", "AnneeEnquete", "RevenuBrutAnnuelAvecPrimesThesard", "RevenuBrutAnnuelHorsPrimesThesard", "ActiviteActuelle", "LieuTravailDetailEnquete2012", "LieuLaboDoctoratEnquete2012")])

salaire = rbind(salaireBrut2016, salaireBrut2015, salaireBrut2014, salaireBrut2013, salaireBrut2012)
salaire = salaire[salaire$Sexe != "" & ! is.na(salaire$Sexe),]
salaire = salaire[salaire$AnneeDiplome != "" & ! is.na(salaire$AnneeDiplome),]
salaire$Sexe = as.factor(salaire$Sexe)
salaire$dureesortie = as.factor(as.integer(salaire$AnneeEnquete) - as.integer(salaire$AnneeDiplome))
salaire$AnneeDiplome = as.factor(salaire$AnneeDiplome)
salaire$AnneeEnquete = factor(salaire$AnneeEnquete)


#salaire$AnneeEnquete = relevel(salaire$AnneeEnquete, "2013")
#salaire$AnneeEnquete = relevel(salaire$AnneeEnquete, "2014")
salaire$SalaireAvecPrime = pmax( salaire$SalaireBrutAnnuelHorsPrimes, salaire$SalaireBrutAnnuelAvecPrimes, salaire$RevenuBrutAnnuelAvecPrimesThesard, salaire$RevenuBrutAnnuelHorsPrimesThesard, na.rm= TRUE )

salaire = salaire[! is.na(salaire$SalaireAvecPrime),]

salaire$lieu = paste(as.character(salaire$LieuTravailDetailEnquete2012), as.character(salaire$LieuLaboDoctoratEnquete2012))
salaire$lieu = sub("NA", "", salaire$lieu)
salaire$lieu = gsub(" ", "", salaire$lieu)
salaire[salaire$lieu != "Etranger" & salaire$lieu != "Île-de-France"  & salaire$lieu != "","lieu"] = "France (Province)"
salaire[salaire$lieu == "","lieu"] = "Non renseigné"

library(ggplot2)

summary(salaire$SalaireAvecPrime[salaire$AnneeEnquete == 2016 & salaire$AnneeDiplome== 2015])
summary(salaire$SalaireAvecPrime[salaire$AnneeEnquete == 2016 & salaire$AnneeDiplome== 2014])
summary(salaire$SalaireAvecPrime[salaire$AnneeEnquete == 2016 & salaire$AnneeDiplome== 2013])

summary(salaire$SalaireAvecPrime[salaire$AnneeEnquete == 2015 & salaire$AnneeDiplome== 2013])
summary(salaire$SalaireAvecPrime[salaire$AnneeEnquete == 2015 & salaire$AnneeDiplome== 2014])
summary(salaire$SalaireAvecPrime[salaire$AnneeEnquete == 2015 & salaire$AnneeDiplome== 2012])
summary(salaire$SalaireAvecPrime[salaire$AnneeEnquete == 2014 & salaire$AnneeDiplome== 2012])
summary(salaire$SalaireAvecPrime[salaire$AnneeEnquete == 2014 & salaire$AnneeDiplome== 2013])
summary(salaire$SalaireAvecPrime[salaire$AnneeEnquete == 2013 & salaire$AnneeDiplome== 2011])
summary(salaire$SalaireAvecPrime[salaire$AnneeEnquete == 2013 & salaire$AnneeDiplome== 2012])
summary(salaire$SalaireAvecPrime[salaire$AnneeEnquete == 2012 & salaire$AnneeDiplome== 2010])
summary(salaire$SalaireAvecPrime[salaire$AnneeEnquete == 2012 & salaire$AnneeDiplome== 2011])



## affiche toutes les valeurs de salaires avec des points et un jitter
## ou bien un boxplot ?

p = ggplot(data=salaire, aes(x=AnneeDiplome, y=SalaireAvecPrime, ymin=0)) + geom_jitter(aes(alpha=0.01)) + geom_boxplot(outlier.size=0)
p + facet_grid(dureesortie ~ AnneeEnquete) + theme(title=element_text("Salaire Brut (+ primes) par année de promotion et année de l'enquête"))
ggsave("../../Output/ensimag_salaire_total.png", width=2*par("din")[1])
p + facet_grid(dureesortie ~ AnneeEnquete + Sexe) + theme(title=element_text("Salaire Brut (+ primes) par année de promotion, sexe et année de l'enquête"))
ggsave("../../Output/ensimag_salaire_sexe.png", width=2*par("din")[1])
p + facet_grid(dureesortie ~ AnneeEnquete + ActiviteActuelle) + theme(title=element_text("Salaire Brut (+ primes) par année de promotion, activité et année de l'enquête"))
ggsave("../../Output/ensimag_salaire_activite.png", width=2*par("din")[1])
p + facet_grid(dureesortie ~ AnneeEnquete + lieu) + theme(title=element_text("Salaire Brut (+ primes) par année de promotion, lieu de travail et année de l'enquête"))
ggsave("../../Output/ensimag_salaire_lieu.png", width=2.2*par("din")[1])

p + facet_grid(dureesortie ~ .) + theme(title=element_text("Salaire Brut (+ primes) par année de promotion, lieu de travail et année de l'enquête")) + ylim(20000,50000)
ggsave("../../Output/ensimag_salaire_evol.png", width=2.2*par("din")[1])


s15 = data.frame(AnneeDiplome= factor(data2015$Promo), SalaireAvecPrime = data2015$RemunerationTotaleCorrigee)
s15f = na.omit(s15[s15$SalaireAvecPrime < 100000 & s15$SalaireAvecPrime > 10000,])
p = ggplot(data=s15f, aes(x=AnneeDiplome, y=SalaireAvecPrime, ymin=0)) + geom_jitter(position = position_jitter(width=0.3), aes(alpha=0.01)) + geom_boxplot(outlier.size=0.1)
p + theme(title=element_text("Salaire Brut (+ primes) par année de promotion et année de l'enquête")) + geom_text(label="8 2012, 3 2013, 1 2014 > 100000", x= 2, y = 100000 , color = "red", size= 8) + geom_text(label="1 2012, 2 2013 proche 0 (création d'entreprise)", x= 2, y = 90000 , color = "red", size= 8) + geom_text(label="Pas de IF en 2013 !", x= 2, y = 80000 , color = "red", size= 8) + ggtitle("Salaire avec prime, enquête 2015, filtré aux extrèmes")
ggsave("../../Output/ensimag_2015_salaire_total_inf100000.png", width=1.5*par("din")[1])

















