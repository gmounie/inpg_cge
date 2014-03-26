source("../DataReader/dataReader.R")

## Donne le salaire moyen et le salaire médian ?
salaireBrut2014 = as.data.frame(data2014[ data2014$AnneeEnquete == 2014, c("SalaireBrutAnnuelAvecPrimes", "SalaireBrutAnnuelHorsPrimes", "Sexe", "AnneeDiplome", "AnneeEnquete", "RevenuBrutAnnuelAvecPrimesThesard", "RevenuBrutAnnuelHorsPrimesThesard", "ActiviteActuelle", "LieuTravailDetailEnquete2012", "LieuLaboDoctoratEnquete2012")])
salaireBrut2013 = as.data.frame(data2013[ data2013$AnneeEnquete == 2013, c("SalaireBrutAnnuelAvecPrimes", "SalaireBrutAnnuelHorsPrimes", "Sexe", "AnneeDiplome", "AnneeEnquete", "RevenuBrutAnnuelAvecPrimesThesard", "RevenuBrutAnnuelHorsPrimesThesard", "ActiviteActuelle", "LieuTravailDetailEnquete2012", "LieuLaboDoctoratEnquete2012")])
salaireBrut2012 = as.data.frame(data2012[ data2012$AnneeEnquete == 2012, c("SalaireBrutAnnuelAvecPrimes", "SalaireBrutAnnuelHorsPrimes", "Sexe", "AnneeDiplome", "AnneeEnquete", "RevenuBrutAnnuelAvecPrimesThesard", "RevenuBrutAnnuelHorsPrimesThesard", "ActiviteActuelle", "LieuTravailDetailEnquete2012", "LieuLaboDoctoratEnquete2012")])

salaire = rbind(salaireBrut2014, salaireBrut2013, salaireBrut2012)
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

summary(salaire$SalaireAvecPrime[salaire$AnneeEnquete == 2014 & salaire$AnneeDiplome== 2012])
summary(salaire$SalaireAvecPrime[salaire$AnneeEnquete == 2014 & salaire$AnneeDiplome== 2013])
summary(salaire$SalaireAvecPrime[salaire$AnneeEnquete == 2013 & salaire$AnneeDiplome== 2011])
summary(salaire$SalaireAvecPrime[salaire$AnneeEnquete == 2013 & salaire$AnneeDiplome== 2012])
summary(salaire$SalaireAvecPrime[salaire$AnneeEnquete == 2012 & salaire$AnneeDiplome== 2010])
summary(salaire$SalaireAvecPrime[salaire$AnneeEnquete == 2012 & salaire$AnneeDiplome== 2011])



## affiche toutes les valeurs de salaires avec des points et un jitter
## ou bien un boxplot ?

p = ggplot(data=salaire, aes(x=AnneeDiplome, y=SalaireAvecPrime, ymin=0)) + geom_jitter(aes(alpha=0.01)) + geom_boxplot(outlier.size=0)
p + facet_grid(dureesortie ~ AnneeEnquete) + opts(title="Salaire Brut (+ primes) par année de promotion et année de l'enquête")
ggsave("../../Output/ensimag_salaire_total.pdf", width=2*par("din")[1])
p + facet_grid(dureesortie ~ AnneeEnquete + Sexe) + opts(title="Salaire Brut (+ primes) par année de promotion, sexe et année de l'enquête")
ggsave("../../Output/ensimag_salaire_sexe.pdf", width=2*par("din")[1])
p + facet_grid(dureesortie ~ AnneeEnquete + ActiviteActuelle) + opts(title="Salaire Brut (+ primes) par année de promotion, activité et année de l'enquête")
ggsave("../../Output/ensimag_salaire_activite.pdf", width=2*par("din")[1])
p + facet_grid(dureesortie ~ AnneeEnquete + lieu) + opts(title="Salaire Brut (+ primes) par année de promotion, lieu de travail et année de l'enquête")
ggsave("../../Output/ensimag_salaire_lieu.pdf", width=2.2*par("din")[1])


