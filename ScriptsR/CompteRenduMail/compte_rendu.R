source("../DataReader/dataReader.R")
# enquete 2018
repondants6_2017 = length( data2018$X244..AnneeEnquete[ data2018$X244..AnneeEnquete == 2018 & data2018$X14..AnneeDiplomeVerifieParLeDiplome == 2017 & data2018$X20..ActiviteActuelle != ""])
repondants18_2016 = length( data2018$X244..AnneeEnquete[ data2018$X244..AnneeEnquete == 2018 & data2018$X14..AnneeDiplomeVerifieParLeDiplome == 2016 & data2018$X20..ActiviteActuelle != ""])
repondants30_2015 = length( data2018$X244..AnneeEnquete[ data2018$X244..AnneeEnquete == 2018 & data2018$X14..AnneeDiplomeVerifieParLeDiplome == 2015 & data2018$X20..ActiviteActuelle != ""])

sink(file="sortie2018.txt", append=FALSE, split=TRUE)

print("### % Répondants par promo 2017, 2016 et 2015 ###")
print(repondants6_2017/taille2017)
print(repondants18_2016/taille2016)
print(repondants30_2015/taille2015)


postes = data.frame(poste=data2018$X256..Option_EmploiPosteListe, activité=data2018$X20..ActiviteActuelle, secteur=data2018$X55..EmploiEntrepriseSecteurActivite, lieu=data2018$X33..EmploiLieuRegionEtranger, lieuThese=data2018$X122..TheseRegionEtranger, salaire=data2018$X183..EmploiCorrige_BrutAnnuelHorsPrimes, salairePrime=data2018$X186..Emploi_SalaireBrutAnnuelAvecPrimes, salaireThese=data2018$X136..TheseSalaireBrutAnnuelAvecPrimes)



                                        # postes
print("### Situation, les 3 promos  (Blanc: NA non répondu)###")
summary(as.data.frame(postes$activité), maxsum=10)

print("### Postes occupés , les 3 promos  (Blanc: NA non répondu ou pas concerné)###")
summary(as.data.frame(postes$poste), maxsum=10)
print("### Nombre de type de postes, les 3 promo ###")
length(levels(postes$poste))

levels(postes$secteur) = c(levels(postes$secteur), "Recherche (doctorat)")
postes[postes$activité == "En thèse",]$secteur = "Recherche (doctorat)"
print("### Secteur (1er chiffre: NA non répondu ou pas concerné)###")
summary(as.data.frame(postes$secteur), maxsum=10)
print("### Nombre de type de secteurs, les 3 promo ###")
length(levels(postes$secteur))

print("### Lieu Entreprise (1er chiffre: NA non répondu ou pas concerné)###")
summary(as.data.frame(postes$lieu), maxsum=10)

print("### Lieu Doctorat (1er chiffre: NA non répondu ou pas concerné)###")
summary(as.data.frame(postes$lieuThese), maxsum=10)

print("### Quartile salaire hors prime France  ###")
quantile(postes$salaire[postes$lieu != "Étranger"], na.rm=TRUE, probs = seq(0.25,0.75, 0.25))

print("### Quartile salaire hors prime Étranger ###")
quantile(postes$salaire[postes$lieu == "Étranger"], na.rm=TRUE, probs = seq(0.25,0.75, 0.25))

print("### Quartile salaire avec primes France  ###")
quantile(postes$salairePrime[postes$lieu != "Étranger"], na.rm=TRUE, probs = seq(0.25,0.75, 0.25))

print("### Quartile salaire avec primes Étranger ###")
quantile(postes$salairePrime[postes$lieu == "Étranger"], na.rm=TRUE, probs = seq(0.25,0.75, 0.25))

print("### Quartile Salaire Doctorat France et Étranger ###")
quantile(postes$salaireThese, na.rm=TRUE, probs = seq(0.25,0.75, 0.25))

sink()



