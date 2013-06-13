source("script_data.R")

library(ggplot2)

lieu = data.frame(promo=data2012$PromoEnquete2012, lieudoc = data2012$LieuLaboDoctoratEnquete2012, lieuTravail = data2012$LieuTravailDetailEnquete2012, activites= data2012$ActiviteActuelle, anneeEnquete = data2012$AnneeEnquete)
lieu = lieu[lieu$anneeEnquete == 2012,]
lieu$lieu[lieu$lieudoc != ""] = as.character(lieu$lieudoc[lieu$lieudoc != ""])
lieu$lieu[lieu$lieuTravail != ""] = as.character(lieu$lieuTravail[lieu$lieuTravail != ""])
lieu$lieu = as.factor(lieu$lieu)
lieu = lieu[! is.na(lieu$lieu),] 

print(count(as.data.frame(lieu$lieu[lieu$lieu == "Etranger" & lieu$promo == 2011 & ! is.na(lieu$lieu)])))
print(count(as.data.frame(lieu$lieu[lieu$lieu == "Etranger" & lieu$promo == 2010 & ! is.na(lieu$lieu)])))
print(count(as.data.frame(lieu$promo[lieu$promo == 2011])))
print(count(as.data.frame(lieu$promo[lieu$promo == 2010])))

duree = data.frame(duree= data2012$DureeRechercheEmploiINPG.2, promo= data2012$PromoEnquete2012 )
duree = duree[! is.na(duree$duree),]
summary(duree)
