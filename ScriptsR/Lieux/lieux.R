source("../DataReader/dataReader.R")

library(ggplot2)

for (i in 0:1) { 
  if (i == 0) {
    data = data2013
    data = data[data$PromoEnquete2013 == 2012 | data$PromoEnquete2013 == 2011,]
    fname = "../../Output/ensimag_2013_lieu.pdf"
    titre = "Lieu de travail des diplômés, par type d'activité, début 2O13, 6 et 18 mois"
    lapromo = data$PromoEnquete2013
    anneeE = 2013
  } else {
    data = data2012
    data = data[data$PromoEnquete2012 == 2011 | data$PromoEnquete2012 == 2010,]
    fname = "../../Output/ensimag_2012_lieu.pdf"
    titre = "Lieu de travail des diplômés, par type d'activité, début 2O12, 6 et 18 mois"
    lapromo = data$PromoEnquete2012
    anneeE = 2012
  }


  lieu = data.frame(promo=as.factor(lapromo), lieudoc = data$LieuLaboDoctoratEnquete2012, lieuTravail = data$LieuTravailDetailEnquete2012, activites= data$ActiviteActuelle, anneeEnquete = data$AnneeEnquete)
  lieu = lieu[lieu$anneeEnquete == anneeE,]
  lieu$lieu[lieu$lieudoc != ""] = as.character(lieu$lieudoc[lieu$lieudoc != ""])
  lieu$lieu[lieu$lieuTravail != ""] = as.character(lieu$lieuTravail[lieu$lieuTravail != ""])
  lieu$lieu = as.factor(lieu$lieu)
  lieu$lieu = relevel(lieu$lieu, "Provence-Alpes-Côte d'Azur")
  lieu$lieu = relevel(lieu$lieu, "Etranger")
  lieu$lieu = relevel(lieu$lieu, "Rhône-Alpes")
  lieu$lieu = relevel(lieu$lieu, "Île-de-France")

  
  p = ggplot(data=lieu, aes(x=as.factor(promo), fill=lieu), colour=black)  + geom_bar() + facet_grid(promo ~ activites)
  p  + opts(title=titre) + xlab("Promo Ensimag") + scale_fill_hue(l=70, c=150)  + ylab("Nb de diplômés")
  ggsave(fname, width=2*par("din")[1])
}














