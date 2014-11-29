source("../DataReader/dataReader.R")

library(ggplot2)

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
} else {
    data = data2014
    data = data[data$Promo == 2013 | data$Promo == 2012,]
    fname = "../../Output/ensimag_2014_lieu.pdf"
    fnameF = "../../Output/ensimag_2014_lieu_filiere.pdf"
    titre = "Lieu de travail des diplômés, par type d'activité, début 2O14, 6 et 18 mois"
    lapromo = data$Promo
    lafiliere= data$FiliereFormation
    anneeE = 2014
    
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














