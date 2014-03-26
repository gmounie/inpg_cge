source("../DataReader/dataReader.R")

repondants6_2013 = length( data2014$AnneeEnquete[ data2014$AnneeEnquete == 2014 & data2014$AnneeDiplome == 2013 & data2014$ActiviteActuelle != ""])
repondants18_2012 = length( data2014$AnneeEnquete[ data2014$AnneeEnquete == 2014 & data2014$AnneeDiplome == 2012 & data2014$ActiviteActuelle != ""])


print(repondants6_2013/taille2013)
print(repondants18_2012/taille2012)


repondants6_2012 = length( data2013$AnneeEnquete[ data2013$AnneeEnquete == 2013 & data2013$AnneeDiplome == 2012 & data2013$ActiviteActuelle != ""])
repondants18_2011 = length( data2013$AnneeEnquete[ data2013$AnneeEnquete == 2013 & data2013$AnneeDiplome == 2011 & data2013$ActiviteActuelle != ""])


print(repondants6_2012/taille2012)
print(repondants18_2011/taille2011)

repondants_CGE13 = data.frame(annee=2013, promo=c(2012, 2011),
  repondants= c(repondants6_2012, repondants18_2011), taillepromo = c(taille2012, taille2011))

filière6_2012 = data.frame(promo=as.factor(2012), filiere= data2013$FiliereFormation[ data2013$AnneeEnquete == 2013 & data2013$AnneeDiplome == 2012 & data2013$ActiviteActuelle != ""])
filière18_2011 =  data.frame(promo=as.factor(2011), filiere= data2013$FiliereFormation[ data2013$AnneeEnquete == 2013 & data2013$AnneeDiplome == 2011 & data2013$ActiviteActuelle != ""])

filière6_2012$filiere = relevel(filière6_2012$filiere,"Commun M2R Informatique")
filière6_2012$filiere = relevel(filière6_2012$filiere,"SLE (Filière Systèmes et logiciels embarqués, commun Phelma)")
filière6_2012$filiere = relevel(filière6_2012$filiere,"ISI (Filière Ingénierie des systèmes d'information)")

library(ggplot2)
p = ggplot(data=filière6_2012, aes(x=promo, fill=filiere)) + geom_bar() + scale_fill_hue()
p

p = ggplot(data = repondants_CGE13, aes(x=as.factor(promo), fill=as.factor(promo), weight=100*repondants/taillepromo)) + geom_bar()+ scale_fill_manual(values=c("blue","blue"))
p +  opts(title="Pourcentage de répondants, début 2O13, promo à 6 et 18 mois") + xlab("Promo Ensimag") + ylab("Pourcentage")  + facet_grid(. ~ annee) + coord_flip(ylim=c(0,100))
ggsave("../../Output/repondants.svg")















