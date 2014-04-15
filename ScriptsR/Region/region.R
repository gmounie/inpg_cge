source("../DataReader/dataReader.R")

library(ggplot2)
library(plyr)


## Durée de la recherche d'emploi
# l'info n'est pas obligatoire pour les 2014, peu remplit !

r14 = data.frame(lieu = data2014$LieuTravailDetailEnquete2012, labo= data2014$LieuLaboDoctoratEnquete2012, promo=  data2014$AnneeDiplome)
r13 = data.frame(lieu = data2013$LieuTravailDetailEnquete2012, labo= data2013$LieuLaboDoctoratEnquete2012, promo=  data2013$AnneeDiplome)
r12 = data.frame(lieu = data2012$LieuTravailDetailEnquete2012, labo= data2012$LieuLaboDoctoratEnquete2012, promo=  data2012$AnneeDiplome)

r = rbind(r14[r14$promo == 2012,], r13[r13$promo == 2011,] ,r12[r12$promo == 2010,])
r$region = paste(as.character(r$lieu),as.character(r$labo),sep="")


r$region = factor(r$region)
r$promo = factor(r$promo)
r= r[r$lieu != "" || r$labo != "",]
r= r[r$promo != "NA",]

length(r$region)
summary(r)


ggplot(r, aes(x=promo, fill=region)) + geom_bar() + xlab("Année enquête") + ylab("Nombre de diplômés")
ggsave("../../Output/ensimag_duree_recherche.png", width=2*par("din")[1])


## avec la moyenne et la médiane ? boxplot ?

r6 = rbind(r14[r14$promo == 2013,], r13[r13$promo == 2012,] ,r12[r12$promo == 2011,])
r6$region = paste(as.character(r6$lieu),as.character(r6$labo),sep="")


r6$region = factor(r6$region)
r6$promo = factor(r6$promo)
r6= r6[r6$lieu != "" || r6$labo != "",]
r6= r6[r6$promo != "NA",]

length(r6$region)
summary(r6)









