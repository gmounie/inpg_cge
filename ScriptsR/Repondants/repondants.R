source("../DataReader/dataReader.R")

# enquete 2018
repondants6_2017 = length( data2018$X244..AnneeEnquete[ data2018$X244..AnneeEnquete == 2018 & data2018$X14..AnneeDiplomeVerifieParLeDiplome == 2017 & data2018$X20..ActiviteActuelle != ""])
repondants18_2016 = length( data2018$X244..AnneeEnquete[ data2018$X244..AnneeEnquete == 2018 & data2018$X14..AnneeDiplomeVerifieParLeDiplome == 2016 & data2018$X20..ActiviteActuelle != ""])
repondants30_2015 = length( data2018$X244..AnneeEnquete[ data2018$X244..AnneeEnquete == 2018 & data2018$X14..AnneeDiplomeVerifieParLeDiplome == 2015 & data2018$X20..ActiviteActuelle != ""])

print(repondants6_2017/taille2017)
print(repondants18_2016/taille2016)
print(repondants30_2015/taille2015)


# enquete 2017
repondants6_2016 = length( data2017$X1..AnneeEnquete[ data2017$X1..AnneeEnquete == 2017 & data2017$X21..AnneeDiplomeVerifieParLeDiplome == 2016 & data2017$X26..ActiviteActuelle != ""])
repondants18_2015 = length( data2017$X1..AnneeEnquete[ data2017$X1..AnneeEnquete == 2017 & data2017$X21..AnneeDiplomeVerifieParLeDiplome == 2015 & data2017$X26..ActiviteActuelle != ""])
repondants30_2014 = length( data2017$X1..AnneeEnquete[ data2017$X1..AnneeEnquete == 2017 & data2017$X21..AnneeDiplomeVerifieParLeDiplome == 2014 & data2017$X26..ActiviteActuelle != ""])

print(repondants6_2016/taille2016)
print(repondants18_2015/taille2015)
print(repondants30_2014/taille2014)


# enquete 2016

repondants6_2015 = length( data2016$AnneeEnquete[ data2016$AnneeEnquete == 2016 & data2016$AnneeDiplome == 2015 & data2016$ActiviteActuelle != ""])

repondants18_2014 = length( data2016$AnneeEnquete[ data2016$AnneeEnquete == 2016 & data2016$AnneeDiplome == 2014 & data2016$ActiviteActuelle != ""])
repondants30_2013 = length( data2016$AnneeEnquete[ data2016$AnneeEnquete == 2016 & data2016$AnneeDiplome == 2013 & data2016$ActiviteActuelle != ""])

print(repondants6_2015/taille2015)
print(repondants18_2014/taille2014)
print(repondants30_2013/taille2013)

# enquete 2015

repondants6_2014 = length( data2015$AnneeEnquete[ data2015$AnneeEnquete == 2015 & data2015$AnneeDiplome == 2014 & data2015$ActiviteActuelle != ""])

repondants18_2013 = length( data2015$AnneeEnquete[ data2015$AnneeEnquete == 2015 & data2015$AnneeDiplome == 2013 & data2015$ActiviteActuelle != ""])
repondants30_2012 = length( data2015$AnneeEnquete[ data2015$AnneeEnquete == 2015 & data2015$AnneeDiplome == 2012 & data2015$ActiviteActuelle != ""])

print(repondants6_2014/taille2014)
print(repondants18_2013/taille2013)
print(repondants30_2012/taille2012)


repondants6_2013 = length( data2014$AnneeEnquete[ data2014$AnneeEnquete == 2014 & data2014$AnneeDiplome == 2013 & data2014$ActiviteActuelle != ""])
repondants18_2012 = length( data2014$AnneeEnquete[ data2014$AnneeEnquete == 2014 & data2014$AnneeDiplome == 2012 & data2014$ActiviteActuelle != ""])


print(repondants6_2013/taille2013)
print(repondants18_2012/taille2012)
print((repondants6_2013+repondants18_2012)/(taille2012+taille2013))

repondants6_2012 = length( data2013$AnneeEnquete[ data2013$AnneeEnquete == 2013 & data2013$AnneeDiplome == 2012 & data2013$ActiviteActuelle != ""])
repondants18_2011 = length( data2013$AnneeEnquete[ data2013$AnneeEnquete == 2013 & data2013$AnneeDiplome == 2011 & data2013$ActiviteActuelle != ""])


print(repondants6_2012/taille2012)
print(repondants18_2011/taille2011)

repondants6_2011 = length( data2012$AnneeEnquete[ data2012$AnneeEnquete == 2012 & data2012$AnneeDiplome == 2011 & data2012$ActiviteActuelle != ""])
repondants18_2010 = length( data2012$AnneeEnquete[ data2012$AnneeEnquete == 2012 & data2012$AnneeDiplome == 2010 & data2012$ActiviteActuelle != ""])

repondants_CGE = data.frame(annee=factor(c(2012,2012,2013,2013,2014,2014,2015,2015,2015,2016,2016,2016,2017,2017,2017)), promo=factor(c(2011,2010, 2012,2011, 2013,2012,2014,2013,2012,2015,2014,2013,2016,2015,2014)), repondants= c(repondants6_2011, repondants18_2010, repondants6_2012, repondants18_2011, repondants6_2013, repondants18_2012, repondants6_2014, repondants18_2013, repondants30_2012, repondants6_2015, repondants18_2014, repondants30_2013,repondants6_2016,repondants18_2015,repondants30_2014), taillepromo = c(taille2011, taille2010, taille2012, taille2011, taille2013, taille2012, taille2014, taille2013, taille2012, taille2015, taille2014, taille2013,taille2016,taille2015,taille2014),sortie=c("6 mois","18 mois","6 mois","18 mois","6 mois","18 mois","6 mois","18 mois","30 mois","6 mois","18 mois","30 mois","6 mois","18 mois","30 mois"))

repondants_CGE$sortie = relevel(repondants_CGE$sortie,"30 mois")

print(repondants6_2011/taille2011)
print(repondants18_2010/taille2010)


filière6_2012 = data.frame(promo=as.factor(2012), filiere= data2013$FiliereFormation[ data2013$AnneeEnquete == 2013 & data2013$AnneeDiplome == 2012 & data2013$ActiviteActuelle != ""])
filière18_2011 =  data.frame(promo=as.factor(2011), filiere= data2013$FiliereFormation[ data2013$AnneeEnquete == 2013 & data2013$AnneeDiplome == 2011 & data2013$ActiviteActuelle != ""])

filière6_2012$filiere = relevel(filière6_2012$filiere,"Commun M2R Informatique")
filière6_2012$filiere = relevel(filière6_2012$filiere,"SLE (Filière Systèmes et logiciels embarqués, commun Phelma)")
filière6_2012$filiere = relevel(filière6_2012$filiere,"ISI (Filière Ingénierie des systèmes d'information)")

library(ggplot2)
p = ggplot(data=filière6_2012, aes(x=promo, fill=filiere)) + geom_bar() + scale_fill_hue()
p

p = ggplot(data = repondants_CGE, aes(x=promo, fill=promo, weight=100*repondants/taillepromo)) + geom_bar()
#+ scale_fill_manual(values=c("blue","blue","blue","blue"))
p +  ggtitle("Pourcentage de répondants, taux de réponses des promotions à 6, 18 et 30 mois après la sortie") + xlab("Promo Ensimag") + ylab("Pourcentage")  + facet_grid( ~ sortie) + coord_flip(ylim=c(0,100))
ggsave("../../Output/repondants17.png", width=2*par("din")[1])















