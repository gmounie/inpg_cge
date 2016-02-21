data2016 = read.csv("../../../ObservatoirePourLEmploi.git/CGE2016/_ENSIMAG2016_610.csv", sep =",", header=T, dec=".")
data2015 = read.csv("../../../ObservatoirePourLEmploi.git/CGE2015/Ensimag2015-2015-04-01.csv", sep =",", header=T, dec=".")
data2014 = read.csv("../../../ObservatoirePourLEmploi.git/CGE2014/_ensimag2014_296.csv", sep =",", header=T, dec=".")
data2013 = read.csv("../../../ObservatoirePourLEmploi.git/CGE2013/_ensimag2013-filtree-18-avr-2013.csv", sep =";", header=T, dec=",")
data2012 = read.csv("../../../ObservatoirePourLEmploi.git/CGE2012/_cge2012-ensimag_final_26mars.csv", sep =";", header=T, dec=",")
data2011 = read.csv("../../../ObservatoirePourLEmploi.git/CGE2011/cge-ensimag-2011_SPHINX_7avril2011.csv", sep =";", header=T, dec=",");
data2010_2009 = read.csv("../../../ObservatoirePourLEmploi.git/CGE2010/CGE_2010_Grenoble_INP-_Ensimag_-_promotion_2009_UTF8.CSV", sep =",", header=T, dec=",")
data2010_2008 = read.csv("../../../ObservatoirePourLEmploi.git/CGE2010/CGE_2010_Grenoble_INP-_Ensimag_-_promotion_2008_UTF8.CSV", sep =",", header=T, dec=",")
data2009_2008 = read.csv("../../../ObservatoirePourLEmploi.git/CGE2009/CGE2009-20mars-ENSIMAG-Promo2008_utf8.CSV", sep =",", header=T, dec=",")


data2014 = data2014[data2014$AnneeEnquete == 2014,]
data2013 = data2013[data2013$AnneeEnquete == 2013,]
data2012 = data2012[data2012$AnneeEnquete == 2012,]
# grave typo dans 2011 noté "2 011", ne pas filtré (inutile en plus)
# data2011 = data2011[data2011$AnneeEnquete == 2011,] 


taille2015 = 274

# 2015 financiers entré l'année d'avant
taille2014 = 255

# attention promo sans la filère finance
taille2013 = 184


promo2012 = read.csv("../../../ObservatoirePourLEmploi.git/CGE2013/promo_2012.csv", sep=";", header=T)
promo2011 = read.csv("../../../ObservatoirePourLEmploi.git/CGE2013/promo_2011.csv", sep=";", header=T)
taille2011 = sum(promo2011$M, promo2011$F)
taille2012 = sum(promo2012$M, promo2012$F)
taille2010 = 242

promo6_2014 = taille2014
promo18_2013 = taille2013


promo6_2013 = taille2013
promo18_2012 = taille2012

promo6_2012 = taille2012
promo18_2011 = taille2011

promo6_2011 = taille2011
promo18_2010 = 242

promo6_2010 = 242
promo18_2009 = 239
  
promo6_2009 = 239
## attention promo sans telecom
promo18_2008 = 157

promo6_2008 = 157

