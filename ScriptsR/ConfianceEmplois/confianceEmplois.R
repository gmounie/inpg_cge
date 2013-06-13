source("script_data.R")


repondants6_2012 = length( data2013$AnneeEnquete[ data2013$AnneeEnquete == 2013 & data2013$AnneeDiplome == 2012 & data2013$ActiviteActuelle != ""])
repondants18_2011 = length( data2013$AnneeEnquete[ data2013$AnneeEnquete == 2013 & data2013$AnneeDiplome == 2011 & data2013$ActiviteActuelle != ""])

repondants6_2011 = length( data2012$AnneeEnquete[ data2012$AnneeEnquete == 2012 & data2012$AnneeDiplome == 2011 & data2012$ActiviteActuelle != ""])
repondants18_2010 = length( data2012$AnneeEnquete[ data2012$AnneeEnquete == 2012 & data2012$AnneeDiplome == 2010 & data2012$ActiviteActuelle != ""])

enrecherche6_2012 = length( data2013$AnneeEnquete[ data2013$AnneeEnquete == 2013 & data2013$AnneeDiplome == 2012 & data2013$ActiviteActuelle != "" & data2013$ActiviteActuelle == "En recherche d'emploi"])
enrecherche18_2011 = length( data2013$AnneeEnquete[ data2013$AnneeEnquete == 2013 & data2013$AnneeDiplome == 2011 & data2013$ActiviteActuelle != "" & data2013$ActiviteActuelle == "En recherche d'emploi"])

enrecherche6_2011 = length( data2012$AnneeEnquete[ data2012$AnneeEnquete == 2012 & data2012$AnneeDiplome == 2011 & data2012$ActiviteActuelle != "" & data2012$ActiviteActuelle == "En recherche d'emploi"])
enrecherche18_2010 = length( data2012$AnneeEnquete[ data2012$AnneeEnquete == 2012 & data2012$AnneeDiplome == 2010 & data2012$ActiviteActuelle != "" & data2012$ActiviteActuelle == "En recherche d'emploi"])

repondants6_2010 = 184
enrecherche6_2010 = 7

repondants18_2009 = 160
enrecherche18_2009 = 4

repondants6_2009 = 154
enrecherche6_2009 = 7


brutconfint = function (promo, repondants, x) {
  nbmin= x
  proba = data.frame( xval= nbmin:(promo-repondants+nbmin), proba=-1)
  
  for(nbcour in nbmin:(promo-repondants+nbmin)) {
    proba$proba[proba$xval == nbcour] = choose(nbcour,x)*choose(promo-nbcour,repondants - x) / choose(promo,repondants)
  }
  
  xmin = min(proba[proba$proba >= 0.05,1])
  xmax = max(proba[proba$proba >= 0.05,1])
  
  return(data.frame( promo=promo, repondants=repondants, x=nbmin, xmin=xmin, xmax=xmax, pmoyen = 100*x/repondants, pmin=100*xmin/promo, pmax=100*xmax/promo ))
}

library(epitools)
afficheIC = function(annee, promo, repondants, x) {
  cf = brutconfint(promo,repondants, x)
  print(paste(annee,": IC recherche emploi entre ",100*cf$xmin/promo,"% et ",100*cf$xmax/promo,"%"))
  print(binom.exact(x, repondants))
}

afficheIC("6 mois recherche 2012", promo6_2012,repondants6_2012,enrecherche6_2012);
afficheIC("prev 6 mois recherche 2011", promo6_2011,repondants6_2011,enrecherche6_2011);
afficheIC("prev 6 mois recherche 2010", promo6_2010,repondants6_2010,enrecherche6_2010);
afficheIC("18mois recherche 2011", promo18_2011,repondants18_2011,enrecherche18_2011)
afficheIC("prev 18mois recherche 2010", promo18_2010,repondants18_2010,enrecherche18_2010)
afficheIC("prev 18mois recherche 2009", promo18_2009,repondants18_2009,enrecherche18_2009)


cf6_2012 = brutconfint(promo6_2012,repondants6_2012,enrecherche6_2012)
cf6_2011 = brutconfint(promo6_2011,repondants6_2011,enrecherche6_2011)
cf6_2010 = brutconfint(promo6_2010,repondants6_2010,enrecherche6_2010)

cf18_2011 = brutconfint(promo18_2011,repondants18_2011,enrecherche18_2011)
cf18_2010 = brutconfint(promo18_2010,repondants18_2010,enrecherche18_2010)
cf18_2009 = brutconfint(promo18_2009,repondants18_2009,enrecherche18_2009)

cf6 = data.frame(year = as.integer(c(2012,2011,2010)),
  pmoyen = c(cf6_2012$pmoyen, cf6_2011$pmoyen, cf6_2010$pmoyen),
  pmin = c(cf6_2012$pmin, cf6_2011$pmin, cf6_2010$pmin),
  pmax = c(cf6_2012$pmax, cf6_2011$pmax, cf6_2010$pmax))

cf18 = data.frame(year = as.integer(c(2011,2010, 2009)),
  pmoyen = c(cf18_2011$pmoyen, cf18_2010$pmoyen, cf18_2009$pmoyen),
  pmin = c(cf18_2011$pmin, cf18_2010$pmin, cf18_2009$pmin),
  pmax = c(cf18_2011$pmax, cf18_2010$pmax, cf18_2009$pmax))


library(ggplot2)
 ## + coord_cartesian(ylim=c(0,100))
p = ggplot(cf6, aes(x=year, y=pmoyen, ymin=pmin, ymax=pmax)) 
## p = p + geom_ribbon(aes(ymin=pmin, ymax=pmax, alpha=0.5)) + xlab("Promo") + ylab("Pourcentage")
## p = p + geom_line(colour="blue",size=2)
p = p  + geom_errorbar(colour="blue") + geom_point(size=3)
p + ggtitle("Pourcentage de recherche d'emploi à 6 mois (Intervalle de confiance à 95%)") + coord_cartesian(ylim=c(0,10))
ggsave("ensimag_itchomeur_6mois.svg")
p = ggplot(cf18, aes(x=year, y=pmoyen, ymin=pmin, ymax=pmax))
## p = p + geom_ribbon(aes(ymin=pmin, ymax=pmax, alpha=0.5)) + xlab("Promo") + ylab("Pourcentage")
## p = p + geom_line(colour="blue",size=2)
p = p  + geom_errorbar(colour="blue") + geom_point(size=3)
p + ggtitle("Pourcentage de recherche d'emploi à 18 mois  (Intervalle de confiance à 95%)") + coord_cartesian(ylim=c(0,10))
ggsave("ensimag_itchomeur_18mois.svg")
