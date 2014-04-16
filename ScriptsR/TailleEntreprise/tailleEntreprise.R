source("../DataReader/dataReader.R")

library(ggplot2)
library(plyr)

taillesE = data2013$TailleEntrepriseV2010[data2013$AnneeEnquete == 2013]
a = data.frame(tailles = data2013$TailleEntrepriseV2010, poids= 1, situation=data2013$ActiviteActuelle)
a = a[a$tailles != "",]

a$tailles = relevel(a$tailles, "50 000 salarié(e)s et plus")
a$tailles = relevel(a$tailles, "De 10 000 à 49 999 salarié(e)s")
a$tailles = relevel(a$tailles, "De 5 000 à 9 999 salarié(e)s")
a$tailles = relevel(a$tailles, "De 2 000 à 4 999 salarié(e)s")
a$tailles = relevel(a$tailles, "De 500 à 1 999 salarié(e)s")
a$tailles = relevel(a$tailles, "De 250 à 499 salarié(e)s")
a$tailles = relevel(a$tailles, "De 100 à 249 salarié(e)s")
a$tailles = relevel(a$tailles, "De 50 à 99 salarié(e)s")
a$tailles = relevel(a$tailles, "De 20 à 49 salarié(e)s")
a$tailles = relevel(a$tailles, "De 10 à 19 salarié(e)s")
a$tailles = relevel(a$tailles, "Moins de 10 salarié(e)s")

levels(a$tailles)
length(a$tailles[a$tailles != ""])
ddply(a, .(tailles), summarize, nb=round(100*sum(poids)/274, digits=1))

## val = count(data2013)
## val2 = val
## val2$freq = val$freq / sum(val$freq)
## val3 = val2
## val3$agglosect = val3$SecteurActivite
#for(i in 1:length(val3$freq)) { if (val3$freq[i] < 0.1) val3$agglosect[i] = "Autres secteurs" }



#p = ggplot(val3, aes(x=factor(agglosect), weight=freq)) + geom_bar(fill="lightgreen", colour="darkgreen") + coord_flip() + opts(title="Secteurs d'activité") + xlab("") + ylab("Pourcentage") 

p = ggplot(a, aes(x=factor(tailles), weight=poids/(length(a$tailles)))) + geom_bar(fill="lightgreen", colour="darkgreen") + coord_flip() + opts(title="Tailles des entreprises") + xlab("") + ylab("Pourcentage") 


#p + geom_text(x=1, y=0.11, label="secteurs < 10%", size=16) + opts(plot.title = theme_text(size=32, lineheight=.8, face="bold"), axis.text.x = theme_text(size=28, lineheight=.8, face="bold"), axis.text.y = theme_text(size=28, lineheight=.8, face="bold"),  axis.title.x = theme_text(size=28, lineheight=.8)) 


#p + geom_text(x=1, y=0.11, label="15 secteurs < 10%")
ggsave("../../Output/ensimag_2013_tailles.svg", width=2*par("din")[1])

t12 = data.frame(taille = data2014$TailleEntrepriseV2010[data2014$AnneeDiplome == 2012],promo= 2012)
t11 = data.frame(taille = data2013$TailleEntrepriseV2010[data2013$AnneeDiplome == 2011],promo= 2011)
t10 = data.frame(taille = data2012$TailleEntrepriseV2010[data2012$AnneeDiplome == 2010],promo= 2010)

t = rbind(t12, t11, t10)
t$promo = factor(t$promo)
t = t[t$taille != "",]

levels(t$taille) = c(
""                              , "Plus de 5000 salarié(e)s"   , 
"Plus de 5000 salarié(e)s", "De 20 à 249 salarié(e)s"     , 
"De 10 à 19 salarié(e)s"        , "De 250 à 4999 salarié(e)s" , 
"De 20 à 249 salarié(e)s"        , "De 250 à 4999 salarié(e)s"     , 
"Plus de 5000 salarié(e)s"  , "De 250 à 4999 salarié(e)s"   , 
"De 20 à 249 salarié(e)s"        , "Moins de 10 salarié(e)s"      )

length(t$taille)
summary(t)











