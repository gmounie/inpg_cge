data2013 = read.csv("../CGE2013/_ensimag2013-24-fev-2013.csv", sep =";", header=T)
data2012 = read.csv("../CGE2012/_cge2012-ensimag_final_26mars.csv", sep =";", header=T)



taux_emplois6 =data.frame(promo=c(2011, 2012), taux=c(85,86),taux_max = c(95,97), taux_min = c(80,80))

library(ggplot2)
p = ggplot(taux_emplois6, aes(x=promo,ymin=taux_min, ymax=taux_max, y=taux))
p + geom_ribbon() + ylim(0,100) +labs(title="Taux net d'emploi à 6 mois", y="pourcentage")
