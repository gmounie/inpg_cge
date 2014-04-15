source("../DataReader/dataReader.R")

library(ggplot2)
library(plyr)

pfe14 = data2014$EmploiEntreprisePFE[data2014$AnneeDiplome == 2013]
pfe13 = data2013$EmploiEntreprisePFE[data2013$AnneeDiplome == 2012]
pfe12 = data2012$EmploiEntreprisePFE[data2012$AnneeDiplome == 2011]

pfe = data.frame(pfe=factor(c(as.character(pfe14), as.character(pfe13), as.character(pfe12))))

summary(pfe)
qplot(pfe$pfe)

