## competences

## difficile à représenter ?
## juste citer les mots clefs présentés avec des histogrammes ?

source("../DataReader/dataReader.R")

write.csv(data2013$ActivitesTechniquesFonctionINPG, file="compet13.csv", row.names= F, quote=F)
write.csv(data2014$ActivitesTechniquesFonctionINPG, file="compet14.csv", row.names= F, quote=F)
