## competences

## difficile à représenter ?
## juste citer les mots clefs présentés avec des histogrammes ?

source("../DataReader/dataReader.R")

library(stringr)
library(plyr)
library(ggplot2)
a = str_split(data2013$ActivitesTechniquesFonctionINPG,";")
filiere = data2013$FiliereFormation
a = a[a[] != ""]
filiere = filiere[data2013$ActivitesTechniquesFonctionINPG != ""]
taille=length(a[a[] != ""])
b = unlist(a)
c = as.factor(str_replace_all(b, "^ | $", ""))
i=1
filiereDUP=c()
for(line in a) { for(nb in seq(1, length(line))) { filiereDUP = c(filiereDUP, as.character(filiere[i]))}; i=i+1  }
str(filiereDUP)
data = data.frame(compétence=c, poids=1/taille, filiere=as.factor(filiereDUP))
p = ggplot(data, aes(x=compétence, weight=poids,fill=filiere))+ geom_bar() + coord_flip() + opts(title="Compétences techniques") + xlab("") + ylab("Pourcentage") 
p 
ggsave("../../Output/ensimag_2013_competence.png", width=3*par("din")[1])

write.csv(data2013$ActivitesTechniquesFonctionINPG, file="compet13.csv", row.names= F, quote=F)
write.csv(data2014$ActivitesTechniquesFonctionINPG, file="compet14.csv", row.names= F, quote=F)















