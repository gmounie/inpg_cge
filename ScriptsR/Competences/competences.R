## competences

## difficile à représenter ?
## juste citer les mots clefs présentés avec des histogrammes ?

source("../DataReader/dataReader.R")

library(stringr)
library(plyr)
library(ggplot2)
a = str_split(data2013$ActivitesTechniquesFonctionINPG,";")
a = a[a[] != ""]
taille=length(a[a[] != ""])
b = unlist(a)
c = as.factor(str_replace_all(b, "^ | $", ""))
data = data.frame(compétence=c, poids=1/taille)
p = ggplot(data, aes(x=compétence, weight=poids))+ geom_bar(fill="lightgreen", colour="darkgreen") + coord_flip() + opts(title="Compétences techniques") + xlab("") + ylab("Pourcentage") 
p 
ggsave("../../Output/competence.pdf", width=2*par("din")[1])
