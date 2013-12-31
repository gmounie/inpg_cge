## satisfaction sur la formation et la filière

source("../DataReader/dataReader.R")

library(ggplot2)

data2013$RessentiFormationINPG = relevel(data2013$RessentiFormationINPG, "Très satisfaisante")
data2013$RessentiFormationINPG = relevel(data2013$RessentiFormationINPG, "Satisfaisante")
data2013$RessentiFormationINPG = relevel(data2013$RessentiFormationINPG, "")
data2013$RessentiFormationINPG = relevel(data2013$RessentiFormationINPG, "Insatisfaisante")
data2013$RessentiFormationINPG = relevel(data2013$RessentiFormationINPG, "Très insatisfaisante")

p = ggplot(data2013, aes(x= SatisfactionEmploiActuel, y=RessentiFormationINPG, color=FiliereFormation)) + geom_point() + geom_jitter(width=.2,height=0.25) + scale_color_brewer(palette="Set1") + labs("Qualité de l'emploi (de 1 à 5) versus le ressenti de la formation") + xlab("Qualité de l'emploi (de 1 à 5)") + ylab("Ressenti de la formation")
p
ggsave("../../Output/satisfaction.svg", width=2*par("din")[1])
