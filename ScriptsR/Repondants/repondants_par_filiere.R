source("../DataReader/dataReader.R")
library(ggplot2)
a = data.frame(Filiere=data2015$Option_FiliereFormation, FSN= data2015$Option_FiliereFormation)
levels(a$FSN) = c(1:10)
p = ggplot(a, aes(x=FSN,fill=Filiere)) + geom_bar(aes(show_guide=FALSE))+coord_flip()+ geom_text(label="FILIÈRE FINANCE décalé de 6 MOIS (sous-représenté: 2/3)!!!", color="red", x=4, y=100) + xlab("") + ylab("")
p
ggsave("../../Output/ensimag_2015_repondants_filiere.png", width=1.2*par("din")[1])