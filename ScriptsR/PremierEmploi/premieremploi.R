## premier emploi par promo

source("../DataReader/dataReader.R")

library(ggplot2)

                                        # 2016
a = data.frame(premierEmploi=data2016$EmploiPrecedent1er, promo=data2016$AnneeDiplome, filiere=data2016$Option_FiliereFormation)


levels(a$filiere) = c( "Master" , "Master" , "IF" , "ISI" , "Tcom/ISSC" , "Master" , "Master" , "Master" , "MMIS" , "SLE" , "Tcom/ISSC")

ggplot(a,aes(premierEmploi, fill=filiere)) + geom_bar(colour="white") + facet_wrap(c("promo"))
ggsave("../../Output/ensimag_2016_premieremploi.png", width=1.5*par("din")[1])

