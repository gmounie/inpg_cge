## nature des postes: comme pour les secteurs ?

source("../DataReader/dataReader.R")

library(ggplot2)
library(plyr)
                                        # 2017
methode = data2017$X76..EmploiCommentTrouve

a = data.frame(methode = methode, poids= 1, filiere=data2017$X247..Option_ScolariteFiliereFormation)
levels(a$filiere) = c("Non renseigné" , "Master" , "Master" , "IF" , "ISI" , "ISSC" , "Master" , "Master" , "Master" , "Master" , "MMIS" , "SLE")
a$filiere = factor(a$filiere)

levels(a$methode)

p = ggplot(a, aes(x=factor(methode), weight=poids/(length(a$methode)),fill=filiere)) + geom_bar(colour="white") + coord_flip() + theme(title=element_text("Nature des postes")) + xlab("") + ylab("Pourcentage") 
p
ggsave("../../Output/ensimag_2017_methode.svg", width=2*par("din")[1])
ggsave("../../Output/ensimag_2017_methode.png", width=2*par("din")[1])


                                        # 2016
methode = data2016$CommentTrouveEmploiV2015

a = data.frame(methode = data2016$CommentTrouveEmploiV2015, poids= 1, filiere=data2016$Option_FiliereFormation)

levels(a$filiere) = c( "Master" , "Master" , "IF" , "ISI" , "Tcom/ISSC" , "Master" , "Master" , "Master" , "MMIS" , "SLE" , "Tcom/ISSC")
levels(a$methode)

p = ggplot(a, aes(x=factor(methode), weight=poids/(length(a$methode)),fill=filiere)) + geom_bar(colour="white") + coord_flip() + theme(title=element_text("Nature des postes")) + xlab("") + ylab("Pourcentage") 
p
ggsave("../../Output/ensimag_2016_methode.svg", width=2*par("din")[1])
ggsave("../../Output/ensimag_2016_methode.png", width=2*par("din")[1])





