source("../DataReader/dataReader.R")
library(ggplot2)

# 2019
a = data.frame(Filiere=data2019$X264..Option_ScolariteFiliereFormation, FSN= data2019$X264..Option_ScolariteFiliereFormation)
levels(a$Filiere) = c("NA", "IF – Ingénierie pour la Finance", "ISI – Ingénierie des Systèmes d'Information", "ISSC –Internet, Services et Systèmes Connectés", "Master", "MMIS – Modélisation, Mathématique, Images, Simulation", "SEOC - Systèmes Embarqués et Objets Connectés", "SLE – Systèmes et Logiciels Embarqués")

levels(a$FSN) = levels(a$Filiere)

#levels(a$FSN) = c(1,2,2,2,3,4,1,1,1,5,6,4)
p = ggplot(a, aes(x=FSN,fill=Filiere)) + geom_bar(colour="white",aes(show_guide=FALSE))+coord_flip() + xlab("") + ylab("")
p
ggsave("../../Output/ensimag_2019_repondants_filiere.png", width=2*par("din")[1])

# 2018
a = data.frame(Filiere=data2018$X258..Option_ScolariteFiliereFormation, FSN= data2018$X258..Option_ScolariteFiliereFormation)
levels(a$Filiere) = c("NA", "IF – ingénierie pour la finance" , "ISI – ingénierie des systèmes d’information" , "ISSC" , "Master" , "Master Msiam" , "Master" , "MMIS" , "SLE")
levels(a$FSN) = c("NA", "IF – ingénierie pour la finance" , "ISI – ingénierie des systèmes d’information" , "ISSC" , "Master" , "Master Msiam" , "Master" , "MMIS" , "SLE")


#levels(a$FSN) = c(1,2,2,2,3,4,1,1,1,5,6,4)
p = ggplot(a, aes(x=FSN,fill=Filiere)) + geom_bar(colour="white",aes(show_guide=FALSE))+coord_flip() + xlab("") + ylab("")
p
ggsave("../../Output/ensimag_2018_repondants_filiere.png", width=2*par("din")[1])


# 2017
a = data.frame(Filiere=data2017$X247..Option_ScolariteFiliereFormation, FSN= data2017$X247..Option_ScolariteFiliereFormation)
levels(a$Filiere) = c("Non renseigné" , "Master" , "Master" , "IF" , "ISI" , "ISSC" , "Master" , "Master" , "Master" , "Master" , "MMIS" , "SLE")
levels(a$FSN) = c("Non renseigné" , "Master" , "Master" , "IF" , "ISI" , "ISSC" , "Master" , "Master" , "Master" , "Master" , "MMIS" , "SLE")

#levels(a$FSN) = c(1,2,2,2,3,4,1,1,1,5,6,4)
p = ggplot(a, aes(x=FSN,fill=Filiere)) + geom_bar(colour="white",aes(show_guide=FALSE))+coord_flip()+ geom_text(label="FILIÈRE FINANCE décalé de 1 an (sous-représenté, car répond moins)!!!", color="red", x=2, y=100) + xlab("") + ylab("")
p
ggsave("../../Output/ensimag_2017_repondants_filiere.png", width=2*par("din")[1])

# 2016
a = data.frame(Filiere=data2016$Option_FiliereFormation, FSN= data2016$Option_FiliereFormation)
levels(a$Filiere) = c( "Master" , "Master" , "IF" , "ISI" , "Tcom/ISSC" , "Master" , "Master" , "Master" , "MMIS" , "SLE" , "Tcom/ISSC")
levels(a$FSN) = c(1,1,2,3,4,1,1,1,5,6,4)
p = ggplot(a, aes(x=FSN,fill=Filiere)) + geom_bar(colour="white",aes(show_guide=FALSE))+coord_flip()+ geom_text(label="FILIÈRE FINANCE décalé de 1 an (sous-représenté)!!!", color="red", x=2, y=100) + xlab("") + ylab("")
p
ggsave("../../Output/ensimag_2016_repondants_filiere.png", width=2*par("din")[1])


# 2015
a = data.frame(Filiere=data2015$Option_FiliereFormation, FSN= data2015$Option_FiliereFormation)
levels(a$FSN) = c(1:10)
p = ggplot(a, aes(x=FSN,fill=Filiere)) + geom_bar(aes(show_guide=FALSE))+coord_flip()+ geom_text(label="FILIÈRE FINANCE décalé de 6 MOIS (sous-représenté: 2/3)!!!", color="red", x=4, y=100) + xlab("") + ylab("")
p
ggsave("../../Output/ensimag_2015_repondants_filiere.png", width=1.2*par("din")[1])








