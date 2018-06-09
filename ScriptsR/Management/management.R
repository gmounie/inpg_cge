## satisfaction sur la formation et la filière

source("../DataReader/dataReader.R")

library(ggplot2)

                                        # 2017

aRH = data.frame(type="Resp. Hiérarchiques", val=data2017$X64..EmploiResponsabilitesHierarchiques, filiere=data2017$X247..Option_ScolariteFiliereFormation)
aRB = data.frame(type="Resp. Budget", val=data2017$X65..EmploiResponsabiliteBudget, filiere=data2017$X247..Option_ScolariteFiliereFormation)
aAE = data.frame(type="Resp. Equipe", val=data2017$X66..EmploiResponsabiliteEquipe, filiere=data2017$X247..Option_ScolariteFiliereFormation)
aRP = data.frame(type="Resp. Projet", val=data2017$X67..EmploiResponsabiliteProjet, filiere=data2017$X247..Option_ScolariteFiliereFormation)
aAI = data.frame(type="Emploi Internationale", val=data2017$X69..EmploiInternational, filiere=data2017$X247..Option_ScolariteFiliereFormation)


b = rbind(aRH,aRB, aAE, aRP, aAI)
b = na.omit(b[b$val != "",])

levels(b$val)

str(b)
levels(b$filiere) = c("Non renseigné" , "Master" , "Master" , "IF" , "ISI" , "ISSC" , "Master" , "Master" , "Master" , "Master" , "MMIS" , "SLE")
b$filiere = factor(b$filiere)

ggplot(b,aes(val, fill=filiere)) + geom_bar(colour="white") + facet_wrap(c("type"))
ggsave("../../Output/ensimag_2017_management.png", width=1.5*par("din")[1])


                                        # 2016

aRH = data.frame(type="Resp. Hiérarchiques", val=data2016$ResponsabilitesHierarchiques, filiere=data2016$Option_FiliereFormation)
aRB = data.frame(type="Resp. Budget", val=data2016$ResponsabiliteBudget, filiere=data2016$Option_FiliereFormation)
aAE = data.frame(type="Anim. Equipe", val=data2016$AnimationEquipeSansHierarchie, filiere=data2016$Option_FiliereFormation)
aRP = data.frame(type="Resp. Projet", val=data2016$ResponsabiliteProjet, filiere=data2016$Option_FiliereFormation)
aC = data.frame(type="Consultant", val=data2016$Consultant, filiere=data2016$Option_FiliereFormation)
aAI = data.frame(type="Activ. Internationale", val=data2016$ActiviteInternationale, filiere=data2016$Option_FiliereFormation)


b = rbind(aRH,aRB, aAE, aRP, aC, aAI)
b = na.omit(b[b$val != "",])
str(b)
levels(b$filiere) = c( "Master" , "Master" , "IF" , "ISI" , "Tcom/ISSC" , "Master" , "Master" , "Master" , "MMIS" , "SLE" , "Tcom/ISSC")

ggplot(b,aes(val, fill=filiere)) + geom_bar(colour="white") + facet_wrap(c("type"))
ggsave("../../Output/ensimag_2016_management.png", width=1.5*par("din")[1])
