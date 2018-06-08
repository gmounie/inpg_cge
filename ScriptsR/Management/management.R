## satisfaction sur la formation et la filière

source("../DataReader/dataReader.R")

library(ggplot2)

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
