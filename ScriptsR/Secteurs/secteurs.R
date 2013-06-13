source("script_data.R")

library(ggplot2)

secteurs = data2013$SecteurActiviteFinale[data2013$AnneeEnquete == 2013]
count(secteurs)

val = count(data)
val2 = val
val2$freq = val$freq / sum(val$freq)
val3 = val2
val3$agglosect = val3$SecteurActiviteINPG
for(i in 1:length(val3$freq)) { if (val3$freq[i] < 0.1) val3$agglosect[i] = "Autres secteurs" }



p = ggplot(val3, aes(x=factor(agglosect), weight=freq)) + geom_bar(fill="lightgreen", colour="darkgreen") + coord_flip() + opts(title="Secteurs d'activité") + xlab("") + ylab("Pourcentage") 
## png("secteurs2012.png", 1600, 1200)
p + geom_text(x=1, y=0.11, label="15 secteurs < 10%", size=16) + opts(plot.title = theme_text(size=32, lineheight=.8, face="bold"), axis.text.x = theme_text(size=28, lineheight=.8, face="bold"), axis.text.y = theme_text(size=28, lineheight=.8, face="bold"),  axis.title.x = theme_text(size=28, lineheight=.8)) 
## dev.off()
## svg("secteurs2012.svg")
p + geom_text(x=1, y=0.11, label="15 secteurs < 10%")
## dev.off()
