# importer les données
library(readr)
library(tidyverse)
library(prettyR)
smp_1=read.csv2("C:/mes dossiers/Formation_fun_mooc_2024/données/smp1.csv",header=T)
smp_1
# structure des données
str(smp_1)
# dimension
dim(smp_1)
# le nom des colonnes
names(smp_1)
# Analyse descriptives des données
describe(smp_1)
# Calculer le nombre de detenu ayant u metier
tab_prof=table(smp_1$prof)
# un diagramme en bar
pie(tab_prof,col=rainbow(10),main="Distribution de la variable profession")
ggplot(smp_1,aes(x=prof,fill=prof))+
                  geom_bar()+
                  coord_flip()
# Histogramme de l'age

ggplot(smp_1,aes(x=age))+
                  geom_histogram(fill="blue",binwidth = 5)+
                  ggtitle("Diagramme en Histogramme de l'age")
# Diagramme en boxplot
ggplot(smp_1,aes(y=age))+
                  geom_boxplot()+
                  ggtitle("Diagramme en boxplot de l'age")
# Nuage de point
plot(smp_1$age,smp_1$n.enfant,xlab="age",ylab="enfant",
     main="nuage de point")
savehistory("commande.R")
hist(smp_1$age)
lines(density(smp_1$age,na.rm = T),col="red")
# Continuer le code pour faire la visualisation
# with ggplot

