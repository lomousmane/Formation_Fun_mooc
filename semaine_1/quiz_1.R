setwd("C:/mes dossiers/Formation_fun_mooc_2024/données")
# importer les données
smp_2=read.csv2("smp2.csv")
# fournir le resultat avec na en faisant min
min(smp_2$age)
# cette commande ne fournit pas de résultats
str(smp_2)
smp_2$dep_cons<-factor(smp_2$dep.cons,levels = c(0,1),
                    labels =c("Non","Oui"))
str(smp_2)
smp_2$prof<-factor(smp_2$prof)
unique(smp_2$suicide.past)
# somme des valeurs de la variable
table(smp_2$separation)
# IQR de la variable dur.Inter
quantile(smp_2$dur.interv,na.rm=T)
# recoder fraterie
smp_2$n.fratrie<-factor(smp_2$n.fratrie>5,labels=c("<5",">5"))
# filter ecole (1,2,3)
df_ecole=subset(smp_2,ecole==1|ecole==2|ecole==3)
nrow(df_ecole) # nombre de ligne de école
# donnons le nombre d'individus sans emploi
table(smp_2$prof)
?factor
# Exo facultatifs
# les 10 premiers individus
mean(smp_2[1:10,"age"])
smp_2$age[1:10]
# commande pour valeur manquante
is.na(smp_2)
# les 300 premiers individus
median(smp_2[1:300,"dur.interv"],na.rm=T)
?cut
summary(smp_2$age)
smp_2$classe_age=cut(smp_2$age,breaks = c(19, 28, 37, 48, 83),include.lowest = TRUE)
View(smp_2)
table(smp_2$classe_age)
