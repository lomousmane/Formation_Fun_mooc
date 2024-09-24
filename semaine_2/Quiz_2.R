setwd("C:/mes dossiers/Formation_fun_mooc_2024/données")
smp_2=read.csv2("smp2.csv")
str(smp_2)
View(smp_2)
# which
which(smp_2$prof=="agriculteur")
# La distribution d'une variable
barplot(table(smp_2$prof),main="la distribution de la
        variable prof",col=rainbow(10),xlab="Profession",
        ylab="Proportion")
# subseting
subset(smp_2,prof=="agriculteur")

# le nombre moyen d'enfant moyen chez les individus dep.cons=1
df_dep_cons=subset(smp_2,dep.cons==1)
mean(df_dep_cons$n.enfant,na.rm = T)
# calcul IQR pour la variable durée chez les individus dont age<35 ans
df_age=subset(smp_2,age<35)
quantile(df_age$duree,na.rm = T,probs=c(0.25,0.50,0.75))
# calcul smp suicide
mean(smp_2[smp_2$suicide.past==1,"dur.interv"],na.rm=T)


# exo faculctatifs
# histogramme de lage avec 6 classes
hist(smp_2$age,nclass=6,main="histogramme age",xlab="age")
is.na(smp_2[c(20, 221, 342, 446, 531),])
# proportion dindividus sans emploi
df_emploi=subset(smp_2,prof=="sans emploi")
tab=table(df_emploi$prof)
prop.table(table(smp_2$prof)*100)
# age >20 et age<=30
df_age=subset(smp_2,age>=20 & age<=30)
nrow(df_age)
