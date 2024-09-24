library(readr)
library(prettyR)
# importation des données
smp_1=read.csv2("C:/mes dossiers/Formation_fun_mooc_2024/données/smp1.csv")
str(smp_1)
# plot
plot(jitter(smp_1$age),jitter(smp_1$n.enfant),xlab="age",
     ylab="n.enfant")
# claclculer la correlation
cor(smp_1$age,smp_1$n.enfant,use="complete.obs")
# calcul des OR (odd ration) et RR (risques relatifs)
smp_1$ed.b<-ifelse(smp_1$ed>2,1,0)
str(smp_1)
tab=table(smp_1$ed.b,smp_1$ed,useNA = "always",deparse.level = 2)
names(tab)<-c("ed.b","ed")
# calcul OR et RR avec twoby2
library(Epi)
twoby2(1-smp_1$ed.b,1-smp_1$dep.cons)
