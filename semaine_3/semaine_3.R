setwd("C:/mes dossiers/Formation_fun_mooc_2024/données")
smp_1=read.csv2("smp1.csv")
str(smp_1)
smp_1$ed.b<-ifelse(smp_1$ed>2,1,0)
# test de Chi deux
tab=table(smp_1$ed.b,smp_1$dep.cons,
          deparse.level =2)
tab
chisq.test(tab,correct = F)
# verifier la correlation
cor(smp_1$age,smp_1$rs,use="complete.obs")
?cor
# test de nullité de pearson
cor.test(smp_1$age,smp_1$rs,use="complete.obs",
         method = "pearson")
# Test de comparaison d'une moyennne par rapport à mu (refrence)
t.test(smp_1$age,mu=24,conf.level = 0.95)

