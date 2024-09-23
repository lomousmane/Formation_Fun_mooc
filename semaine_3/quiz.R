setwd("C:/mes dossiers/Formation_fun_mooc_2024/données")
smp_2=read.csv2("smp2.csv")
# structure
str(smp_2)
# afficher total proportion par ligne
tab=table(smp_2$ed,smp_2$dep.cons)
prop.table(tab)
?prop.table
# calcul la moyenne de chaque groupe
with(smp_2,tapply(age,prof,mean,na.rm=TRUE))
# test de wilkox
?wilcox.test
t.test(dur.interv~dep.cons,smp_2)
# correlation entre duree interw et age
cor.test(x=smp_2$age,y=smp_2$dur.interv,
    method="pearson",use="complete.obs",conf.level = 0.95)
?cor
# a durée d'interview (dur.interv) diffère sensiblement selon que les individus ont déjà effectué une tentative de suicide dans le passé
#ou non (suicide.past) à l'aide d'un test de Wilcoxon. Le degré de significativité du test est :
wilcox.test(dur.interv~suicide.past,smp_2)

tapply(smp_2$age,list(smp_2$prof),mean)
# boxplot
boxplot(age~prof,data=smp_2)
# Test de student var egal=T
t.test(dur.interv~dep.cons,smp_2,var.equal=TRUE)
# test de Fisher pour afficher odd ration
tab=table(smp_2$dep.cons,smp_2$suicide.past)
tab
fisher.test(tab)
