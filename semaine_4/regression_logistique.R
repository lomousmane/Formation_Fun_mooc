setwd("C:/mes dossiers/Formation_fun_mooc_2024/données")
smp_1=read.csv2("smp2.csv")
str(smp_1)
names(smp_1)
# regression logistique simple
mod_1=glm(suicide.hr~abus,data=smp_1,family="binomial")
summary(mod_1) # le coeffiecent abus=0.768 non interprétable 
# odd ration
OR=exp(mod_1$coefficients[2]) 
#abus=2.15 signifie que les antécédents abus à l'enfant multiplie
#par 2 la probabilité du risque suicidaire
# on utilis la fonction twoby2 pour calculer Odd ration
library(Epi)
twoby2(1-smp_1$suicide.hr,1-smp_1$abus)
# regression logistique multiple
model=glm(suicide.hr~abus+discip+duree,smp_1,family="binomial")
# resume model
summary(model)
# predictes
predict(model)
# les odd ration
exp(model$coefficients)
# avec la variable prof
smp_1$prof<-relevel(factor(smp_1$prof),ref="agriculteur")
model=glm(suicide.hr~abus+prof+duree,smp_1,family="binomial")
summary(model)
# effet de la variable
drop1(model,.~.,test="Chisq")
