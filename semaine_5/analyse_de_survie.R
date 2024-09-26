setwd("C:/mes dossiers/Formation_fun_mooc_2024/données")
alc=read.csv2("alcool.csv")
# visualiser les 1er lignes
head(alc)
# structure
str(alc)
# Résumé des données
summary(alc)
# Installer librairie 
install.packages("survival")
library(survival)
plot(survfit(Surv(alc$t,alc$SEVRE)~1),
     main="courbe d'abstinence des patients",
     lw=2,col=c("blue"))
# Comparaison de 2 groupes avec le test de log rank
? survdiff
surv_diff=survdiff(Surv(t,SEVRE)~SEXE,data=alc)
## le délai de survie est non significatif en fonction du sexe
# Le risque de survie avec une variable quantitative age
cox<-coxph(Surv(t,SEVRE)~AGE,data=alc)
cox
# Tester avec plusieurs variables explicatives
mod<-coxph(Surv(t,SEVRE)~AGE+SEXE+EDVNEG,data=alc)
mod
exp(mod$coefficients)
# Visualiser le modéle
par(mfrow=c(2,2))
plot(cox.zph(mod))
