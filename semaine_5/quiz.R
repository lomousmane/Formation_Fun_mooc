setwd("C:/mes dossiers/Formation_fun_mooc_2024/données")
alc=read.csv2("alcool.csv")
str(alc)
# recode la variable age
alc$new_age<-factor(ifelse(alc$AGE>=50,"1","0"))
# Comparaison de 2 groupes avec le test de log rank
? survdiff
surv_diff=survdiff(Surv(t,SEVRE)~new_age,data=alc)
# interaction entre sexe et age

mod<-coxph(Surv(t,SEVRE)~AGE+SEXE+AGE*SEXE,data=alc)
mod
