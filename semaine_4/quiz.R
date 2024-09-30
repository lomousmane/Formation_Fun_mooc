setwd("C:/mes dossiers/Formation_fun_mooc_2024/données")
smp=read.csv2("smp2.csv")
smp
str(smp)
unique(smp$prof)
# anova
mod=lm(age~prof+n.enfant ,data=smp)
summary(mod)
anova(mod)
df_age=subset(smp,prof%in%c("sans emploi","cadre","ouvrier","employe"      
                                      ))
 View(df_age)
tapply(df_age$age,df_age$prof,sd)
# modélisation
model=glm(separation~age,smp,family=binomial("logit"))
summary(model)
predict(model)
library(Epi)
exp(model$coefficients)
# recodons la variable
smp$n.fratrie_class <- cut(smp$n.fratrie, breaks = c(0, 2, 4, Inf), labels = c("0-2", "3-4", "5+"))
View(smp)
# anova
model_anova=lm(age~n.fratrie_class,smp)
summary(model_anova)
#anova(model_anova)
drop1(model_anova,.~.,test="F")
# Questions facultatives
model=lm(dur.interv~age, data=smp)
summary(model)
confint(model,level=0.90)
# regression logistique
df_log=subset(smp,dep.cons==1)
View(df_log)
model_glm=glm(suicide.hr~age,family="binomial",smp)
predict(model_glm,data.frame(age=35))
