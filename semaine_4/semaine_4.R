setwd("C:/mes dossiers/Formation_fun_mooc_2024/données")
smp.2=read.csv2("smp2.csv")
smp.2
library(tidyverse)
ggplot(smp.2,aes(x=age,y=dur.interv))+
                  geom_point()+
                  geom_smooth(method="lm")
# créer le model
mod=lm(dur.interv~age,smp.2)
mod
# le résumé du model
summary(mod)
# test de correlation
cor.test(smp.2$age,smp.2$dur.interv)
# model 2 avec dep.cons comme variabl independante
mod_1=lm(dur.interv~dep.cons,smp.2)
summary(mod_1)
# test de comparaison des moyennes
t.test(dur.interv~dep.cons,smp.2,var.equal=T)

