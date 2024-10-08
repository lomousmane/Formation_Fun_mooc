setwd("C:/mes dossiers/Formation_fun_mooc_2024/données")
satisfaction_hopital <- read.csv2("satisfaction_hopital.csv")
str(satisfaction_hopital)
# selection la référence pour la variable prof
satisfaction_hopital$profession=relevel(factor(satisfaction_hopital$profession),ref="4")
model<-lm(score.relation~age+sexe+score.information+
                            profession+amelioration.sante+
                            amelioration.moral+service,data=satisfaction_hopital)
model
summary(model)
"
On remarque que la variable age, sexe, score. information et 
amélioration.moral ont un effet significatif sur la variable 
score.relation
"
attributes(model)
# On verifie la validité du modéle
"La linéarité entre les variable"
pairs(satisfaction_hopital[,c("score.relation","age",
                              "sexe","score.information",
                              "service","profession",
                              "amelioration.sante","amelioration.moral")])
"On remarque que la relation entre 
les variables n'est pas linéaire car nous avons des variables
discrétes et continues"
"La normalité des résidus"
hist(model$residuals,xlab="residus",main="distribution 
     des résidus" ,col="green")
# verifions la relation entre les prédicteurs et les erreurs
plot(model$residuals, model$fitted.values,)
" On remarque une absence de corrélation entre 
les valeurs prédites et les erreurs"
# La regression logistique
satisfaction_hopital$recommander.b<-ifelse(
                  satisfaction_hopital$recommander>1,1,0
)
table(satisfaction_hopital$recommander.b)
mod_log<-glm(recommander.b~age+sexe+score.information+
                            profession+amelioration.sante+
                            amelioration.moral+service,data=satisfaction_hopital
           , family=binomial)
summary(mod_log)
