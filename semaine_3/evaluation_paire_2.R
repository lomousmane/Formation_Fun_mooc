setwd("C:/mes dossiers/Formation_fun_mooc_2024/données")
library(readr)
satisfaction_hopital <- read_csv2("satisfaction_hopital.csv")
str(satisfaction_hopital)
unique(satisfaction_hopital$recommander)
# 1. Transformez la variable « recommander » en une variable binaire « recommander.b » :
satisfaction_hopital$recommander.b<-ifelse(satisfaction_hopital$recommander>1,1,0)
# 2. A l’aide d’un odds-ratio, estimez la force de l’association entre « recommander.b » et « sexe ».
## Estimez un intervalle de confiance de cet odds-ratio.
tab=table(satisfaction_hopital$recommander.b,
          satisfaction_hopital$sexe)
tab
fisher.test(tab,conf.level = 0.95)
# 3. Calculez la corrélation (de Pearson) entre « score.relation » et « age ». Testez statistiquement cette corrélation (le script doit inclure la vérification éventuelle 
# des conditions de validité de la méthode utilisée).
## Vérifions les conditions: La linéarité des variables
plot(satisfaction_hopital$age,
     satisfaction_hopital$score.relation,xlab="age",
     ylab="score.relation",
     main="Nuage de point entre age\n score.relation")
## La relation est faiblement  linéaire entre les 2 variables
# Calculons la correlation de pearson
cor(satisfaction_hopital$age,
    satisfaction_hopital$score.relation,use="complete.obs")
##Le coefficient de correlation est faiblement positif entre les variables
# testons la significativité de la relation
cor.test(satisfaction_hopital$age,
         satisfaction_hopital$score.relation,
         method = "pearson")
## Le pvalue >0.05 donc la correlation entre les deux variables 
## n'est pas significative on accepte l'hypothèse H0.
# 4. La moyenne du score de relation est-il significativement différent chez les hommes et chez les femmes ?
satisfaction_hopital$sexe<-factor(satisfaction_hopital$sexe,
                                  levels=c(0,1),labels=c("hommes","femmes"))
# verifier la normalité de la variable age avec le test de Shapiro
shapiro.test(satisfaction_hopital$score.relation)
## le pvalue <0.05 donc la distribution de la variable n'est normale
# on verifie avec l'histogramme de distribution
hist(satisfaction_hopital$score.relation)
## La distribution n'est pas en forme de courbe de cloche donc 
## la distribution ne suit pas la normalité
# on verifie l'égalité des variances
var.test(score.relation~sexe,satisfaction_hopital)
## On a utilisé le test d'égalité des variances avec var.test
## le pvalue>0.05 montre qu'on accepte H0 donc il ya égalité des variances entre les hommes et femmes 
# Test de comparaison  des moyennes avec le test de student
t.test(score.relation~sexe,satisfaction_hopital,var.equal=F)
# le pvalue >0.05 donc on accepte l'hypothèse H0 pas de
## de différence significative entre la moyenne de score entre les sexes.

