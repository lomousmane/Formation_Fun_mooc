library(readr)
# importation des données
setwd("C:/mes dossiers/Formation_fun_mooc_2024/données")
satisfaction_hopital <- read_csv2("satisfaction_hopital.csv")
View(satisfaction_hopital)
# structure
str(satisfaction_hopital)
dim(satisfaction_hopital)
# les trois variables catégorielles (sexe,service,prof)
## Présentons les pourcentage sexe
tab=table(satisfaction_hopital$sexe)
tab
round(prop.table(tab),3)
## Présentons les pourcentage service
tab=table(satisfaction_hopital$service)
tab
round(prop.table(tab),3)
## Présentons les pourcentage prof
tab=table(satisfaction_hopital$profession)
tab
round(prop.table(tab),3)
# Resumes des variables numériques
install.packages("prettyR")
library(prettyR)
describe(satisfaction_hopital[c("age","amelioration.sante",
                                  "amelioration.moral","recommander","score.relation",
                                "score,information")],num.desc = c("min","max","mean" ,"median","sd","valid.n"))

# Faites un histogramme du score de relation (score.relation).
hist(satisfaction_hopital$score.relation, xlab="score relation
    ",ylab="frequence",main="Histogramme score relation",col="blue")
# boxplot la distribution du score de relation chez les hommes et les femmes.
boxplot(satisfaction_hopital$score.relation~satisfaction_hopital$sexe,xlab="sexe",ylab="score relation",
        ,main="Diagramme en boxplot",col=rainbow(10))

