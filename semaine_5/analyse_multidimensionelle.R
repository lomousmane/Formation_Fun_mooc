setwd("C:/mes dossiers/Formation_fun_mooc_2024/données")
smp=read.csv2("smp2.csv")
# structure des données
str(smp)
list(colnames(smp))
var<-c("age","n.enfant","dep.cons","scz.cons",
       "grav.cons","rs","ed","dr")
# matrice de correlation
cor<-round(cor(smp[,var],use="pairwise"),digits=2)
# visualiser avec corrplot
#install.packages("corrplot")
library(corrplot)
corrplot(cor,method="circle")
# ACP
#install.packages("psy")
library(psy)
# Projection de  circulaire
psy::mdspca(smp[,var])
# projection sphérique
psy::sphpca(smp[,var],v=55)
# Relation entre variable expliquer et explicatives
expliquer<-"grav.cons"
explicatives<-c("age","n.enfant","dep.cons","scz.cons"
                     ,"rs","ed","dr")
? fpca
fpca(y=expliquer,x=explicatives,data=smp,partial = "No")
