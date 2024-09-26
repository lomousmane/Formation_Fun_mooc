setwd("C:/mes dossiers/Formation_fun_mooc_2024/données")
# Classification hiérarchique 
smp=read.csv2("smp2.csv")
# structure des données
str(smp)
list(colnames(smp))
var<-c("age","n.enfant","dep.cons","scz.cons",
       "grav.cons","rs","ed","dr")
# t=classification hiérarchique 
cah=hclust(dist(t(scale(smp[,var]))),method="ward.D2")
cah
plot(cah,main="cluster dendogramme")
