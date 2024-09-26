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
# install factominer et factoextra
library(factoextra)
library(FactoMineR)
library(missMDA)
# Compute PCA with ncp = 3
nb <- estim_ncpPCA(smp[,var],ncp.min=0,ncp.max=5,method.cv="Kfold",nbsim=50)
imputed <- imputePCA(smp[,var],ncp=nb$ncp)
res.pca <- PCA(imputed$completeObs)
# hierarchical clustering principal component
res_cah<-HCPC(res.pca,graph = F)
plot(res_cah)
library(ggpubr)
fviz_cluster(res_cah,
             repel = TRUE, # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco", # Color palette see ?
             #ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)

