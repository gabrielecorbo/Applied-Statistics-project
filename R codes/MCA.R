source('include_all.R')

for (i in 1:20){
  only_cat[,i] <- factor(only_cat[,i])
}

x11()
res.mca=MCA(only_cat, ncp = 5, ind.sup = NULL, quanti.sup = NULL,quali.sup =NULL,
            excl=NULL, graph = TRUE,level.ventil = 0, axes = c(1,2), row.w = NULL,
            method="Indicator", na.method="NA", tab.disj=NULL)
summary(res.mca)