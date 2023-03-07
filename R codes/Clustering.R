source('include_all.R')

singoli_osp3=singoli_osp2[which(singoli_osp2$COD_REG %in% id_Same_osp),]
# prendo quelli che non cambiano mai ospedale
milano=singoli_osp3[which(singoli_osp3$ASL_RESIDENZA=='308'),]
mil=milano[,13:32]

mil.m <- dist(as.matrix(mil), method='minkowski')
mil.mc <- hclust(mil.m, method='ward.D2')
plot(mil.mc, main='euclidian-ward.D2', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(mil.mc, k=6)
cluster.mc <- cutree(mil.mc, k=6)
table(cluster.mc)
cl_table=table(cluster.mc)

coph.mc <- cophenetic(mil.mc) # compute the cophenetic matrices
mc <- cor(mil.m, coph.mc) # cophenetic coefficients, se vicino a |1| ? meglio
