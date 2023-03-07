source('include_all.R')

###### Functional Analysis ######
library(fda)
ospxmon <- read.table('ospxmon_func.txt', header=T)
x11()
matplot(t(ospxmon),type='l')

data_W <- t(ospxmon)
abscissa <- 1:84

# nbasis <- 6:100
# gcv <- numeric(length(nbasis))
# for (i in 1:length(nbasis)){
#   basis <- create.bspline.basis(rangeval=c(0,365), nbasis[i],m)
#   gcv[i] <- smooth.basis(abscissa, data_W, basis)$gcv
# }
# x11()
# par(mfrow=c(1,1))
# plot(nbasis,gcv)
# nbase=nbasis[which.min(gcv)]   #14
# abline(v = nbasis[which.min(gcv)], col = 2)

#basis <- create.bspline.basis(rangeval=c(0,365), nbase,m)

m=3
nbase=14
basis.1 <- create.bspline.basis(rangeval=c(0,84),nbasis=nbase,m)
data_W.fd.1 <- Data2fd(y = data_W,argvals = abscissa,basisobj = basis.1)
x11()
plot.fd(data_W.fd.1)

pca_W.1 <- pca.fd(data_W.fd.1,nharm=5,centerfns=TRUE)

pca_W.1$values[1:5]
# 1868.863549  279.044590   27.987432    3.556465    1.482923

# scree plot
# pca.fd computes all the 365 eigenvalues, but only the first 
# N-1=131 are non-null
x11()
plot(pca_W.1$values[1:14],xlab='j',ylab='Eigenvalues')
plot(cumsum(pca_W.1$values)[1:14]/sum(pca_W.1$values),xlab='j',ylab='CPV')

cumsum(pca_W.1$values)[1:5]/sum(pca_W.1$values)
# first three FPCs
x11()
par(mfrow = c(1,3))
plot(pca_W.1$harmonics[1,],col=1,ylab='FPC1')
abline(h=0,lty=2)
plot(pca_W.1$harmonics[2,],col=2,ylab='FPC2')
abline(h=0,lty=2)
plot(pca_W.1$harmonics[3,],col=2,ylab='FPC3')


x11()
plot(pca_W.1$harmonics[1,],col=1,ylab='FPC1')
abline(v=c(12,24,36,48,60,72),lty=2)


x11()
par(mfrow=c(1,2))
plot.pca.fd(pca_W.1, nx=100, pointplot=TRUE, harm=c(1,2), expand=0, cycle=FALSE)

x11()
plot(pca_W.1$scores[,1],pca_W.1$scores[,2], pch = 16, xlab = 'PC1', ylab = 'PC2')
