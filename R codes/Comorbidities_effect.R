source('include_all.R')

morti=singoli_osp[singoli_osp$dead==1,13:36]
vivi=singoli_osp[singoli_osp$dead==0,13:36]

n1 <- dim(morti)[1]
n2 <- dim(vivi)[1]
p <- dim(vivi)[2]

x.mean1 <- sapply(morti, mean)
x.mean2 <- sapply(vivi, mean)

p.hat <- (x.mean1*n1+x.mean2*n2)/(n1+n2)
x.var <- (p.hat*(1-p.hat))

alpha=0.01
# Test: H0.i: mu.i1 == mu.i2  vs H1.i: mu.i1 != mu.i2

z.i <- (x.mean1-x.mean2)/sqrt(x.var*(1/n1+1/n2))
p.i <- ifelse(z.i<0, 2*pnorm(z.i),2*(1-pnorm(z.i)))

which(p.i<alpha)

# Bonferoni test
k=p

which(p.i*k<alpha)  

# or
p.Bf <- p.adjust(p.i, method='bonferroni')

which(p.Bf<alpha)  

# Benjamini-Hockberg (control the false discovery rate)  
p.BH <- p.adjust(p.i, method='BH')

which(p.BH<alpha)


x11(width=21, height=7)
par(mfrow=c(1,3))
plot(p.i, main='Univariate')
abline(h=alpha, lwd=2, col='red')

plot(p.Bf, main='Corrected - Bonferroni')
abline(h=alpha, lwd=2, col='red')

plot(p.BH, main='Corrected - BH')
abline(h=alpha, lwd=2, col='red')


#notiamo che la presenza di quasi tutte le malattie e di tutti i dispositivi medici hanno influenza significativa 
# sui pazienti deceduti e quelli no