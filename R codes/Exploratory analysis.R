source('include_all.R')

#Istogram of ages with overline boxplot
x11()
dev.off()
layout(mat = matrix(c(1,2),2,1, byrow=TRUE), height = c(1,8))
# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(singoli_osp$eta_Min , horizontal=TRUE, main="Patient age at first ospedalization", xaxt="n" , frame=F)
par(mar=c(4, 3.1, 1.1, 2.1))
hist(singoli_osp$eta_Min, breaks = 40, main="" , xlab="Age") #  border=F , 

# quantile(singoli_osp$eta_Min) # 18   71   79   85  110 years
# mean(singoli_osp$eta_Min)  # 77.16838

#Istogram of ages with overline boxplot and colours by group
dev.off()
x11()
layout(mat = matrix(c(1,2),2,1, byrow=TRUE), height = c(1,8))
# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(singoli_osp$eta_Min , horizontal=TRUE, main="Patient age at first ospedalization", xaxt="n" , frame=F)
my_hist = hist(singoli_osp$eta_Min, col="white", breaks = 40, main="" , xlab="Age", plot=F) #  border=F , 
my_color= ifelse (my_hist$breaks < 60, rgb(0,1,0,0.5) , ifelse (my_hist$breaks >= 80, "purple", rgb(0.2,0.2,0.2,0.2) ))
par(mar=c(4, 3.1, 1.1, 2.1))
plot(my_hist, col=my_color , main="" , xlab="Age" )

# Boxplot sesso vs eta
# par(mar=c(5.1, 4.1, 4.1, 2.1))
# par(mfrow = c(1,1))
dev.off()
boxplot(singoli_osp$eta_Min ~ singoli_osp$SESSO, col = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), xlab = "Gender", ylab="First ospedalization age")

#Boxplot sesso vs num_comor
# par(mar=c(5.1, 4.1, 4.1, 2.1))
# par(mfrow = c(1,1))
boxplot(singoli_osp$num_comor ~ singoli_osp$SESSO, col = c("pink", "deepskyblue1"))



#Histograms ages by sesso
dev.off()
males = singoli_osp[singoli_osp$SESSO == "M",]$eta_Min
n_male = length(males)    # 92249
females = singoli_osp[singoli_osp$SESSO == "F",]$eta_Min
n_female = length(females)  # 95244
hist(females,  breaks = 40, col=rgb(1,0,0,0.5), xlab="Age", 
     ylab="Frequency", main="Distribution of ages by sex" )

# Second with add=T to plot on top
hist(males,  breaks = 40, col=rgb(0,0,1,0.5), add=T)

# Add legend
legend("topright", legend=c("Females","Males"), col=c(rgb(1,0,0,0.5), 
                                                      rgb(0,0,1,0.5)), pt.cex=2, pch=15 )



#Multiple boxplots ospedalization by age

a = singoli_osp[singoli_osp$eta_Min <= 40,]$ospitalizations
b = singoli_osp[singoli_osp$eta_Min > 40 & singoli_osp$eta_Min <= 60,]$ospitalizations
c = singoli_osp[singoli_osp$eta_Min > 60 & singoli_osp$eta_Min <= 70,]$ospitalizations
d = singoli_osp[singoli_osp$eta_Min > 70 & singoli_osp$eta_Min <= 80,]$ospitalizations
e = singoli_osp[singoli_osp$eta_Min > 80 & singoli_osp$eta_Min <= 90,]$ospitalizations
f = singoli_osp[singoli_osp$eta_Min > 90,]$ospitalizations

ve = list(a,b,c, d, e, f)

x.mean=rbind(c(0,0,0,0,0,0), c(0,0,0,0,0,0))

for (i in 1:6) {
  x.mean[1,i]=sapply(ve[i],mean)
  x.mean[2,i]=sapply(ve[i],sd)
}

T2 = cbind(c(0,0,0,0,0,0), c(0,0,0,0,0,0), c(0,0,0,0,0,0))

for (i in 1:6) {
  T2[i,1] = x.mean[1,i] - x.mean[2,i]
  T2[i,2] = x.mean[1,i] 
  T2[i,3] = x.mean[1,i] + x.mean[2,i]
}
aa = mean(a)
bb = mean(b)
cc = mean(c)
dd = mean(d)
ee = mean(e)
ff = mean(f)

aa_cov = var(a)
bb_cov = var(b)
cc_cov = var(c)
dd_cov = var(d)
ee_cov = var(e)
ff_cov = var(f)

dev.off()
x11()
matplot(1:6,1:6,pch='',ylim=c(0,7), xlab='Ages',ylab='Ospedalization',
        main='Numer of ospedalizations by age range') 
for(i in 1:6){
  points(1:6, T2[,2], pch=16, col=1:6)
  segments(i,T2[i,1],i,T2[i,3],lwd=3,col=i) 
}
