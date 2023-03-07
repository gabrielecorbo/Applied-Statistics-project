source('include_all.R')

###### Number of Re-hospitalizations ######
############ 1
hist(milano$ric1)

hist(milano$ric1, breaks = 100, xlim = c(0, quantile(reduced_time$V2, 0.99)))
curve(dpois(x, lambda = 1), from = 0, col = "red", add = TRUE)

qqplot(qpois((1:10000/10000-0.5/10000),1), milano$ric1, col='red', xlab='Theoretical quantile', ylab='Sample Quantile', asp=1)
abline(0, 1, col='blue')

poi_mod1 = glm( ric1 ~ (cluster + num_comor)^2 + eta_Min + SESSO, data=milano[which(milano$time_study>364),], family = 'poisson')
summary(poi_mod1)

colori =rainbow(length(unique(milano$strutt_id[1:1000])))
boxplot(poi_mod1$residuals[1:1000] ~ milano$strutt_id[1:1000], col=colori,
        xlab='Hospitals', ylab='Residuals', main ='Distribution of residuals across hospitals') 

############ 1
lmm1 = glmer(ric1 ~ (cluster + num_comor)^2 + eta_Min + SESSO + (1|strutt_id), 
             data = milano[which(milano$time_study>364),] , family = 'poisson')
summary(lmm1)

sigma2_eps <- as.numeric(get_variance_residual(lmm1))
sigma2_b <- as.numeric(get_variance_random(lmm1))

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE  

x11()
dotplot(ranef(lmm1))  

# 1) Assessing Assumption on the within-group errors
x11()
plot(lmm1)  

x11()
qqnorm(resid(lmm1))
qqline(resid(lmm1), col='red', lwd=2)    

# 2) Assessing Assumption on the Random Effects
x11()
qqnorm(unlist(ranef(lmm1)$strutt_id), main='Normal Q-Q Plot - Random Effects for Hospitals')
qqline(unlist(ranef(lmm1)$strutt_id), col='red', lwd=2)   

# 3)
tab_model(lmm1)

ranef(lmm1)$strutt_id
ranef(lmm1)$strutt_id[which(ranef(lmm1)$strutt_id > 104),1]
# 030DJQ00

###################### 3
hist(milano$ric3)

qqplot(qpois((1:1000/1000-0.5/1000),1.6), milano$ric3, col='red', xlab='Theoretical quantile', ylab='Sample Quantile', asp=1)
abline(0, 1, col='blue')

poi_mod3 = glm( ric3 ~ (cluster + num_comor)^2 + eta_Min + SESSO, data=milano[which(milano$time_study>1094),], family = 'poisson')
summary(poi_mod3)

x11()
par(mfrow=c(2,2))
plot(poi_mod3) 

colori =rainbow(length(unique(milano$strutt_id[1:1000])))
boxplot(poi_mod3$residuals[1:1000] ~ milano$strutt_id[1:1000], col=colori,
        xlab='Hospitals', ylab='Residuals', main ='Distribution of residuals across hospitals') 

############ 3
lmm3 = glmer(ric3 ~ (cluster + num_comor)^2 + eta_Min + SESSO + (1|strutt_id), 
             data = milano[which(milano$time_study>1094),] , family = 'poisson')
summary(lmm3)

sigma2_eps <- as.numeric(get_variance_residual(lmm3))
sigma2_b <- as.numeric(get_variance_random(lmm3))

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE  

x11()
dotplot(ranef(lmm3))  

# 1) Assessing Assumption on the within-group errors
x11()
plot(lmm3)  

x11()
qqnorm(resid(lmm3))
qqline(resid(lmm3), col='red', lwd=2)    

# 2) Assessing Assumption on the Random Effects
x11()
qqnorm(unlist(ranef(lmm3)$strutt_id), main='Normal Q-Q Plot - Random Effects for Hospitals')
qqline(unlist(ranef(lmm3)$strutt_id), col='red', lwd=2)    

# 3)
tab_model(lmm3)

ranef(lmm3)$strutt_id
ranef(lmm3)$strutt_id[which(ranef(lmm3)$strutt_id > 1.46),1]
# 042IMR00

###################### 5
hist(milano$ric5)

qqplot(qpois((1:1000/1000-0.5/1000),1), milano$ric5, col='red', xlab='Theoretical quantile', ylab='Sample Quantile', asp=1)
abline(0, 1, col='blue')

poi_mod5 = glm( ric5 ~ (cluster + num_comor)^2 + eta_Min + SESSO, data=milano[which(milano$time_study>1824),], family = 'poisson')
summary(poi_mod5)

x11()
par(mfrow=c(2,2))
plot(poi_mod5) 

colori =rainbow(length(unique(milano$strutt_id[1:1000])))
boxplot(poi_mod5$residuals[1:1000] ~ milano$strutt_id[1:1000], col=colori,
        xlab='Hospitals', ylab='Residuals', main ='Distribution of residuals across Hospitals') 

############ 5
lmm5 = glmer(ric5 ~ (cluster + num_comor)^2 + eta_Min + SESSO + (1|strutt_id), 
             data = milano[which(milano$time_study>1824),] , family = 'poisson')
summary(lmm5)

sigma2_eps <- as.numeric(get_variance_residual(lmm5))
sigma2_b <- as.numeric(get_variance_random(lmm5))

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE  

x11()
dotplot(ranef(lmm5))  

# 1) Assessing Assumption on the within-group errors
x11()
plot(lmm5)  

x11()
qqnorm(resid(lmm5))
qqline(resid(lmm5), col='red', lwd=2)    

# 2) Assessing Assumption on the Random Effects
x11()
qqnorm(unlist(ranef(lmm5)$strutt_id), main='Normal Q-Q Plot - Random Effects for Hospitals')
qqline(unlist(ranef(lmm5)$strutt_id), col='red', lwd=2)    

# 3)
tab_model(lmm5)

ranef(lmm5)$strutt_id
ranef(lmm5)$strutt_id[which(ranef(lmm5)$strutt_id > 1.6),1]
# 030INS00
