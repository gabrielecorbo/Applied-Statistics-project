source('include_all.R')

milano <- read.table('milano.txt', header=T)
milano_logistica=milano[,1:43]
milano_logistica$anno1 = 0
milano_logistica$anno3 = 0
milano_logistica$anno5 = 0

# see if patient died in 1,3,5 years
milano_logistica[which(milano_logistica$labelOUT == "DECEDUTO"),]$anno1 = ifelse(as.Date(milano_logistica[which(milano_logistica$labelOUT == "DECEDUTO"),5])-as.Date(milano_logistica[which(milano_logistica$labelOUT == "DECEDUTO"),2])>364,0,1)
milano_logistica[which(milano_logistica$labelOUT == "DECEDUTO"),]$anno3 = ifelse(as.Date(milano_logistica[which(milano_logistica$labelOUT == "DECEDUTO"),5])-as.Date(milano_logistica[which(milano_logistica$labelOUT == "DECEDUTO"),2])>1094,0,1)
milano_logistica[which(milano_logistica$labelOUT == "DECEDUTO"),]$anno5 = ifelse(as.Date(milano_logistica[which(milano_logistica$labelOUT == "DECEDUTO"),5])-as.Date(milano_logistica[which(milano_logistica$labelOUT == "DECEDUTO"),2])>1824,0,1)

# keep all patient died or alive after 5 years of study
milano_logistica$tmp = 0
milano_logistica[which(milano_logistica$labelOUT == "DECEDUTO"),]$tmp = 1
milano_logistica[as.Date(milano_logistica[,5])-as.Date(milano_logistica[,2])>364,]$tmp = 1
df_aspettativa = milano_logistica[milano_logistica$tmp == 1,]
df_aspettativa$tmp = NULL

#Anno 1
id_test = c(3,11,33:36,43:44)
colnames(df_aspettativa)[id_test]
# id_samp = sample(1:nrow(df_aspettativa), size = 50000)
df_test = df_aspettativa[,id_test]
df_test$cluster=as.factor(df_test$cluster)
fit1 <- glm(anno1 ~ ., data= df_test, family='binomial')
summary(fit1)

## Analisi modello ## 
library(ROCR)
p <- predict(fit1, df_test, type='response')
pr <- prediction(p, df_test$anno1)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
x11()
plot(prf)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc  # 0.7369479  -->0.7650424

for (trsh in (1:10)/10){
  predictions = ifelse(p > trsh,1,0)
  errorsq = (df_test$anno1 != predictions)
  APER   = sum(errorsq)/length(predictions)
  print(trsh) # 0.5
  print(APER) # 0.2938179
}

trsh = 0.5
predictions = ifelse(p > trsh,1,0)
table(class.true=df_test$anno1, class.assigned=predictions)
#         class.assigned
# class.true     0     1
#          0 9065 1346
#          1 3440 2438
errorsq = (df_test$anno1 != predictions)
APER   = sum(errorsq)/length(predictions)
print(APER) # 0.3195408  __>0.2938179

Z0.new <- data.frame(cluster = as.factor(2), num_comor = 3, eta_Min = 40, SESSO = 'F',ICD=0,SHOCK=0,CABG=0,PTCA=0) # questo ? il nuovo dato
Z1.new <- data.frame(cluster = as.factor(3), num_comor = 3, eta_Min = 40, SESSO = 'F',ICD=0,SHOCK=0,CABG=0,PTCA=0) # questo ? il nuovo dato
p0 <- predict(fit1, Z0.new, type='response')
p0
p1 <- predict(fit1, Z1.new, type='response')
p1

#Anno 3
id_test = c(3,11,43,45)
colnames(milano_logistica)[id_test]
# id_samp = sample(1:nrow(df_aspettativa), size = 50000)
df_test = milano_logistica[,id_test]
df_test$cluster=as.factor(df_test$cluster)
fit3 <- glm(anno3 ~ ., data= df_test, family='binomial')
summary(fit3)

## Analisi modello ## 
library(ROCR)
p <- predict(fit3, df_test, type='response')
pr <- prediction(p, df_test$anno3)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc  # 0.7760562    -->0.7454778

for (trsh in (1:10)/10){
  predictions = ifelse(p > trsh,1,0)
  errorsq = (df_test$anno3 != predictions)
  APER   = sum(errorsq)/length(predictions)
  print(trsh) # 0.3-->0.5
  print(APER) # 0.2518628-->0.3157345
}

trsh = 0.5
predictions = ifelse(p > trsh,1,0)
table(class.true=df_test$anno3, class.assigned=predictions)
#         class.assigned
# class.true     0     1
#          0 13882 20090
#          1  7222 67246

errorsq = (df_test$anno3 != predictions)
APER   = sum(errorsq)/length(predictions)
print(APER) # 0.2518628-->0.3157345


#Anno 5
id_test = c(3,11,43,46)
colnames(milano_logistica)[id_test]
# id_samp = sample(1:nrow(df_aspettativa), size = 50000)
df_test = milano_logistica[,id_test]
df_test$cluster=as.factor(df_test$cluster)
fit5 <- glm(anno5 ~ ., data= df_test, family='binomial')
summary(fit5)

## Analisi modello ## 
library(ROCR)
p <- predict(fit5, df_test, type='response')
pr <- prediction(p, df_test$anno5)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc  # 0.8121018


for (trsh in (1:10)/10){
  predictions = ifelse(p > trsh,1,0)
  errorsq = (df_test$anno5 != predictions)
  APER   = sum(errorsq)/length(predictions)
  print(trsh) # 0.2
  print(APER) # 0.1896717
}

trsh = 0.2
predictions = ifelse(p > trsh,1,0)
table(class.true=df_test$anno5, class.assigned=predictions)
#         class.assigned
# class.true     0     1
#          0  6440 17448
#          1  3120 81432


id_test = c(3,11,12,33:36,43:44)
colnames(df_aspettativa)[id_test]
# id_samp = sample(1:nrow(df_aspettativa), size = 50000)
df_test = df_aspettativa[,id_test]
df_test$cluster=as.factor(df_test$cluster)
log_m_model <- glmer(anno1 ~ eta_Min + SESSO +ICD+SHOCK+CABG+PTCA+ cluster+ (1|strutt_id) , data = df_test, nAGQ=0,
                     family = "binomial")

summary(log_m_model)
# vif(log_m_model)
sigma2_eps <- as.numeric(get_variance_residual(log_m_model))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(log_m_model))
sigma2_b

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE
x11()
dotplot(ranef(log_m_model))

sort(ranef(log_m_model)$strutt_id[,1])
ranef(log_m_model)$strutt_id[which(ranef(log_m_model)$strutt_id > 0.5),1]
which(ranef(log_m_model)>0.5)

p <- predict(log_m_model, df_test, type='response')
pr <- prediction(p, df_test$anno1)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc  # 0.8121018


for (trsh in (1:10)/10){
  predictions = ifelse(p > trsh,1,0)
  errorsq = (df_test$anno1 != predictions)
  APER   = sum(errorsq)/length(predictions)
  print(trsh) # 0.5
  print(APER) # 0.2669374
}

trsh = 0.5
predictions = ifelse(p > trsh,1,0)
table(class.true=df_test$anno1, class.assigned=predictions)

Z0.new.best <- data.frame(cluster = as.factor(2), num_comor = 3, eta_Min = 40, SESSO = 'F',ICD=0,SHOCK=0,CABG=0,PTCA=0,strutt_id='030DJS00') # questo ? il nuovo dato
Z0.new.worst <- data.frame(cluster = as.factor(2), num_comor = 3, eta_Min = 40, SESSO = 'F',ICD=0,SHOCK=0,CABG=0,PTCA=0,strutt_id='030EKU00') # questo ? il nuovo dato
Z1.new.best <- data.frame(cluster = as.factor(3), num_comor = 3, eta_Min = 40, SESSO = 'F',ICD=0,SHOCK=0,CABG=0,PTCA=0,strutt_id='030DJS00') # questo ? il nuovo dato
Z1.new.worst <- data.frame(cluster = as.factor(3), num_comor = 3, eta_Min = 40, SESSO = 'F',ICD=0,SHOCK=0,CABG=0,PTCA=0,strutt_id='030EKU00') # questo ? il nuovo dato
p0 <- predict(log_m_model, Z0.new.best, type='response')  #0.005076995  
p0
p0 <- predict(log_m_model, Z0.new.worst, type='response')  #0.01836582 
p0
p1 <- predict(log_m_model, Z1.new.best, type='response')  #0.05910724 
p1
p1 <- predict(log_m_model, Z1.new.worst, type='response') #0.1872076 
p1
