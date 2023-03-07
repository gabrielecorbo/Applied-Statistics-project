source('include_all.R')

###### Time between hospitalizations ######
time_btw_osp = read.table('time_btw_osp2.txt', header = T)
time_btw_osp$num_com_pre = rowSums(time_btw_osp[,12:31])
time_btw_osp$delta_num_com = rowSums(time_btw_osp[,36:55])
time_btw_osp$CountTot = rowSums(time_btw_osp[,9:11])
#Creo reduced_time, dataset time_btw_osp in cui tengo:
# COD_REG : codice paziente 
# V2: giorni passati dall'ospedalizzazione precedente (risposta che studio!)
# SESSO
# qt_prest_Sum :         numero di giorni in ospedale nella ospedalizzazione precedente
# eta_Min :              et? del paziente all'osp precedente (non ? la minima perch? cambia!!!!)
# strutt_id :            ospedale dove ? stato ricoverato la volta precedente
# CountA,CountB,CountC : numero di medicine prese (per tipo A,B,C) ?
# CountTot :             somma di CountA,CountB,CountC
# num_com_pre :          numero di comorbidit? all'ospedalizzazione precedente
# delta_num_com :        variazione nel numero di comorbidit? dalla precedente alla corrente ospedalizzazione
# class_prest :          motivo per cui hanno ospedalizzato!

reduced_time = time_btw_osp[,c(1,2,3,6,7,8,9:11,60,61,62,5)]

#Risposta con box-cox
reduced_time$V2_bc = reduced_time$V2+1
lambda_opt = powerTransform(reduced_time$V2_bc)
lambda_opt$lambda
lambda_opt_approx = 0
reduced_time$V2_bc = bcPower(reduced_time$V2_bc, lambda_opt_approx) 
hist(reduced_time$V2_bc)

#Analisi esplorativa

hist(reduced_time$countA)
length(which(reduced_time$countA > 0))
#Effetto medicine:
boxplot(V2~ countA, data = reduced_time)
boxplot(V2_bc~ CountTot, data = reduced_time)
boxplot(V2_bc~ countB, data = reduced_time)
boxplot(V2_bc~ countC, data = reduced_time)
#Si riduce la dispersione e trend positivo! Gli ultimi boxplot non sono 
# molto attendibili dato che le osservazioni sono molto poche

#Effetto et?:
boxplot(V2~ eta_Min, data = reduced_time)
boxplot(V2_bc~ eta_Min, data = reduced_time)
#Non sembra esserci molta differenza...

#Effetto comorb:
boxplot(V2 ~ num_com_pre, data = reduced_time)
boxplot(V2_bc ~ num_com_pre, data = reduced_time)
#Pi? ho numero alto di comor, pi? avr? log(V2+1) piccolo quindi V2 diminuisce

#Effetto tempo in ospedale:
boxplot(V2 ~ qt_prest_Sum, data = reduced_time)
boxplot(V2_bc ~ qt_prest_Sum, data = reduced_time)
#Non ? chiaro ...

#Effetto delta numero comorbidit?:
boxplot(V2 ~ delta_num_com, data = reduced_time)
boxplot(V2_bc ~ delta_num_com, data = reduced_time)
#Stessa cosa dei count delle medicine..? Forse ha senso toglierla?

#Effetto sesso:
boxplot(V2 ~ SESSO, data = reduced_time)
boxplot(V2_bc ~ SESSO, data = reduced_time)
# Non sembra esserci differenza

#Faccio lm con tutto tranne cod_reg,strutt_id e CountTot (tanto ? somma degli altri count)
lin_mod_iniziale = lm(V2_bc ~ SESSO+qt_prest_Sum+eta_Min+countA+countB+countC+num_com_pre+delta_num_com, data=reduced_time) #uso risposta box-cox 
summary(lin_mod_iniziale)
lin_mod = lm(V2_bc ~ SESSO+qt_prest_Sum+eta_Min+countA+countB+countC+num_com_pre, data=reduced_time) #uso risposta box-cox 
summary(lin_mod) 

x11()
par(mfrow = c(2,2))
plot(lin_mod)
plot(lin_mod$residuals) #ok
vif(lin_mod)

#Controllo se i residui hanno diverse variabilit? per diversi ospedali o COD_REG
x11()
boxplot(lin_mod$residuals ~ reduced_time$strutt_id,xlab='Strutt_id', ylab='Residuals')
sample_indx = sample(size = 1000, x=1:dim(reduced_time)[1])
boxplot(lin_mod$residuals[sample_indx] ~ reduced_time$strutt_id[sample_indx],xlab='Strutt_id', ylab='Residuals')
boxplot(lin_mod$residuals[sample_indx] ~ reduced_time$COD_REG[sample_indx],xlab='Strutt_id', ylab='Residuals')

#Passo a lmm con rand_int rispetto gli ospedali 
lmm1 = lmer(V2_bc ~ SESSO+qt_prest_Sum+eta_Min+countA+countB+countC+num_com_pre + (1|strutt_id), data = reduced_time) #modello come lin_mod ma con rand_eff su strutt_id
library(sjPlot)
library(sjstats)
tab_model(lmm1)  
summary(lmm1)
vif(lmm1)
x11()
plot(lmm1)
x11()
dotplot(ranef(lmm1))
x11()
qqnorm(unlist(ranef(lmm1)$strutt_id), main='Normal Q-Q Plot - Random Effects for strutt_id')
qqline(unlist(ranef(lmm1)$strutt_id), col='red', lwd=2)
print(vc <- VarCorr(lmm1), comp = c("Variance", "Std.Dev."))
sigma2_eps <- as.numeric(get_variance_residual(lmm1))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(lmm1))
sigma2_b
PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE

#Provo rispetto COD_REG
lmm1 = lmer(V2_bc ~ SESSO+qt_prest_Sum+eta_Min+countA+countB+countC+num_com_pre+ (1|COD_REG), data = reduced_time)
tab_model(lmm1)
summary(lmm1)
x11()
plot(lmm1)
x11()
dotplot(ranef(lmm1))
x11()
qqnorm(unlist(ranef(lmm1)$COD_REG), main='Normal Q-Q Plot - Random Effects for COD_REG')
qqline(unlist(ranef(lmm1)$COD_REG), col='red', lwd=2)
print(vc <- VarCorr(lmm1), comp = c("Variance", "Std.Dev."))
sigma2_eps <- as.numeric(get_variance_residual(lmm1))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(lmm1))
sigma2_b
PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE 

#Provo con tutti e due i rand effects
lmm1 = lmer(V2_bc ~ SESSO+qt_prest_Sum+eta_Min+countA+countB+countC+num_com_pre + (1|strutt_id) + (1|COD_REG), data = reduced_time)
tab_model(lmm1)
summary(lmm1)
x11()
plot(lmm1)
dotplot(ranef(lmm1)) #ci mette un po ma li fa entrambi uno dopo l'altro
x11()
qqnorm(unlist(ranef(lmm1)$COD_REG), main='Normal Q-Q Plot - Random Effects for COD_REG')
qqline(unlist(ranef(lmm1)$COD_REG), col='red', lwd=2)
x11()
qqnorm(unlist(ranef(lmm1)$strutt_id), main='Normal Q-Q Plot - Random Effects for strutt_id')
qqline(unlist(ranef(lmm1)$strutt_id), col='red', lwd=2)
print(vc <- VarCorr(lmm1), comp = c("Variance", "Std.Dev."))
sigma2_eps <- as.numeric(get_variance_residual(lmm1))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(lmm1))
sigma2_b
PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE 





