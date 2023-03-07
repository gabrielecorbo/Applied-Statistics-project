library(FactoMineR)
library(Rcpp)
library(car)
library(MASS)
library(rgl)
library(olsrr)
library(nlmeU) ## --> for the dataset
library(nlme)  ## --> for models implementation
library(corrplot)
library(lattice)
library(plot.matrix)
library(ggplot2)
library(insight)
library(lattice)
library(lme4)

load("HF.Rdata")
heartf=HF.2006_2012
remove(HF.2006_2012)

singoli_osp <- read.table('datasets/singoli_osp.txt', header=T) 
singoli_osp2 <- read.table('datasets/singoli_osp2.txt', header=T) 
milano <- read.table('datasets/milano.txt', header=T) 
mil_red <- read.table('datasets/mil_red.txt', header=T) 
only_cat <- read.table('datasets/only_cat.txt', header=T) 
day_deaths <- read.table('datasets/day_deaths.txt', header=T) 
Dead_como <- read.table('datasets/Dead_como.txt', header=T) 

heartf_osp=heartf[heartf$tipo_prest==41,]  # dataset con solo le ospedalizzazioni
heartf_pres=heartf[heartf$tipo_prest==30,c(1:11)]  # dataset con solo le prestazioni

milano=singoli_osp[which(singoli_osp$ASL_RESIDENZA=='308'),]
mil=singoli_osp[which(singoli_osp$ASL_RESIDENZA=='308'),13:32]

load("Cluster_Milano.RData") #contiene cluster.mc, gia in milano
