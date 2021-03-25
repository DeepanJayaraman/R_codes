
library(R.matlab)

setwd("D:\\R\\Seimens")
library(SPOT)

source('D:/R/Seimens/CROSSVAL.R', echo=TRUE)

N <- 10 # sample size ... N= 10,20,50,100

load(file=(paste("SD_L2_investigation_",N,"_samples",".Rdata",sep="")))


# Preallocation
PRESSRMS_KRG_v_lmom = matrix(nrow = 1,ncol=100);PRESSRMS_KRG_1_lmom = matrix(nrow = 1,ncol=100);PRESSRMS_KRG_2_lmom = matrix(nrow = 1,ncol=100);PRESSRMS_KRG_3_lmom = matrix(nrow = 1,ncol=100);PRESSRMS_KRG_4_lmom = matrix(nrow = 1,ncol=100);
PRESSRMS_KRG_v = matrix(nrow = 1,ncol=100); PRESSRMS_KRG_1 = matrix(nrow = 1,ncol=100);PRESSRMS_KRG_2 = matrix(nrow = 1,ncol=100);PRESSRMS_KRG_3 = matrix(nrow = 1,ncol=100);PRESSRMS_KRG_4 = matrix(nrow = 1,ncol=100);

## Mean and STD of responces

# Mean and standard deviation are calculated at each design point

K = 100 #repetitions

## responce - Standard deviation

response_v <- von + 3*sd_von
response_1 <- con1 + 3*sd_con1
response_2 <- con2 + 3*sd_con2
response_3 <- con3 + 3*sd_con3
response_4 <- con4 + 3*sd_con4

## responce  - Lmoment
response_v_lmom <- von + 3*l2_von
response_1_lmom <- con1 + 3*l2_con1
response_2_lmom <- con2 + 3*l2_con2
response_3_lmom <- con3 + 3*l2_con3
response_4_lmom <- con4 + 3*l2_con4

# Preallocation

surrkrg_v = matrix(nrow = 1,ncol=100);surrkrg_1 = matrix(nrow = 1,ncol=100);surrkrg_2 = matrix(nrow = 1,ncol=100);surrkrg_3 = matrix(nrow = 1,ncol=100);surrkrg_4 = matrix(nrow = 1,ncol=100);
surrkrg_v_lmom = matrix(nrow = 1,ncol=100);surrkrg_1_lmom = matrix(nrow = 1,ncol=100);surrkrg_2_lmom = matrix(nrow = 1,ncol=100);surrkrg_3_lmom = matrix(nrow = 1,ncol=100);surrkrg_4_lmom = matrix(nrow = 1,ncol=100);
Objective = matrix(nrow=100,ncol=1)
cons1 = matrix(nrow=100,ncol=1)
cons2 = matrix(nrow=100,ncol=1)
cons3 = matrix(nrow=100,ncol=1)
cons4 = matrix(nrow=100,ncol=1)
Objective_lmom = matrix(nrow=100,ncol=1)
cons1_lmom = matrix(nrow=100,ncol=1)
cons2_lmom = matrix(nrow=100,ncol=1)
cons3_lmom = matrix(nrow=100,ncol=1)
cons4_lmom = matrix(nrow=100,ncol=1)


## SURFACE MODELING OF VON MISES STRESS
X <- Designvariable
Y <- response_v

control=list(regr=regpoly1,corr=corrgauss)

for (k in 1:K){
  Z<-Y[,k]
  surrkrg_v <- buildKrigingDACE(X,Z,control)
  Objective[k] <- list(assign(paste("surrkrg_v", k, sep = ""), surrkrg_v))
  
}


Y <- response_v_lmom

control=list(regr=regpoly1,corr=corrgauss)

for (k in 1:K){
  Z<-Y[,k]
  surrkrg_v_lmom <- buildKrigingDACE(X,Z,control)
  Objective_lmom[k] <- list(assign(paste("surrkrg_v_lmom", k, sep = ""), surrkrg_v_lmom))
  
}

save(Objective,Objective_lmom,file = "PressModel_10samples.Rdata")