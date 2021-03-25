
#Input data



# library(readxl)
# data <- read_excel("D:/R/Seimens/data.xlsx", 
#                       range = "A4:W203", col_names = FALSE)

# Set working directory 
setwd("D:\\R\\Seimens")
# 
data <- read.csv(file ="data.csv",header = FALSE)

X <- as.matrix(data[,1:17])#Design variable
Y <- as.matrix(data[,19])#Objective functions [Y(:,1) = von]

Z <- as.matrix(data[,20:23])#Constraints


## Approximation - Von mises stress and constraints
library(SPOT)
## Create model

N <- Y# change the responses
KRG_von <- buildKrigingDACE(X,N,control=list(regr=regpoly2,corr=correxp))

N <- Z[,1]# change the responses
KRG_cons1 <- buildKrigingDACE(X,N,control=list(regr=regpoly1,corr=correxp))

N <- Z[,2]# change the responses
KRG_cons2 <- buildKrigingDACE(X,N,control=list(regr=regpoly2,corr=correxp))

N <- Z[,3]# change the responses
KRG_cons3 <- buildKrigingDACE(X,N,control=list(regr=regpoly2,corr=corrspline))

N <- Z[,4]# change the responses
KRG_cons4 <- buildKrigingDACE(X,N,control=list(regr=regpoly1,corr=correxp))


save.image(file = "Base_Surrogate.RData")

