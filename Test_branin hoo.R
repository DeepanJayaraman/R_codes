## Test-function:
braninFunction <- function (x) {	
  (x[2]  - 5.1/(4 * pi^2) * (x[1] ^2) + 5/pi * x[1]  - 6)^2 +
    10 * (1 - 1/(8 * pi)) * cos(x[1] ) + 10
}
## Create design points 
x <- cbind(runif(20)*15-5,runif(20)*15)

## Compute observations at design points (for Branin function)
y <- as.matrix(apply(x,1,braninFunction))

library(SPOT)
## Create model
fit <- buildKrigingDACE(x,y,control=list(regr=regpoly2,corr=corrspline))
# fit$target <- c("y","s","ei")

# create testing points
x1 <- cbind(runif(20)*15-5,runif(20)*15)

## first estimate error with regressive predictor
Y1 = predict(fit,x1)

LIST = list(x ,y ,x1,Y1 )

## cross validation
library(crossval)
# n = max(dim(x)) # Number of times
crossval(fit,x,y,K=1)

library(xlsx)
write.xlsx(LIST, "D:\\R\\Braninhoo.xlsx")
