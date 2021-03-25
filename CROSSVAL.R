Crossval <- function(X,Y){
  # X Input
  # Y Responce
  
  library(SPOT)
  Yhat = matrix(ncol=1,nrow=(dim(X)[1]))
  X = as.matrix(X)
  Y = as.matrix(Y)
  for (i in 1:(dim(X)[1])){
    A = X[-i,]
    B  = Y[-i,]
    Fit = buildKrigingDACE(A,B,control=list(regr=regpoly1,corr=corrgauss))
    # Fit = buildKrigingDACE(A,B,control=list(regr=regpoly1,corr=corrspline))
    P = predict(Fit,t(X[i,1:ncol(X)]))
    P <- P[1,1]
    Yhat[i,1] <- P
  }
  exV = Yhat-Y;
    PRESS = t(exV)%*%exV;
  PRESSRMS_v = sqrt(PRESS/(dim(X)[1]));
}

