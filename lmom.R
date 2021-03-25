# lmom by Kobus N. Bekker, 14-09-2004
# Based on calculation of probability weighted moments and the coefficient
# of the shifted Legendre polynomial.

# Given nonnegative integer nL, compute the
# nL l-moments for given data vector X.
# Return the l-moments as vector L.

lmom <- function(X,nL) {
  
  # nL=2
  
  rows <- nrow(X)
  cols <- ncol(X)
  
  if (cols == 1)
  { X <- t(X) }
  
  n <- max(dim(X))
  X <- t(sort(X))
  b <- matrix(0L,1,nL-1)
  l <- matrix(0L,1,nL-1)
  b0 <- mean(X)
  
  if (nL == 1){
    b0 <- b0
    L=t(c(b0))
    return(L)
  }
  
  else{
    for (r in 1:(nL-1)){
      if (r!=0){
        if(n==r & n>r){
          A1 = matrix(0L,r+1,1)
          A2 = matrix(0L,r+1,1)}
        else{
          A1 = repmat(t((r+1):n),r,1)
          A2 = repmat(matrix(1:r),1,n-r)}
        
        A = A1-A2
        
        Num <- t(apply(A, 2, prod))
        B= repmat(matrix(n),1,r) - t(1:r)
        Den <- apply(matrix(B),2,prod)
        
        b[r] <- (1/n) * sum( (Num/Den) * X[(r+1):n] )
      }
    }
    tB <- matrix(c(b0,b))
    
    # B <- matrix(tB[max(dim(tB)):-1:1])
    
    B <- matrix(tB[max(dim(tB)):1])
    
    
    for (i in 1:(nL-1)){
      if (i!=0){
        Spc <- matrix(0L,(max(dim(B))-(i+1)),1)
        Coeff <- matrix(c(Spc,LegendreShiftPoly(i)))
        l[i] <- colSums(Coeff*B)
      }
    }
    L <- t(c(b0,l))
    return(L)
  }
}

