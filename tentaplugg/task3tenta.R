posteriorGP <- function(x, y, xStar, Kernel, sigmaNoise){
  K <-kernelMatrix(Kernel, x, x)
  L <- t(chol(K + sigmaNoise *diag(1, nrow(K),ncol(K))))
  alpha <- solve(t(L), solve(L, y))
  kStar <- kernelMatrix(Kernel, x, xStar) 
  MeanPosterior <- t(kStar)%*%alpha
  v <- solve(L, kStar)
  VarPosterior <- kernelMatrix(Kernel,xStar, xStar) - t(v)%*%v
  return(list(MeanPosterior, VarPosterior))
}


# Squared exponential kernel
k1 <- function(sigmaf = 1, ell = 1)  
{   
  rval <- function(x, y = NULL) 
  {       
    r = sqrt(crossprod(x-y))       
    return(sigmaf^2*exp(-r^2/(2*ell^2)))     
  }   
  class(rval) <- "kernel"   
  return(rval) 
}


k2 <- function(sigmaf = 1, ell = 1, alpha = 1)  
{   
  rval <- function(x, y = NULL) 
  {     r = sqrt(crossprod(x-y))     
  return(sigmaf^2*(1+r^2/(2*alpha*ell^2))^-alpha)   
  }   
  class(rval) <- "kernel"   
  return(rval) 
} 


k3 <- function(sigmaf = 1, ell = 1)  
{   
  rval <- function(x, y = NULL) 
  {	r = sqrt(crossprod(x-y))
  return(sigmaf^2*(1+sqrt(3)*r/ell)*exp(-sqrt(3)*r/ell))   
  }   
  class(rval) <- "kernel"   
  return(rval) 
} 

### 3a
library(kernlab)

sigmaf = 1
l = 1

r <- seq(0,4, by=0.01)

kern1 = k1(sigmaf, l)
kernmatr1 <- kernelMatrix(kern1, r, 0)

kern21 = k2(sigmaf, l, alpha=1/2)
kernmatr21 <- kernelMatrix(kern21, r, 0)

kern22 = k2(sigmaf, l, alpha=2)
kernmatr22 <- kernelMatrix(kern22, r, 0)

kern23 = k2(sigmaf, l, alpha=20)
kernmatr23 <- kernelMatrix(kern23, r, 0)

kern3 = k3(sigmaf, l)
kernmatr3 <- kernelMatrix(kern3, r, 0)

plot(kernmatr1, type="l")
lines(kernmatr21, col="red")
lines(kernmatr22, col="green")
lines(kernmatr23, col="yellow")
lines(kernmatr3, col="blue")

