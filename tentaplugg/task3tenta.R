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


plot(r, mvrnorm(1, rep(0,length(r)), kernelMatrix(kern21,r,r)), type="l", col="red", ylim=c(-2,2))
lines(r, mvrnorm(1, rep(0,length(r)), kernelMatrix(kern21,r,r)), type="l", col="red")
lines(r, mvrnorm(1, rep(0,length(r)), kernelMatrix(kern22,r,r)), type="l",col="green")
lines(r, mvrnorm(1, rep(0,length(r)), kernelMatrix(kern22,r,r)), type="l",col="green")
lines(r, mvrnorm(1, rep(0,length(r)), kernelMatrix(kern23,r,r)), type="l",col="yellow")
lines(r, mvrnorm(1, rep(0,length(r)), kernelMatrix(kern23,r,r)), type="l",col="yellow")

## 3bi

load("GPdata.RData")

xGrid <- seq(-3, 3, by=0.01)

mod1 <- gausspr(x,y, kernel=kern1, var=(0.5^2))
pred1 <- predict(mod1, xGrid)

mod3 <- gausspr(x,y, kernel=kern3, var=(0.5^2))
pred3 <- predict(mod3, xGrid)

plot(x,y)
lines(xGrid, pred1)
lines(xGrid, pred3, col="red")

## 3bii

kxGxG <- kernelMatrix(kern1, xGrid, xGrid)
kxxG <- kernelMatrix(kern1, x, xGrid)
kxGx <- kernelMatrix(kern1, xGrid, x)
kxx <- kernelMatrix(kern1, x, x)

covf <- kxGxG - kxGx %*% solve(kxx + 0.25*diag(1, length(x), length(x))) %*% kxxG
var1 <- diag(covf)

kxGxG <- kernelMatrix(kern3, xGrid, xGrid)
kxxG <- kernelMatrix(kern3, x, xGrid)
kxGx <- kernelMatrix(kern3, xGrid, x)
kxx <- kernelMatrix(kern3, x, x)

covf <- kxGxG - kxGx %*% solve(kxx + 0.25*diag(1, length(x), length(x))) %*% kxxG
var3 <- diag(covf)

upper1 <- pred1 + 1.96*sqrt(var1+0.25)
lower1 <- pred1 - 1.96*sqrt(var1+0.25)

upper3 <- pred3 + 1.96*sqrt(var3+0.25)
lower3 <- pred3 - 1.96*sqrt(var3+0.25)

lines(xGrid, upper1, col="gray")
lines(xGrid, lower1, col="gray")

lines(xGrid, upper3, col="pink")
lines(xGrid, lower3, col="pink")
