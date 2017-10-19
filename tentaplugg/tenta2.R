# gamma-exponential kernel
k <- function(sigmaf = 1, ell = 1, gamma = 2)  
{   
  rval <- function(x, y = NULL) 
  {       
    r = sqrt(crossprod(x-y))       
    return(sigmaf^2*exp(-(r/ell)^gamma))     
  }   
  class(rval) <- "kernel"   
  return(rval) 
}

library(kernlab)

r <- seq(0,3, by=0.01)

expk1 <- k(1,1,1)

expk2 <- k(1,1,2)

kM1r0<- kernelMatrix(expk1, r, 0)

kM2r0<- kernelMatrix(expk2, r, 0)

par(mfrow = c(1,1))
legend("topright", c("kM1","kM2"),
       col=c("green", "red"),
       lty = c(1,1))
plot(kM1r0, type="l", col="green")
lines(kM2r0, col="red")

x <- seq(0,2,by=0.01)

kM1<- kernelMatrix(expk1, x, x)

kM2<- kernelMatrix(expk2, x, x)

### 2ii. 
library(MASS)

dim(kM1)

result2 <- mvrnorm(3, rep(0,201), kM1)
plot(result2[1,], type="l", col="green", ylim=c(-4,4))
lines(result2[2,], col="green")
lines(result2[3,], col="green")

result2 <- mvrnorm(3, rep(0,201), kM2)
lines(result2[1,], col="red")
lines(result2[2,], col="red")
lines(result2[3,], col="red")

## 3b i
data <- load("GPdata.Rdata")

plot(x,y, ylim=c(-3,6))

sigman = 0.5^2

kM1<- kernelMatrix(expk1, x, x)

kM2<- kernelMatrix(expk2, x, x)

res1 <- gausspr(x,y, kernel=expk1, var=sigman)

res2 <- gausspr(x,y, kernel=expk2, var=sigman)

xGrid <- seq(-3,3, by=0.01)

pred1 <- predict(res1, xGrid)

pred2 <- predict(res2, xGrid)


plot(x,y,  ylim=c(-3,6))
lines(xGrid, pred1, col="blue")
lines(xGrid, pred2, col="red")



## 3b ii
cov1 <- kernelMatrix(expk1, xGrid, xGrid) - kernelMatrix(expk1, xGrid, x) %*% solve(kernelMatrix(expk1, x,x) + sigman*diag(1,nrow=length(x),ncol=length(x)))%*%kernelMatrix(expk1,x,xGrid)
var1 <- diag(cov1)
cov2 <- kernelMatrix(expk2, xGrid, xGrid) - kernelMatrix(expk2, xGrid, x) %*% solve(kernelMatrix(expk2, x,x) + sigman*diag(1,nrow=length(x),ncol=length(x)))%*%kernelMatrix(expk2,x,xGrid)
var2 <- diag(cov2)

upper1 <- pred1 + 1.96*(sqrt(var1))
lines(xGrid, upper1, col="lightblue")
lower1 <- pred1 - 1.96*(sqrt(var1))
lines(xGrid, lower1, col="lightblue")

upper2 <- pred2 + 1.96*(sqrt(var2))
lines(xGrid, upper2, col="pink")
lower2 <- pred2 - 1.96*(sqrt(var2))
lines(xGrid, lower2, col="pink")




