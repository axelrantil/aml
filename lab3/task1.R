
SquaredExpKernel <- function(x1,x2,sigmaF=1,l=3){ #Implementera egen?
  n1 <- length(x1)
  n2 <- length(x2)
  K <- matrix(NA,n1,n2)
  for (i in 1:n2){
    K[,i] <- sigmaF^2*exp(-0.5*( (x1-x2[i])/l)^2 )
  }
  return(K)
}


# xStar: grid
# x: datapunkter
# y: target
# hyperparam: vector with (sigmaf, l)
posteriorGP <- function(x, y, xStar, hyperParam, sigmaNoise){
  sigmaF <- hyperParam[[1]]
  l <- hyperParam[[2]]
  K <-SquaredExpoKernel(x, x, sigmaF, l)
  L <- t(chol(K + sigmaNoise*diag(1, nrow(K),ncol(K))))
  alpha <- solve(t(L), solve(L, y))
  kStar <- SquaredExpoKernel(x, xStar, sigmaF, l)
  MeanPosterior <- t(kStar)%*%alpha
  v <- solve(L, kStar)
  VarPosterior <- SquaredExpoKernel(xStar, xStar, sigmaF, l) - t(v)%*%v
  return(list(MeanPosterior, VarPosterior))
}

xStar <- seq(-1,1,0.01)

#meanvar <- posteriorGP(0.4, 0.719, xStar, c(1, 0.3), 0.1)

sigmaF <- 1
l <- 0.3
sigmaNoise <- 0.1
x <- 0.4
y <- 0.719
K <-SquaredExpKernel(x, x, sigmaF, l)
L <- t(chol(K + sigmaNoise*diag(1, nrow(K),ncol(K))))
alpha <- solve(t(L), solve(L, y))
kStar <- SquaredExpKernel(x, xStar, sigmaF, l) 
MeanPosterior <- t(kStar)%*%alpha # fstar normalisera?
v <- solve(L, kStar)
VarPosterior <- SquaredExpKernel(xStar, xStar, sigmaF, l) - t(v)%*%v



#mean <- meanvar[[1]]

#var <- meanvar[[2]]
#diag(var)

# matplot(1:201, mean, type='l', col ="red", xlab="x", ylab="y")
# lines(mean+1.96*sqrt(diag(var)), type="l", col="blue")
# lines(mean-1.96*sqrt(diag(var)), type="l", col="green")
