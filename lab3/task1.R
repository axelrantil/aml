
SquaredExpKernel <- function(x1,x2,sigmaF=1,l=3){ #Implementera egen?
  n1 <- length(x1)
  n2 <- length(x2)
  K <- matrix(NA,n1,n2)
  for (i in 1:n2){
    K[,i] <- sigmaF^2*exp(-0.5*( (x1-x2[i])/l)^2 )
  }
  return(K)
}

PlotLines <- function(x, y, grid, Mean, Variance){
  UpperLimit <- Mean+1.96 * sqrt(Variance)
  LowerLimit <- Mean-1.96 * sqrt(Variance)
  matplot(grid, Mean, type='l', col ="red", xlab="x", ylab="y", ylim=c(-3, 3))
  points(x, y, col="blue")
  lines(grid, UpperLimit, type="l", col="green")
  lines(grid, LowerLimit, type="l", col="green")
}

# xStar: grid
# x: datapunkter
# y: target
# hyperparam: vector with (sigmaf, l)
posteriorGP <- function(x, y, xStar, hyperParam, sigmaNoise){
  sigmaF <- hyperParam[[1]]
  l <- hyperParam[[2]]
  K <-SquaredExpKernel(x, x, sigmaF, l)
  L <- t(chol(K + sigmaNoise*diag(1, nrow(K),ncol(K))))
  alpha <- solve(t(L), solve(L, y))
  kStar <- SquaredExpKernel(x, xStar, sigmaF, l) 
  MeanPosterior <- t(kStar)%*%alpha
  v <- solve(L, kStar)
  VarPosterior <- SquaredExpKernel(xStar, xStar, sigmaF, l) - t(v)%*%v
  return(list(MeanPosterior, VarPosterior))
}

xStar <- seq(-1,1,0.01)

### 1b) ###

x <- 0.4

y <- 0.719

posterior <- posteriorGP(x, y, xStar, c(1, 0.3), 0.1)

MeanPosterior <- posterior[[1]]

VarPosterior <- posterior[[2]]

PlotLines(x, y, xStar, MeanPosterior, diag(VarPosterior))

### 1c) ###

x <- c(-0.6, 0.4)
y <- c(-0.044, 0.719)

posterior <- posteriorGP(x, y, xStar, c(1, 0.3), 0.1)

MeanPosterior <- posterior[[1]]

VarPosterior <- posterior[[2]]

PlotLines(x, y, xStar, MeanPosterior, diag(VarPosterior))


### 1d) ###

x <- c(-1.0, -0.6, -0.2, 0.4, 0.8)
y <- c(0.768, -0.044, -0.940, 0.719, -0.664)

posterior <- posteriorGP(x, y, xStar, c(1, 0.3), 0.1)

MeanPosterior <- posterior[[1]]

VarPosterior <- posterior[[2]]

PlotLines(x, y, xStar, MeanPosterior, diag(VarPosterior))

### 1e) ###

posterior <- posteriorGP(x, y, xStar, c(1, 1), 0.1)

MeanPosterior <- posterior[[1]]

VarPosterior <- posterior[[2]]

PlotLines(x, y, xStar, MeanPosterior, diag(VarPosterior))

### 2 ###
#install.packages("kernlab")
library(kernlab)

data <- read.csv('https://github.com/STIMALiU/AdvMLCourse/raw/master/GaussianProcess/Code/TempTullinge.csv', header=TRUE,
          sep=';')

time <- c(1:2190)

day <- c(1:2190 %% 365)
day <- replace(day, day==0, 365)

time <- time[seq(1,length(time), 5)]

day <- day[seq(1,length(day), 5)]

### 2a ###

SEkernel <- function(sigmaf=1, ell=3){
  rval <- function(x, y = NULL){
      return(sigmaf^2*exp(-0.5*( (x-y)/ell)^2))
  }
  class(rval) <- "kernel"
  return(rval)
}

mySE <- SEkernel(sigmaf=1, ell=1) 

mySE(1,2)

kernelMatrix(mySE, c(1,3,4), c(2,3,4))

### 2b ###

summary(lm(formula = data[day,2] ~ day + I(day^2)))

plot(day, data[day,2])
lines(day, (-20.013013+0.359757*day-0.000939*I(day^2)))

quadfit



