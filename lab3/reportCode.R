SquaredExpKernel <- function(x1,x2,sigmaF=1,l=3){ 
  n1 <- length(x1)
  n2 <- length(x2)
  K <- matrix(NA,n1,n2)
  for (i in 1:n2){
    K[,i] <- sigmaF^2*exp(-0.5*( (x1-x2[i])/l)^2 )
  }
  return(K)
}

# xStar: grid
# x: input
# y: target
# hyperparam: vector (sigmaf, l)
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

PlotLines <- function(x, y, grid, Mean, Variance, yaxis=c(-3, 3)){
  UpperLimit <- Mean+1.96 * sqrt(Variance)
  LowerLimit <- Mean-1.96 * sqrt(Variance)
  matplot(grid, Mean, type='l', col ="blue", xlab="x", ylab="y", ylim=yaxis)
  points(x, y, col="gray")
  lines(grid, UpperLimit, type="l", col="purple")
  lines(grid, LowerLimit, type="l", col="purple")
}


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
[1] 0.6065307

kernelMatrix(mySE, c(1,3,4), c(2,3,4))
An object of class "kernelMatrix"
          [,1]      [,2]      [,3]
[1,] 0.6065307 0.1353353 0.0111090
[2,] 0.6065307 1.0000000 0.6065307
[3,] 0.1353353 0.6065307 1.0000000

### 2b ###

sigmaNoise <- summary(lm(formula = temp ~ time + I(time^2)))$sigma #day would be more reasonable(?)

# ell = 0.02 => overfitting
# ell = 5 => overly smooth
# smaller sigmaf => more compact predictions (worse)
# sigmaf large enough => no changes of posterior function
model <- gausspr(time, temp, kernel = SEkernel(sigmaf=20, ell=0.2), var = sigmaNoise^2)

meanTime <- predict(model, time)

plot(time, temp, col="gray")
lines(time, meanTime, col="red")

### 2c ###

posterior <- posteriorGP(scale(time), scaledTemp, scale(time), c(20, 0.2), sigmaNoise = sigmaNoise^2)
MeanPosterior <- posterior[[1]]
VarPosterior <- posterior[[2]]

PlotLines(time, temp, time, scaledCenter + scale*MeanPosterior, diag(VarPosterior), c(-20,35))

### Closer grid and higher l

posterior <- posteriorGP(time, temp, 1:2186, c(20, 20), sigmaNoise = sigmaNoise^2)
MeanPosterior <- posterior[[1]]
VarPosterior <- posterior[[2]]

PlotLines(time, temp, 1:2186, MeanPosterior, diag(VarPosterior), c(-15,30))

### 2d ### 

model <- gausspr(day, temp, kernel = SEkernel(sigmaf=20, ell=1.2), var = sigmaNoise^2)

meanDay <- predict(model, day)

lines(time, meanDay , col="blue")

### 2e) ###

PeriodicKernel <- function(sigmaf=1, l1=3, l2=3, d){
  rval <- function(x, y = NULL){
    periodic <- exp(-(2/l1^2)*(sin(pi*abs(x-y)/d)^2))
    se <- exp(-0.5*( (x-y)/l2)^2)
    return(sigmaf^2*periodic*se)
  }
  class(rval) <- "kernel"
  return(rval)
}

PK <- PeriodicKernel(sigmaf=20, l1=1, l2=10, d=365/sd(time))

model <- gausspr(time, temp, kernel = PK, var = sigmaNoise^2)

meanPeriodicKernel <- predict(model, time)

lines(time, meanPeriodicKernel , col="green")

### Task 3

library(AtmRay)

data <- read.csv('https://github.com/STIMALiU/AdvMLCourse/raw/master/GaussianProcess/Code/banknoteFraud.csv',
                 header=FALSE, sep=',')
names(data) <- c("varWave","skewWave","kurtWave","entropyWave","fraud")
data[,5] <- as.factor(data[,5])

set.seed(111); 
SelectTraining <- sample(1:dim(data)[1], size = 1000, replace = FALSE)

training <- data[SelectTraining,]
test <- data[-SelectTraining,]

### 3a

GPClassification <- gausspr(x=training[,1:2], y=training[,5], data=training, type="classification")

preds <- predict(GPClassification,subset(training, select=c("varWave", "skewWave")))

confMatrixTr <- table(preds, training$fraud)
preds   0   1
0     512  24
1      44 420

accTr <- sum(diag(confMatrixTr))/sum(confMatrixTr)
[1] 0.932

x1 <- seq(min(training[,1]),max(training[,1]),length=1000)
x2 <- seq(min(training[,2]),max(training[,2]),length=1000)
gridPoints <- meshgrid(x1, x2)
gridPoints <- cbind(c(gridPoints$x), c(gridPoints$y))

gridPoints <- data.frame(gridPoints)
names(gridPoints) <- names(data)[1:2]

probPreds <- predict(GPClassification, gridPoints, type="probabilities")

contour(x1,x2,t(matrix(probPreds[,1],1000)), 20, xlab = "varWave", ylab = "skewWave", main = 'Prob(Not Fraud) - Not fraud is red')
points(training[training$fraud=='0',1],training[training$fraud=='0',2],col="red")
points(training[training$fraud=='1',1],training[training$fraud=='1',2],col="blue")

### 3b

GPClassification <- gausspr(fraud ~  varWave + skewWave, data=training)
GPClassification

predict(GPClassification,subset(test, select=c("varWave", "skewWave")))
confMatrixTe <- table(predict(GPClassification,subset(test, select=c("varWave", "skewWave"))), test$fraud)
    0   1
0 192   9
1  14 157

accTe <- sum(diag(confMatrixTe))/sum(confMatrixTe)
[1] 0.938172

### 3c

GPClassification <- gausspr(fraud ~ varWave + skewWave + kurtWave + entropyWave, data=training)
GPClassification

predict(GPClassification, subset(test, select=c("varWave","skewWave","kurtWave","entropyWave")))
confMatrixFullTe <- table(predict(GPClassification,subset(test, select=c("varWave","skewWave","kurtWave","entropyWave"))), test$fraud)
    0   1
0 205   0
1   1 166

accFullTe <- sum(diag(confMatrixFullTe))/sum(confMatrixFullTe)
[1] 0.9973118

