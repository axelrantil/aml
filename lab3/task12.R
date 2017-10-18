SquaredExpKernel <- function(x1,x2,sigmaF=1,l=3){ 
  n1 <- length(x1)
  n2 <- length(x2)
  K <- matrix(NA,n1,n2)
  for (i in 1:n2){
    K[,i] <- sigmaF^2*exp(-0.5*( (x1-x2[i])/l)^2 )
  }
  return(K)
}

PlotLines <- function(x, y, grid, Mean, Variance, yaxis=c(-3, 3)){
  UpperLimit <- Mean+1.96 * sqrt(Variance)
  LowerLimit <- Mean-1.96 * sqrt(Variance)
  matplot(grid, Mean, type='l', col ="blue", xlab="x", ylab="y", ylim=yaxis)
  points(x, y, col="gray")
  lines(grid, UpperLimit, type="l", col="purple")
  lines(grid, LowerLimit, type="l", col="purple")
}

# xStar: grid
# x: datapunkter
# y: target
# hyperparam: vector with (sigmaf, l)
posteriorGP <- function(x, y, xStar, hyperParam, sigmaNoise){
  sigmaF <- hyperParam[[1]]
  l <- hyperParam[[2]]
  K <-SquaredExpKernel(x, x, sigmaF, l)
  L <- t(chol(K + sigmaNoise *diag(1, nrow(K),ncol(K))))
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

posterior <- posteriorGP(x, y, xStar, c(1, 0.3), 0.1^2)

MeanPosterior <- posterior[[1]]

VarPosterior <- posterior[[2]]

PlotLines(x, y, xStar, MeanPosterior, diag(VarPosterior))

### 1c) ###

x <- c(-0.6, 0.4)
y <- c(-0.044, 0.719)

posterior <- posteriorGP(x, y, xStar, c(1, 0.3), 0.1^2) 

MeanPosterior <- posterior[[1]]

VarPosterior <- posterior[[2]]

PlotLines(x, y, xStar, MeanPosterior, diag(VarPosterior))


### 1d) ###

x <- c(-1.0, -0.6, -0.2, 0.4, 0.8)
y <- c(0.768, -0.044, -0.940, 0.719, -0.664)

posterior <- posteriorGP(x, y, xStar, c(1, 0.3), 0.1^2)

MeanPosterior <- posterior[[1]]

VarPosterior <- posterior[[2]]

PlotLines(x, y, xStar, MeanPosterior, diag(VarPosterior))

### 1e) ###

posterior <- posteriorGP(x, y, xStar, c(1, 1), 0.1^2)

MeanPosterior <- posterior[[1]]

VarPosterior <- posterior[[2]]

PlotLines(x, y, xStar, MeanPosterior, diag(VarPosterior))

### 2 ###
#install.packages("kernlab")
library(kernlab)

data <- read.csv('https://github.com/STIMALiU/AdvMLCourse/raw/master/GaussianProcess/Code/TempTullinge.csv', header=TRUE,
          sep=';')

time <- seq(1,2190, 5)

day <- rep(time[1:73],6)

temp <- data[time,2]

scaledTemp <- scale(temp) # Needed for implementation of posteriorGP

scaledCenter <- attributes(scaledTemp)$'scaled:center'

scale <- attributes(scaledTemp)$'scaled:scale'

timetemp <- cbind(time, temp)

daytemp <- cbind(day, temp)

daytemp <- daytemp[order(daytemp[,1]),]

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

sigmaNoise <- summary(lm(formula = temp ~ time + I(time^2)))$sigma

polyFit <- lm(temp ~ time + I(time^2))
sigmaNoise2 = sd(polyFit$residuals)

# ell = 0.02 => overfitting
# ell = 5 => overly smooth
# smaller sigmaf => more compact predictions (worse)
# sigmaf large enough => no changes for larger sigmaf
model <- gausspr(time, temp, kernel = SEkernel(sigmaf=20, ell=0.2), var = sigmaNoise^2)

meanTime <- predict(model, time)

plot(time, temp, col="gray")
#lines(day,mean)
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

sigmaNoise <- summary(lm(formula = temp ~ day + I(day^2)))$sigma

## 6 times 0.2 because of 6 years
model <- gausspr(day, temp, kernel = SEkernel(sigmaf=20, ell=(6*0.2)), var = sigmaNoise^2)

meanDay <- predict(model, day)

#plot(time, temp)
lines(time, meanDay , col="blue")

### 2d ### (own implementation)

model <- gausspr(daytemp[,1], daytemp[,2], kernel = SEkernel(sigmaf=20, ell=1.2), var = sigmaNoise^2)

mean <- predict(model, timetemp[,1])

plot(daytemp[,1], daytemp[,2], col="gray")
lines(timetemp[,1],mean, col="red")

posterior <- posteriorGP(scale(daytemp[,1]), scale(daytemp[,2]), scale(timetemp[,1]), c(20, 5), sigmaNoise = sigmaNoise^2)

MeanPosterior <- posterior[[1]]

VarPosterior <- posterior[[2]]

PlotLines(timetemp[,1], timetemp[,2], timetemp[,1], scaledCenter + scale*MeanPosterior, diag(VarPosterior), c(-15,30))

diag(VarPosterior)

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

## l1 controls the correlation of the same day in different years
## l2 controls the correlation of the days nearby
## kernlab standardized inputs to have standard deviation of 1 (hence strange period)
PK <- PeriodicKernel(sigmaf=20, l1=1, l2=10, d=365/sd(time))

model <- gausspr(time, temp, kernel = PK, var = sigmaNoise^2)

meanPeriodicKernel <- predict(model, time)

lines(time, meanPeriodicKernel , col="green")

legend(1500, -10, legend=c("SE Kernel Time", "SE Kernel Day", "Periodic Kernel Time"),
       col=c("red", "blue", "green"), lty=1:1)

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

accTr <- sum(diag(confMatrixTr))/sum(confMatrixTr)

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

accTe <- sum(diag(confMatrixTe))/sum(confMatrixTe)

### 3c

GPClassification <- gausspr(fraud ~ varWave + skewWave + kurtWave + entropyWave, data=training)
GPClassification

predict(GPClassification, subset(test, select=c("varWave","skewWave","kurtWave","entropyWave")))
confMatrixFullTe <- table(predict(GPClassification,subset(test, select=c("varWave","skewWave","kurtWave","entropyWave"))), test$fraud)

accFullTe <- sum(diag(confMatrixFullTe))/sum(confMatrixFullTe)
