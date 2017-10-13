emissionFunction <- function(z, ufd, sdE){
  if (ufd < 1/3){
    return(rnorm(1, z, sdE))
  } else if (ufd > 2/3) {
    return(rnorm(1, z-1, sdE))
  } else {
    return(rnorm(1, z+1, sdE))
  }
}

transitionFunction <- function(z, ufd, sdT){
  if (ufd < 1/3){
    return(rnorm(1, z, sdT))
  } else if (ufd > 2/3) {
    return(rnorm(1, z+1, sdT))
  } else {
    return(rnorm(1, z+2, sdT))
  }
}

SSM <- function(steps, sdE){
  x <- rep(-1,100)
  z <- rep(-1,100)
  T <- steps
  
  # initial state
  z[1] <- runif(1,0,100)
  
  udfE <- runif(100,0,1)
  udfT <- runif(100,0,1)
  
  # first emission
  x[1] <- emissionFunction(z[1], udfE[1], sdE)
  
  for (t in 2:T){
    #transition
    z[t] <- transitionFunction(z[t-1], udfT[t], 1)
    #emission
    x[t] <- emissionFunction(z[t], udfE[t], sdE)
  }
  return(list(z, x))
}


filtering <- function(x, sdE){
  sdE <- sdE
  zApprox <- matrix(0, 1, 100) 
  weights <- matrix(0,100,100)
  particles <- matrix(0, 100, 101)
  weightComponent<-matrix(0,100,100)
  
  particles[,1] <- runif(100,1,100) # zt^m
  
  for (t in 1:100){ # t for time
    for (p in 1:100){ # p for particle
      weights[p,t] <- (dnorm(x[t], particles[p, t], sdE)
                       + dnorm(x[t], particles[p, t]-1, sdE)
                       + dnorm(x[t], particles[p,t]+1 , sdE))/ 3
    }
    weights[,t] <- weights[,t]/sum(weights[,t]) # Normalize
    weightComponent[,t] <- sample(particles[,t], size=100, replace=TRUE, prob = weights[,t])
    
    ufd <- runif(100,0,1)
    for (p in 1:100){ # p for particle
      particles[p, t+1] <- transitionFunction(weightComponent[p, t], ufd = ufd[p], sdT = 1)
    }
    zApprox[t] <- sum(weights[,t]*particles[,t])
  }
  return(list(zApprox, weights, particles))
}

set.seed(123456)

sdE <- 50

simulation <- SSM(100, sdE)

z <- simulation[[1]]
x <- simulation[[2]]

result <- filtering(x, sdE)

zApprox <- result[[1]]
weights <- result[[2]]
particles <- result[[3]]

SEError <- sqrt(sum((zApprox-z)^2))

plot(1:100,z,type="l", col = "red")
lines(1:100, zApprox, col="green")

plot(1:100, abs(z - zApprox), type="l", ylab="Difference between z and approx", xlab="Step")

zMat<-matrix(z,100,100,byrow=TRUE)

apply(abs(zMat-particles[,1:100]),2,mean)

plot(1:100,apply(abs(zMat-particles[,1:100]),2,mean))

#installed.packages(reshape2)
library(reshape2)
plot(melt(particles)[,2:3],xlab="Step", ylab="Particles, estimated and real z")
lines(1:100, z, col="red")
lines(1:100, zApprox, col="green")
lines(1:100, x, col="yellow")
library(reshape2)
plot(melt(particles)[,2:3])

hist(z-zApprox, breaks=20)

