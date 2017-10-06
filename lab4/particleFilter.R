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
  x[1] <- emissionFunction(z[1], udfE[1], 1)
  
  for (t in 2:T){
    #transition
    z[t] <- transitionFunction(z[t-1], udfT[t], 1)
    #emission
    x[t] <- emissionFunction(z[t], udfE[t], sdE)
  }
  return(list(z, x))
}

sdE <- 1

result <- SSM(100, sdE)

z <- result[[1]]

x <- result[[2]]

zApprox <- matrix(0, 100, 100) 

weights <- rep(0, 100)

particles <- matrix(0, 100, 101)

particles[,1] <- runif(100,1,100) # zt^m

### Loopen

#install.packages("LaplacesDemon")
library(LaplacesDemon)

for (s in 1:100){ # s for step
  for (p in 1:100){ # p for particle
    weights[p] <- (dnorm(x[1], particles[p], sdE) + dnorm(x[1], particles[p]-1, sdE) + dnorm(x[1], particles[p] + 1 , sdE))/ 3
  }
  weights <- weights/sum(weights) # 
  
  for (i in 1:100){ # i for iteration
    particles[i, s+1] <- weights[i] * (rnorm(1, particles[i, s], sdE) + rnorm(1, particles[i, s]+1, sdE) + rnorm(1, particles[i, s]+2, sdE))/3 # mixture distribution
  }
}

hist(particles[particles[,2]<2,2])


##fel nedan

# zApprox[,s] <- weights*particles[,s] # MIXTURE DISTRIBUTION 
# 
# knas <- rcat(100, zApprox[,s])

for (s in 1:100){ # s for step
  ufdE <- runif(100,0,1)
  for (p in 1:100){ # p for particle
    weights[p] <- dnorm(x[s], mean = emissionFunction(particles[p, s], ufdE[p], sdE), sd = sdE)
  }
  weights <- weights/sum(weights) # normalize
  zApprox[s] <- sum(weights*particles[,s]) # approximate z
  ufdT <- runif(100,0,1) # random variables for transition
  for (i in 1:100){ # i for iteration
    particles[i, s+1] <- rnorm(1, mean = transitionFunction(zApprox[s], ufdT[i], sdE), sd=sdE)
  }
}

plot(1:100, apply(particles, 1, mean))
lines(1:100, zApprox)

hist(z-zApprox)

installed.packages(reshape2)
library(reshape2)
plot(melt(particles)[,2:3])
