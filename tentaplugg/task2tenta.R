
### 2a)

library(HMM)
library(data.table)

states = c(1:10)
symbols = c(1:11)

transitionProbs <- diag(1/2, 10, 10)
transitionProbs <- transitionProbs + apply(transitionProbs, 1, function(x) shift(x,n=1, type="lead", fill=x[1]))

startProbs = rep(1, 10)/10

emissionProbs <- c(0.1, 0.1, 0.1, 0, 0, 0, 0, 0, 0.1, 0.1)

for (i in 1:10) {
  emissionProbs <- c(emissionProbs, shift(tail(emissionProbs,10), n=1, fill=tail(emissionProbs,1)))
}

emissionProbs <- matrix(emissionProbs, 10, 10)

emissionProbs <- cbind(emissionProbs, rep(0.5,10))

hmm = initHMM(states, symbols, startProbs, transitionProbs, emissionProbs)

res <- simHMM(hmm, 100)

z <- res$states

x <- res$observation

forward <- max.col(t(prop.table(exp(forward(hmm, x)),2)))
obs
likelyPath <- viterbi(hmm, x)

plot(1:100, likelyPath, type="l")
lines(1:100, z, col="green")
points(1:100, forward, col="red")

### 2b

library(HMM)
library(data.table)

states = c(1:10)
symbols = c(1:5)

transitionProbs <- diag(c(1/2,1), 10, 10)
transitionProbs <- cbind(transitionProbs[,10],transitionProbs[,1:9]) + diag(c(1/2,0),10,10)
transitionProbs

startProbs = rep(c(1,0),5)/5

emissionProbs <- c(0.2,0.2,0,0,0.2)
for (i in 2:10){
  if (i%%2==0){
    emissionProbs <- c(emissionProbs, tail(emissionProbs,5))
  } else{
    emissionProbs <- c(emissionProbs, c(tail(emissionProbs,1),tail(emissionProbs,5))[1:5])
  }
}
emissionProbs <- matrix(emissionProbs, 10, 5, byrow=TRUE)

hmm = initHMM(states, symbols, startProbs, transitionProbs, emissionProbs)

res <- simHMM(hmm, 100)

z <- res$states
z <- z - as.numeric(z%%2==0)
z <- as.integer(z/2)
z <- z+1

x <- res$observation

### 2c) 
# Slide 8, lecture 5 (?)

