library(HMM)
library(entropy)

### TASK ONE ###

states <- paste(rep("z =",10), 0:9)

stateNames <- paste(rep("z =",10), 0:9)

stepProbs <- cbind(c(numeric(9),1/2),1/2*diag(x=10)) 

stepProbs <- stepProbs[,-11] + 1/2*diag(10) 

dimnames(stepProbs) <-list(as.character(0:9), as.character(0:9))

emissionProbs <- matrix(c(1/5, 1/5, 1/5, 0, 0, 0, 0, 0, 1/5, 1/5,
                          1/5, 1/5, 1/5, 1/5, 0, 0, 0, 0, 0, 1/5,
                          1/5, 1/5, 1/5, 1/5, 1/5, 0, 0, 0, 0, 0,
                          0, 1/5, 1/5, 1/5, 1/5, 1/5, 0, 0, 0, 0,
                          0, 0, 1/5, 1/5, 1/5, 1/5, 1/5, 0, 0, 0,
                          0, 0, 0, 1/5, 1/5, 1/5, 1/5, 1/5, 0, 0,
                          0, 0, 0, 0, 1/5, 1/5, 1/5, 1/5, 1/5, 0,
                          0, 0, 0, 0, 0, 1/5, 1/5, 1/5, 1/5, 1/5,
                          1/5, 0, 0, 0, 0, 0, 1/5, 1/5, 1/5, 1/5,
                          1/5, 1/5, 0, 0, 0, 0, 0, 1/5, 1/5, 1/5), 
                        10, 10, byrow=TRUE, 
                        dimnames=list(stateNames, paste(rep("p(x= ",10),as.character(0:9),rep(")",10))))


hmm = initHMM(stateNames, states, transProbs = stepProbs, emissionProbs = emissionProbs)

### TASK TWO ###

nSim <- 100

sim <- simHMM(hmm,nSim)

obs <- sim$observation

trueStates <- sim$states

### TASK THREE, FOUR AND FIVE ###

### FORWARD/ALPHA/FILTERED ###

forwardAlpha = exp(forward(hmm, obs))

filteredCalc <- paste(rep("z =",10), max.col(t(prop.table(forwardAlpha,2)))-1)

filteredAcc <- sum(filteredCalc == sim$states)/length(filteredCalc)

filteredAcc

### SMOOTHED (manually) ###

backwardBeta <- exp(backward(hmm, obs))

smoothingCalcMan <- paste(rep("z =",10), max.col(t(prop.table(forwardAlpha*backwardBeta,2)))-1)

smoothingAccMan <- sum(smoothingCalcMan == sim$states)/length(smoothingCalcMan)

smoothingAccMan

### SMOOTHED (using posterior) ###

smoothingCalc <- paste(rep("z =", 10),max.col(t(posterior(hmm, obs)))-1)

smoothingAcc <- sum(smoothingCalc == sim$states)/length(smoothingCalc)

smoothingAcc

### PATH VITERBI ###

pathCalc <- viterbi(hmm, obs)

viterbiAcc <- sum(pathCalc == sim$states)/length(pathCalc)

viterbiAcc

### Check last index, step 100 ###

prop.table(forwardAlpha*backwardBeta,2)[,100] == prop.table(forwardAlpha,2)[,100] # True for all states

### Question 1: Smoothed is using more information: the whole set of observed emission x[0:T] whereas filtered only uses data emitted up to that point x[0:t]

### Question 2: The viterbi algoritm generates a most likely path, i.e. it has to abide to the constraints of the transitions between states in the hidden variables
### The smooth approximation approximate the most likely state, and can therefor make "illegal" moves from one state to another when predicting current state.

### Task 6 ###

entropyFiltering <- apply(prop.table(forwardAlpha, 2), 2, entropy.empirical)

entropySmoothing <- apply(prop.table(forwardAlpha*backwardBeta,2), 2, entropy.empirical)

sum(entropyFiltering < entropySmoothing)

matplot(1:100, entropyFiltering, type = "l", col = "red", xlab = "Step", ylab = "Entropy")
lines(entropySmoothing,type="l",col="blue")
legend(10, 1.6, legend=c("Filtering", "Smoothing"),
       col=c("red", "blue"), lty=1:1)



### Task 7 ###

step100 <- prop.table(forwardAlpha,2)[,100]

step101 <- 0.5*step100 + 0.5*c(step100[10], step100[1:9])

