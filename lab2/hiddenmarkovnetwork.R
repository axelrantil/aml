library(HMM)

states <- 0:9

stateNames <- paste(rep("z =",10),states)

#stepProbs <- matrix(0.5, 10, 2, dimnames = list(stateNames, c("stay", "step")))

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
emissionProbs

hmm = initHMM(stateNames, states, transProbs = stepProbs, emissionProbs = emissionProbs)

### TASK ONE ###

nSim <- 100

simHMM(hmm,nSim)

dishonestCasino

nSim = 2000
States = c("Fair", "Unfair")
Symbols = 1:6
transProbs = matrix(c(0.99, 0.01, 0.02, 0.98), c(length(States), 
                                                 length(States)), byrow = TRUE)
