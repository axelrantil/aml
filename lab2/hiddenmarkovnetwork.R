library(HMM)

states <- 0:9

statenames <- paste(rep("z =",10),states)

stepProbability <- matrix(0.5, 10, 2, dimnames = list(statenames, c("stay", "leave")))

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
                        dimnames=list(statenames, paste(rep("p(x= ",10),as.character(0:9),rep(")",10))))
emissionProbs



a <- rep("e",10)
b <- as.character(0:9)
 paste(a,b)

### TASK ONE ###

nSim <- 100

dishonestCasino

nSim = 2000
States = c("Fair", "Unfair")
Symbols = 1:6
transProbs = matrix(c(0.99, 0.01, 0.02, 0.98), c(length(States), 
                                                 length(States)), byrow = TRUE)
emissionProbs = matrix(c(rep(1/6, 6), c(rep(0.1, 5), 0.5)), 
                       c(length(States), length(Symbols)), byrow = TRUE)
emissionProbs
