library(HMM)

states <- c("rainy1", "rainy2", "sunny1", "sunny2")

symbols <- c("rainy", "sunny")

startProbs <- c(1,0,0,0)

transProbs <- c(0,  0.5,  0.5, 0,
                0, 0.75, 0.25, 0,
                0.5,  0, 0 , 0.5,
                0.25, 0 ,0, 0.75)

transProbs <- matrix(transProbs, 4,4, byrow=TRUE)

dimnames(transProbs) <- list(states, states)

emissionProbs <- c(0.9, 0.1,
                   0.9, 0.1,
                   0.1, 0.9,
                   0.1, 0.9)
emissionProbs <- matrix(emissionProbs, 4, 2, byrow=TRUE)

model <- initHMM(states, symbols, startProbs, transProbs, emissionProbs)

simHMM(model,10)
