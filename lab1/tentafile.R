######################## LAB 1 ########################

library(bnlearn)
library(gRain)
data(asia) 

### Custom net
dag = model2network("[S][R][WS|S][WG|S:R]")
graphviz.plot(dag)

### Structure learning, Hill Climbing (not asymptotically correct):
### Start with empty graph
### Repeat until no change occurs
###     Add, remove or reverse any edge in G that improves the Bayesian score the most
### iss in this case is from alpha.star of a given model
struct <- hc(asia, restart = 100, perturb = 2, score = "bde", iss=1.503655)

### Compute score
score <- score(struct, asia, type="bde", iss = 1.503655)

### Visualize
graphviz.plot(struct)

### Compute optimal iss assuming uniform prior (BDeu) and given data and structure
alpha.star(dag, asia, debug = FALSE)

graphviz.plot(dag)

### Fit data into network (according to MLE)
fittedBN <- bn.fit(struct, asia, method="mle")

### Exact inference (using LS) and set findings
jTree <- compile(as.grain(fittedBN))
jTreeWithFindings <- setEvidence(jTree, nodes=c("X", "B"), states=c("yes", "yes")) 

exact <- querygrain(jTreeWithFindings, "A")

### Approximate inference
approximate <- prop.table(table(cpdist(fittedBN, "A", (X == "yes" & B == "yes"))))


### Probability distributions given
asia.dag = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]")
lv = c("yes", "no")
A.prob = array(c(0.01, 0.99), dim = 2, dimnames = list(A = lv))
S.prob = array(c(0.01, 0.99), dim = 2, dimnames = list(A = lv))
T.prob = array(c(0.05, 0.95, 0.01, 0.99), dim = c(2, 2),
               dimnames = list(T = lv, A = lv))
L.prob = array(c(0.1, 0.9, 0.01, 0.99), dim = c(2, 2),
               dimnames = list(L = lv, S = lv))
B.prob = array(c(0.6, 0.4, 0.3, 0.7), dim = c(2, 2),
               dimnames = list(B = lv, S = lv))
D.prob = array(c(0.9, 0.1, 0.7, 0.3, 0.8, 0.2, 0.1, 0.9), dim = c(2, 2, 2),
               dimnames = list(D = lv, B = lv, E = lv))
E.prob = array(c(1, 0, 1, 0, 1, 0, 0, 1), dim = c(2, 2, 2),
               dimnames = list(E = lv, T = lv, L = lv))
X.prob = array(c(0.98, 0.02, 0.05, 0.95), dim = c(2, 2),
               dimnames = list(X = lv, E = lv))
cpt = list(A = A.prob, S = S.prob, T = T.prob, L = L.prob, B = B.prob,
           D = D.prob, E = E.prob, X = X.prob)
bn = custom.fit(asia.dag, cpt)


M = paste("[X1][X3][X5][X6|X8][X2|X1][X7|X5][X4|X1:X2]",
          "[X8|X3:X7][X9|X2:X7][X10|X1:X9]", sep = "")
dag = model2network(M)
# Markov blanket - the set of nodes that graphically isolates a target node from the rest of the dag i.e. parents, children and other parents of children, node itself not incl.
mb(dag, node = "X9")

graphviz.plot(dag)


######################## LABB 2 ########################
library(HMM)
library(entropy)
library(data.table)

### TASK ONE ###

states <- paste(rep("z =",10), 0:9)

stateNames <- paste(rep("z =",10), 0:9)

stepProbs <- cbind(c(numeric(9),1/2),1/2*diag(x=10)) 

stepProbs <- stepProbs[,-11] + 1/2*diag(10) 

dimnames(stepProbs) <-list(as.character(0:9), as.character(0:9))

emissionProbs <- c(1/5, 1/5, 1/5, 0, 0, 0, 0, 0, 1/5, 1/5)

for (i in 1:10) {
  emissionProbs <- c(emissionProbs, shift(tail(emissionProbs,n=10), n=1, fill = tail(emissionProbs,n=1)))
}

emissionProbs <- matrix(emissionProbs, 10, 10, byrow=TRUE, dimnames=list(stateNames, paste(rep("p(x=",10),as.character(0:9),rep(")",10))))

hmm = initHMM(stateNames, states, transProbs = stepProbs, emissionProbs = emissionProbs)

### TASK TWO ###
nSim <- 100

sim <- simHMM(hmm,nSim)

obs <- sim$observation

trueStates <- sim$states

### Forward (utan log) ###

normfor <- function (hmm, observation) 
{
  hmm$transProbs[is.na(hmm$transProbs)] = 0
  hmm$emissionProbs[is.na(hmm$emissionProbs)] = 0
  nObservations = length(observation)
  nStates = length(hmm$States)
  f = array(NA, c(nStates, nObservations))
  dimnames(f) = list(states = hmm$States, index = 1:nObservations)
  for (state in hmm$States) {
    f[state, 1] = log(hmm$startProbs[state] * hmm$emissionProbs[state, 
                                                                observation[1]])
  }
  for (k in 2:nObservations) {
    for (state in hmm$States) {
      logsum = -Inf
      for (previousState in hmm$States) {
        temp = f[previousState, k - 1] + log(hmm$transProbs[previousState, 
                                                            state])
        if (temp > -Inf) {
          logsum = temp + log(1 + exp(logsum - temp))
        }
      }
      f[state, k] = log(hmm$emissionProbs[state, observation[k]]) + 
        logsum
    }
    f[,k-1] <- exp(f[,k-1])/sum(exp(f[,k-1]))
  }
  f[,nObservations] <- exp(f[,nObservations]) / sum(exp(f[,nObservations]))
  return(f)
}

