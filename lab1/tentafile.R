library(bnlearn)
data(asia) 
### Structure learning, Hill Climbing (not asymptotically correct):
### Start with empty graph
### Repeat until no change occurs
###     Add, remove or reverse any edge in G that improves the Bayesian score the most
struct <- hc(asia, restart = 100, perturb = 2, score = "bde", iss=1.503655)

### Compute score
score <- score(struct, asia, type="bde", iss = 1)

### Visualize
graphviz.plot(struct)

### Compute optimal iss given assuming uniform prior and given data and structure
alpha.star(dag, asia, debug = FALSE)

graphviz.plot(dag)

### Fit data into network (according to MLE)
fittedBN <- bn.fit(struct, asia, method="mle")

### Exact inference (using LS) and set findings
jTree <- compile(as.grain(fittedBN))
jTreeWithFindings <- setFinding(jTree, nodes=c("X", "B"), states=c("yes", "yes")) 

exact <- querygrain(jTreeWithFindings, "A")

### Approximate inference
approximate <- prop.table(table(cpdist(fittedBN, "A", (X == "yes" & B == "yes"))))


