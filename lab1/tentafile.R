library(bnlearn)

data(asia) 

approx <- hc(asia, restart = 100, perturb = 2, score = "bde", iss=20)

jTree <- compile(as.grain(fittedMLE))

jTree3 <- setFinding(jTree, nodes=c("MaritalAndSex","Housing", "Purpose"), states=c("A93", "own", "A40")) 

fittedMLE <- bn.fit(approx, asia, method="mle")
