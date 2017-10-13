############# TASK ONE #############

library(bnlearn)
library(gRain)

source ("import_data.R") 
ret <- load.data()

df <- ret$df 

header <- ret$header

sapply(df,class)

View(df)


bn1 <- hc(df, start = NULL, whitelist = NULL, blacklist = NULL,
          score = NULL, debug = FALSE, restart = 1, perturb = 4, 
          max.iter = Inf, maxp = Inf, optimized = TRUE)

bn2 <- hc(df, start = NULL, whitelist = NULL, blacklist = NULL,
         score = NULL, debug = FALSE, restart = 1, perturb = 4, 
         max.iter = Inf, maxp = Inf, optimized = TRUE)

graphviz.plot(bn1)

bn1 <- cpdag(bn1)

bn2 <- cpdag(bn2)

all.equal(bn1,bn2) # Should be false

############# TASK TWO #############

im_sa_si <- c(1:2000)

no_of_arcs <- integer(length=0)
score <- integer(length=0)
opt_sample_size <- numeric(length=0)

for (i in im_sa_si){
  bnBD <- hc(df, start = NULL, score = "bde", iss=i, 
                   restart = 3, perturb = 3, max.iter = Inf,
                   maxp = Inf, optimized = TRUE)
  no_of_arcs <- c(no_of_arcs, nrow(bnBD$arcs))
  score <- c(score, score(bnBD,df, type = "bde", iss=i))
  if (i==1 | i ==2000){
    opt_sample_size <- c(opt_sample_size, alpha.star(bnBD, df))
  }
}

graphviz.plot(bnBD)
plot(im_sa_si,no_of_arcs, xlab="Imaginary sample size", ylab="Number of arcs")
abline(v = opt_sample_size, untf = FALSE, col="green")
plot(im_sa_si,score, type="l", xlab="Imaginary sample size", ylab="Score")
abline(v = opt_sample_size, untf = FALSE, col="green")

############# TASK THREE #############

finalBN <- hc(df, start = NULL, score = "bde", iss=80, 
           restart = 0, perturb = 1, max.iter = Inf,
           maxp = Inf, optimized = TRUE)
nrow(finalBN$arcs) # 8 arcs represents a complex enough model
graphviz.plot(finalBN)

### Fit data into BN ### 

fittedMLE <- bn.fit(finalBN, df, method="mle") #done with MLE

### Compute approximate inference, example ###

dfOwn <- df[df$Housing=="own",] # Set conditional to "own"

tableOwnManualMLE <- prop.table(table(dfOwn[,c("Good/BadCredit","CreditHistory")]),2) # Maximum likelihood method

tableOwnFittedMLE <- fittedMLE$`Good/BadCredit`$prob[,,2] # Extract probabilities Good/BadCredit given Housing="own"

all.equal(tableOwnFittedMLE, tableOwnManualMLE) # True for all entries

### Compute exact inference ###

jTree <- compile(as.grain(fittedMLE))

### Compare given no observed nodes ###

fiveExact <- numeric(length=0)
fiveApprox <- numeric(length=0)

for (i in 1:5){
  exact0 <- querygrain(jTree, "Good/BadCredit")
  approx0 <- prop.table(table(cpdist(fittedMLE, "Good/BadCredit", evidence=TRUE)))
  fiveExact <- c(fiveExact, exact0$`Good/BadCredit`)
  fiveApprox <- c(fiveApprox, approx0)
}
matrix(fiveExact,5,2, byrow=TRUE, dimnames = list(c(1:5),c("good","bad")))
matrix(fiveApprox,5,2, byrow=TRUE, dimnames = list(c(1:5),c("good","bad")))

approx0

difference0 <- exact0$`Good/BadCredit` - approx0

### Compare given one observed node ###

jTree1 <- setFinding(jTree, nodes = "Housing", states = "own")

exact1 <- querygrain(jTree1, "Good/BadCredit")

approx1 <- prop.table(table(cpdist(fittedMLE, "Good/BadCredit", (Housing == "own"))))

difference1 <- exact1$`Good/BadCredit`-approx1

approx1

exact1$`Good/BadCredit`

difference1

### Compare given three observed nodes ###

jTree3 <- setFinding(jTree, nodes=c("MaritalAndSex","Housing", "Purpose"), states=c("A93", "own", "A40")) #Single male, new car

exact3 <- querygrain(jTree3, "Good/BadCredit")

approx3 <- prop.table(table(cpdist(fittedMLE, "Good/BadCredit", (MaritalAndSex=="A93" & Housing == "own" & Purpose=="A40"))))

difference3 = exact3$`Good/BadCredit` - approx3

approx3

exact3$`Good/BadCredit`

difference3

cr <- list(c("No observed nodes", "One observed node", "Three observed nodes"),c("good","bad"))

matrix(c(difference0, difference1, difference3),3,2,byrow=TRUE, dimnames = cr)

############# TASK FOUR #############

samples <- 50000

rDAG <- random.graph(LETTERS[1:5], num = samples, method = "melancon")

length(unique(rDAG))

rcpdag <- lapply(rDAG, function(x) cpdag(x))

length(unique(rcpdag))

fraction <- length(unique(rcpdag)) / length(unique(rDAG))

fraction




