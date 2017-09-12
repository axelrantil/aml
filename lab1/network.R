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
          score = NULL, debug = FALSE, restart = 0, perturb = 1, 
          max.iter = Inf, maxp = Inf, optimized = TRUE)

bn2 <- hc(df, start = NULL, whitelist = NULL, blacklist = NULL,
         score = NULL, debug = FALSE, restart = 0, perturb = 1, 
         max.iter = Inf, maxp = Inf, optimized = TRUE)

graphviz.plot(bn1)

all.equal(bn1,bn2) # Should be false

############# TASK TWO #############

im_sa_si <- c(1:1)

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
nrow(finalBN$arcs) #8 arcs should be fine
graphviz.plot(finalBN)

### Fit data into BN ### 

fittedMLE <- bn.fit(finalBN, df, method="mle") #done with MLE

### Compute approximate inference, example ###

dfOwn <- df[c(df$Housing=="own"),] # Set conditional to "own"

tableOwnManualMLE <- prop.table(table(dfOwn[,c("Good/BadCredit","CreditHistory")]),2) # Maximum likelihood method

tableOwnFittedMLE <- fittedMLE$`Good/BadCredit`$prob[,,2] # Extract probabilities Good/BadCredit given Housing="own"

tableOwnFittedMLE == tableOwnManualMLE # True for all entries

### Compute exact inference ###

jTree <- compile(as.grain(fittedMLE))

### Compare given no observed nodes ###

exact0 <- querygrain(jTree, "Good/BadCredit")

#approx0 <- prop.table(table(cpdist(fittedMLE)) # cpdist requires evidence!

### Compare given one observed node ###

jTree1 <- setFinding(jTree, nodes = "Housing", states = "own")

exact1 <- querygrain(jTree1, "Good/BadCredit")

approx1 <- prop.table(table(cpdist(fittedMLE, "Good/BadCredit", (Housing == "own"))))

difference1 <- (exact1$`Good/BadCredit`-approx1)

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

############# TASK FOUR #############

rDAG <- random.graph(LETTERS[1:5], num = 29281) # 4183 = 29281 / 7

rcpdag <- lapply(rDAG, function(x) cpdag(x))

orderedArcs <- rcpdag[[1]]$arcs

for (i in 2:29281){
  orderedArcs <- c(orderedArcs, rcpdag[[i]]$arcs)
}

uqrcpdag <- unique(orderedArcs) 

fractionSample <- length(uqrcpdag)/29281




