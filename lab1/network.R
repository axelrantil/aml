############# TASK ONE #############

load.data <- function() {
  
  df <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data", header = FALSE, sep = " ")
  
  sapply(df,class)
  
  df$V21 <- as.factor(df$V21)
  
  header <- c("Checking account",
              "Duration",
              "CreditHistory",
              "Purpose",
              "Amount credit",
              "Savings account",
              "EmploymentSince",
              "Installment rate",
              "MaritalAndSex",
              "Guarantors",
              "Residence since",
              "Owned property",
              "Age",
              "Installment plans",
              "Housing",
              "Current credit here",
              "Job",
              "People care for",
              "Telephone",
              "Foreign worker",
              "Good/BadCredit"
  )
  
  colnames(df) <- header
  
  df <- df[, c("CreditHistory", "Purpose", "EmploymentSince", "MaritalAndSex", "Job", "Housing", "Good/BadCredit")] #Discrete
  
  require(plyr)
  
  df$CreditHistory <- mapvalues(df$CreditHistory,
                                from=c("A30", "A31", "A32", "A33", "A34"),
                                to=c("No History/Good", "this bank payed back",
                                     "existing and on time", "delay in payments",
                                     "critical/other credits"))
  
  df$Housing <- mapvalues(df$Housing,
                          from=c("A151", "A152", "A153"),
                          to=c("rent", "own", "free"))
  
  df$`Good/BadCredit` <- mapvalues(df$`Good/BadCredit`,
                                   from=c("1", "2"),
                                   to=c("good", "bad"))
  
  return(list("df"=df, "header"=header))
}


library(bnlearn)
library(gRain)

ret <- load.data()

df <- ret$df 

header <- ret$header

sapply(df,class)

View(df)

bn1 <- hc(df, start = NULL, whitelist = NULL, blacklist = NULL, ##BIC by default
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

# Extract probabilities Good/BadCredit given Housing="own"

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

samples <- 1000000

### burn.in and every as high as possible
rDAG <- random.graph(LETTERS[1:5], num = samples, method = "melancon", burn.in = 150, every = 2)

length(unique(rDAG))

rcpdag <- lapply(rDAG, function(x) cpdag(x))

length(unique(rcpdag))

fraction <- length(unique(rcpdag)) / length(unique(rDAG))

fraction




