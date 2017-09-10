############# TASK ONE #############

library(bnlearn)

source ("import_data.R") 
ret <- load.data()

df <- ret$df 

header <- ret$header

sapply(df,class)

#View(df)

bn1 <- hc(df, start = NULL, whitelist = NULL, blacklist = NULL,
          score = NULL, debug = FALSE, restart = 0, perturb = 1, 
          max.iter = Inf, maxp = Inf, optimized = TRUE)

bn2 <- hc(df, start = NULL, whitelist = NULL, blacklist = NULL,
         score = NULL, debug = FALSE, restart = 0, perturb = 1, 
         max.iter = Inf, maxp = Inf, optimized = TRUE)

graphviz.plot(bn1)

all.equal(bn1,bn2) # Should be false

############# TASK TWO #############

score(bn1, df, type = "bde", iss = 1)
score(bn1, df, type = "bde", iss = 3)
score(bn1, df, type = "bde", iss = 5)


im_sa_si <- c(1:2000)

#bnBD <- hc(df, start = NULL, score = "bde", iss=2000, 
#           restart = 3, max.iter = Inf, 
#           maxp = Inf, optimized = TRUE)

no_of_arcs <- integer(length=0)
score <- integer(length=0)

for (i in im_sa_si){
  bnBD <- hc(df, start = NULL, score = "bde", iss=i, 
                   restart = 3, perturb = 3, max.iter = Inf,
                   maxp = Inf, optimized = TRUE)
  no_of_arcs <- c(no_of_arcs, nrow(bnBD$arcs))
  score <- c(score, score(bnBD,df, type = "bde", iss=i))
}

opt_sample_size <- alpha.star(bn1, df)

plot(im_sa_si,no_of_arcs, xlab="Imaginary sample size", ylab="Number of arcs")
abline(v = opt_sample_size, untf = FALSE, col="green")
plot(im_sa_si,score, type="l", xlab="Imaginary sample size", ylab="Score")
abline(v = opt_sample_size, untf = FALSE, col="green")



