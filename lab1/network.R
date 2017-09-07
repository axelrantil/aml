############# TASK ONE #############

library(bnlearn)

source ("import_data.R") 
ret <- load.data()

df <- ret$df 

header <- ret$header

sapply(df,class)

#View(df)



bn1 <- hc(df, start = NULL, whitelist = NULL, blacklist = NULL,
          score = NULL, debug = FALSE, restart = 3, perturb = 3, 
          max.iter = Inf, maxp = Inf, optimized = TRUE)

bn2 <- hc(df, start = NULL, whitelist = NULL, blacklist = NULL,
         score = NULL, debug = FALSE, restart = 3, perturb = 3, 
         max.iter = Inf, maxp = Inf, optimized = TRUE)


graphviz.plot(bn1)

all.equal(bn1,bn2) # Should be false

############# TASK TWO #############

score(bn1, df, type = "bde", iss = 1)
score(bn1, df, type = "bde", iss = 3)
score(bn1, df, type = "bde", iss = 5)


im_sa_si <- c(2:20)

bnBD <- hc(df, start = NULL, score = "bde", iss=1, 
           restart = 3, max.iter = Inf, 
           maxp = Inf, optimized = TRUE)

for (i in im_sa_si){
  bnBD <- c(bnBD, hc(df, start = NULL, score = "bde", iss=i, 
             restart = 3, perturb = 3, max.iter = Inf, 
             maxp = Inf, optimized = TRUE))
}




