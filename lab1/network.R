library(bnlearn)

source ("import_data.R") 

ret <- load.data()

df <- ret$df 

df2 <- ret$df

header <- ret$header

sapply(df,class)

#head(df)

View(df)

#bn <- empty.graph(header, num = 5)

bn <- hc(df, start = NULL, whitelist = NULL, blacklist = NULL,
          score = NULL, debug = FALSE, restart = 3, perturb = 3, 
          max.iter = Inf, maxp = Inf, optimized = TRUE)

bn2 <- hc(df2, start = NULL, whitelist = NULL, blacklist = NULL,
         score = NULL, debug = FALSE, restart = 3, perturb = 3, 
         max.iter = Inf, maxp = Inf, optimized = TRUE)

graphviz.plot(bn)

all.equal(bn,bn2) # Should be false






