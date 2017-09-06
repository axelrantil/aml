library(bnlearn)

source ("import_data.R") 

ret <- load.data()

df <- ret$df 

header <- ret$header

sapply(df,class)

#head(df)

View(df)

#bn <- empty.graph(header, num = 5)

bn1 <- hc(df, start = NULL, whitelist = NULL, blacklist = NULL,
          score = NULL, debug = FALSE, restart = 0, perturb = 1, 
          max.iter = Inf, maxp = Inf, optimized = TRUE)

bn2 <- hc(df, start = NULL, whitelist = NULL, blacklist = NULL,
          score = NULL, debug = FALSE, restart = 0, perturb = 1, 
          max.iter = Inf, maxp = Inf, optimized = TRUE)
  




