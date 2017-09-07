load.data <- function() {
  
  df <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data", header = FALSE, sep = " ")

  
  #Gör detta till en vector som gör om alla samtidigt
  
  df$V2 <- as.numeric(df$V2)
  
  df$V5 <- as.numeric(df$V5)
  
  df$V8 <- as.numeric(df$V8)
  
  df$V8 <- df$V8/100
  
  df$V11 <- as.numeric(df$V11)
  
  df$V13 <- as.numeric(df$V13)
  
  df$V16 <- as.numeric(df$V16)
  
  df$V18 <- as.numeric(df$V18)
  
  df$V21 <- as.factor(df$V21)
  
  
  sapply(df,class)
  
  header <- c("Checking account",
              "Duration",
              "Credit history",
              "Purpose",
              "Amount credit",
              "Savings account",
              "Employment since",
              "Installment rate",
              "Marital and sex",
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
              "Good/bad credit"
              )
  
  colnames(df) <- header
  
  #df <- df[, c("Duration", "Credit history", "Purpose", "Amount credit", "Employment since", "Marital and sex", "Age", "Job", "Housing")] #Mixed
  
  #Gör detta till en vector som tar bort alla kontinuerliga
  
  df <- df[, c("Credit history", "Purpose", "Employment since", "Marital and sex", "Job", "Housing")] #Discrete
  
  return(list("df"=df, "header"=header))
}
