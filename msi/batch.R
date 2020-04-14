# Libraries
library(caret) #for train
library(parallel) #to parallelize
library(doParallel) #to parallelize
library(xgboost) #xgb model
library(readr) #write csv
library(tibble) #save as tibble
library(haven) #import spss

# Data Import and Cleaning 

#using read.sav eliminates the need to remove strange values and reformat missingness and strange values as NAs, it does it automatically. It also refactors the values of the 5 point scale to numeric. Its ultimately about .3 of second faster, but does involve a lot less code. 

data_int <- data_int <- read_sav("GSS2006.sav")
#removing the need for tidyverse eliminated 3 seconds on average from the data import and cleaning portion of the code

data_int_tbl <- data_int[,c("HEALTH", "BIG5A1", "BIG5A2", "BIG5B1", "BIG5B2", "BIG5C1", "BIG5C2", "BIG5D1", "BIG5D2", "BIG5E1", "BIG5E2")]


data_int_tbl$HEALTH <- as.numeric(data_int_tbl$HEALTH)
data_int_tbl$BIG5A1 <- as.numeric(data_int_tbl$BIG5A1)
data_int_tbl$BIG5A2 <- as.numeric(data_int_tbl$BIG5A2)
data_int_tbl$BIG5B1 <- as.numeric(data_int_tbl$BIG5B1)
data_int_tbl$BIG5B2 <- as.numeric(data_int_tbl$BIG5B2)
data_int_tbl$BIG5C1 <- as.numeric(data_int_tbl$BIG5C1)
data_int_tbl$BIG5C2 <- as.numeric(data_int_tbl$BIG5C2)
data_int_tbl$BIG5D1 <- as.numeric(data_int_tbl$BIG5D1)
data_int_tbl$BIG5D2 <- as.numeric(data_int_tbl$BIG5D2)
data_int_tbl$BIG5E1 <- as.numeric(data_int_tbl$BIG5E1)
data_int_tbl$BIG5E2 <- as.numeric(data_int_tbl$BIG5E2)

#converting each variable to numeric separately as opposed to as_tibble(lapply(data_int_tbl, as.numeric)) eliminated 1.5 seconds from the overall time of data import and cleaning


int_missing <- data_int_tbl[!is.na(data_int_tbl$HEALTH),]
int_missing$big5Sums <- rowSums(is.na(int_missing))
int_no_missing <- int_missing[!(int_missing$big5Sums==10),]

#using system.time instead of microbenchmark saves a lot of time since iterations of the code are eliminated.Since the goal is to minimize the run time of the overall code while saving the run time of this chunk, i decied to use system.time. 

#Analysis 
exec_time_nonparallelized <- system.time({
  xgb_mod <- train(
    HEALTH ~ .^3, 
    int_no_missing, 
    method="xgbTree",
    preProcess=c("center", "scale", "zv", "medianImpute"), 
    trControl= trainControl(method="cv", number = 10, verboseIter = T), 
    tuneLength=3,
    na.action = na.pass
    
  )
})

local_cluster <-makeCluster(60)
registerDoParallel(local_cluster)

exec_time_parallelized <- system.time({xgb_mod2 <- train(
  HEALTH ~ .^3, 
  int_no_missing, 
  method="xgbTree",
  preProcess=c("center", "scale", "zv", "medianImpute"), 
  trControl= trainControl(method="cv", number = 10, verboseIter = T), 
  tuneLength=8,
  na.action = na.pass
  
)
})
stopCluster(local_cluster) 
registerDoSEQ()

## Finding the difference in time between the parallelized and non-parallelized xgb models
exec_time_nonparallelized-exec_time_parallelized
# In the non-parallelized version of the model the user time is 13.785, the system time is .416, and the total time elapsed is 15.299. 
# In the parallelized version of the model the user time is .757 seconds, the system time is .039, and the total time elapsed is 10.323
write_csv(tibble(nonparallelized = exec_time_nonparallelized, paralellized=exec_time_parallelized), "../batch.csv", col_names=TRUE, append=TRUE)
