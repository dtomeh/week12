# Rstudio API Code
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Libraries
library(tidyverse)
library(caret)
library(haven)
library(xgboost)
library(parallel)
library(doParallel)
library(microbenchmark)
library(tictoc)

# Data Import and Cleaning 
  week12 <- read_sav("../data/GSS2006.sav")
  week12_tbl <-as_tibble(week12) %>% 
    select(HEALTH, BIG5A1, BIG5B1, BIG5C1, BIG5D1, BIG5E1, BIG5A2, BIG5B2, BIG5C2, BIG5D2, BIG5E2) %>%
    filter(rowSums(is.na(.[,1:11]))!=11)  %>%
    filter(rowSums(is.na(.[,2:11]))!=10) %>%
    filter(is.na(HEALTH)==FALSE) %>%
    mutate_all(as.numeric)

#Analysis 
exec_time_np <- system.time({
xgb_mod <- train(
  HEALTH ~ .^3, 
  week12_tbl, 
  method="xgbTree",
  preProcess=c("center", "scale", "zv", "medianImpute"), 
  trControl= trainControl(method="cv", number = 10, verboseIter = T), 
  tuneLength=2,
  na.action = na.pass
 
)
})

detectCores()
local_cluster <-makeCluster(3)
registerDoParallel(local_cluster)

exec_time_p <- system.time({xgb_mod2 <- train(
  HEALTH ~ .^3, 
  week12_tbl, 
  method="xgbTree",
  preProcess=c("center", "scale", "zv", "medianImpute"), 
  trControl= trainControl(method="cv", number = 10, verboseIter = T), 
  tuneLength=2,
  na.action = na.pass
  
)
})
stopCluster(local_cluster) 
registerDoSEQ()

## Finding the difference in time between the parallelized and non-parallelized xgb models
exec_time_np-exec_time_p
# In the non-parallelized version of the model the user time is 112.120, the system time is 2.857, and the total time elapsed is 116.852. 
# In the parallelized version of the model the user time is 1.458 seconds, the system time is .0327, and the total time elapsed is 70.202