# Rstudio API Code
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Libraries
library(tidyverse)
library(haven)
library(foreign)
library(xgboost)
library(parallel)
library(doParallel)
library(microbenchmark)
library(tictoc)

# Data Import and Cleaning 
data <- read.spss("../data/GSS2006.sav")

data_tbl <-data %>%
  as_tibble %>%
  select(starts_with("BIG5"), HEALTH) %>%
  mutate_all(~ na_if(., "IAP")) %>%
  mutate_all(~ na_if(., "CANT CHOOSE")) %>%
  mutate_all(~ na_if(., "NA")) %>%
  mutate_all(~ na_if (., "DK")) %>%
  mutate_all(as.numeric)

missing_big5_tbl <- data_tbl %>%
  select(starts_with("BIG5")) %>%
  mutate_all(~ ifelse(is.na(.), 1, 0))
data_tbl$missings <- rowSums(missing_big5_tbl)

final_tbl <- data_tbl %>%
  filter(missings <7) %>%
  select(-missings)
