  # click the "Run" button above or
  # highlight the next lines and press ctrl+ENTER
  rm(list=ls());if(!("rstudioapi" %in% installed.packages()[,"Package"])){install.packages("rstudioapi")};require("rstudioapi");
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path));debugging_mode<-F;
  source("./internal/R/hype2.R")
  results$analysisplan_log[,c("independent.var","dependent.var","repeat.var")]
  results$analysisplan[,c("independent.var","dependent.var","repeat.var")]
  