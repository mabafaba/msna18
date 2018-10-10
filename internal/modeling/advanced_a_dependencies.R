rm(list=ls());if(!("rstudioapi" %in% installed.packages()[,"Package"])){install.packages("rstudioapi")};require("rstudioapi");
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

install.packages("multicomp")
install.packages("sjPlot")
install.packages("effects")
install.packages('TMB', type = 'source')
install.packages("installr")
install.packages("outliers")

setwd("../..")
getwd()
suppressMessages(source("./internal/R/120 - dependencies.R"))

###load data

data<-read.csv("./internal/modeling/MCNA_centre_final.csv",stringsAsFactors = F) %>% to_alphanumeric_lowercase_colnames_df
parameters <- list()
parameters$weight <- "normalised_weight"
