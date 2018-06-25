setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("./internal/R/recoding.R")
data<-read.csv("./internal/input_files/data.csv")

composite_indicator<-read.csv("./internal/input_files/composite_indicator_weighted_count")
