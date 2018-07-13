# click the "Run" button above or
# highlight the next line and press ctrl+ENTER
rm(list=ls())
if(!("rstudioapi" %in% installed.packages()[,"Package"])){install.packages("rstudioapi")};require("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("./internal/hype.R")
"./internal/R/dependencies.R"