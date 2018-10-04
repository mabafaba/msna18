rm(list=ls());if(!("rstudioapi" %in% installed.packages()[,"Package"])){install.packages("rstudioapi")};require("rstudioapi");
setwd(dirname(rstudioapi::getActiveDocumentContext()$path));setwd("./../../../")
require("testthat")
source("./internal/R/unit_tests/test_utilities.R")
source("./internal/R/120 - dependencies.R")
test_file(path = "./internal/R/unit_tests/test_questionnaire_skiplogic.R")



