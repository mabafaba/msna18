rm(list=ls());if(!("rstudioapi" %in% installed.packages()[,"Package"])){install.packages("rstudioapi")};require("rstudioapi");
setwd(dirname(rstudioapi::getActiveDocumentContext()$path));setwd("./../../../")
require("testthat")
source("./internal/R/unit_tests/test_utilities.R")
source("./internal/R/120 - dependencies.R")

is_in_test_dir<-(getwd() %>% gsub("\\/$","",.) %>% strsplit("/") %>% .[[1]] %>% last)=="unit_tests"
if(!is_in_test_dir){setwd("./internal/R/unit_tests/")}

example<-load.example("example1",F)
data <- example$data
tf <- example$tf
questionnaire <- example$questionnaire
design <- svydesign(~0, data = data)

test_file(path = "./test_questionnaire_skiplogic.R")
test_file(path = "./test_summary_statistics_categorical.R")
test_file(path = "./test_summary_statistics_numerical.R")
test_file(path = "./test_load_questionnaire_m.R")

