rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
 setwd("../")
getwd()

# clear/create folders
unlink("./output/modified_data/",recursive=TRUE) 
unlink("./output/percent_aggregations_raw_csv",recursive=TRUE) 
dir.create("./output",showWarnings = F)
dir.create("./output/modified_data",showWarnings = F)
dir.create("./output/percent_aggregations_raw_csv",showWarnings = F)
dir.create("./output/mean_aggregations_raw_csv",showWarnings = F)

#load dependencies
source("./internal/R/dependencies.R")
source("./internal/R/hypegrammar_dependencies.R")

hypegrammaR_source_path<-paste("./internal/R/hypegrammaR/")
hypegrammaR_source_files<-list.files(hypegrammaR_source_path) %>% paste0(hypegrammaR_source_path,.)
sapply(hypegrammaR_source_files,source,verbose=F)
# source("./internal/R/recoding.R")
source("./internal/R/composite_indicator_weighted_count.R")
source("./internal/R/survey_design.R")
# source("./internal/R/recoding.R")
# source("./internal/R/aggregation.R")
# source("./internal/R/KI_aggregation.R")
# 
# source("./internal/R/errors.R")
# source("./internal/R/summary_statistics.R")
source("./internal/R/load_questionnaire.R")
source("./internal/R/load_analysis_definitions.R")

# LOAD INPUT DATA AND META
# data 
data<-read.csv("./internal/input_files/data.csv")
# data parameters
data_parameters<-read.csv("./internal/input_files/data_parameters.csv",stringsAsFactors = F)
composite_indicators_weighted_counts<-load_composite_indicator_definition_weighted_count()
data_with_composite_indicators<-add_variable_indicators_weighted_count(data,composite_indicators_weighted_counts)

# load samplingframe (only if data_parameters says it's a stratified sample)
if(data_parameters$stratified=="yes"){sf<-load_samplingframe("./internal/input_files/sampling_frame.csv",
                                                             data.stratum.column = data_parameters$stratum.name.variable,return.stratum.populations = F
                                                             
)}

# load kobo tool:
questionnaire<-load_questionnaire(data,questions.file = "./internal/input_files/kobo_questions.csv",
                                  choices.file = "./internal/input_files/kobo_choices.csv",
                                  choices.label.column.to.use = data_parameters$choices.label.column.to.use)

# LOAD ANALYSIS DEFINITIONS

# aggregating all variables (direct reporting)
analysis_definition_aggregations<-read.csv("./internal/input_files/aggregate all variables.csv",stringsAsFactors = F)
analysis_plan_direct_reporting<-map_to_analysis_plan_all_vars_as_dependent(analysis_definition_aggregations,data)
results<-apply_data_analysis_plan(data,analysis_plan_direct_reporting[c(c(1:10),8   ,13  ,814  ,913  ,816  ,915 ,1315  ,818  ,917 ,1317),])

results %>% lapply(function(x){x$summary.statistic}) %>% do.call(rbind,.) ->sumstatsdf


results[[1000]]$summary.statistic



cn<-lapply(results,function(x){
  x$summary.statistic %>% names %>% paste(collapse="- -")
})


cn %>% unlist -> cn
cn %>% unique
results[[((cn=="dependent.var.value- -independent.var.value- -num- -se- -min- -max") %>% which)[1]]]


empty_result("sdf")
compact(colnames)
require("dplyr")
require("purrr")


cn<-colnames %>% lapply(function(x){paste(x,collapse="...")}) %>% unlist

  
  
colnames %>% lapply(length) %>% unlist %>% table

rm(colnames)


results$X_uuid















