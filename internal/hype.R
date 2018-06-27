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
source("./internal/R/hypegrammar_dependencies.R")
hypegrammaR_source_path<-paste("./internal/R/hypegrammaR/")
hypegrammaR_source_files<-list.files(hypegrammaR_source_path) %>% paste0(hypegrammaR_source_path,.)
sapply(hypegrammaR_source_files,source)
# source("./internal/R/recoding.R")
source("./internal/R/composite_indicator_weighted_count.R")
# source("./internal/R/survey_design.R")
# source("./internal/R/recoding.R")
# source("./internal/R/aggregation.R")
# source("./internal/R/KI_aggregation.R")
# 
# source("./internal/R/errors.R")
# source("./internal/R/summary_statistics.R")
# source("./internal/R/load_questionnaire.R")
source("./internal/R/load_analysis_definitions.R")







# LOAD INPUTS
# data 
data<-read.csv("./internal/input_files/data.csv")
# data parameters
data_parameters<-read.csv("./internal/input_files/data_parameters.csv",stringsAsFactors = F)
ci_weighted_count_def<-load_composite_indicator_definition_weighted_count()
data_with_composite_indicators<-add_variable_indicators_weighted_count(data,ci_weighted_count_def)


# load samplingframe (only if data_parameters says it's a stratified sample)
if(data_parameters$stratified=="yes"){sf<-load_samplingframe("./internal/input_files/sampling_frame.csv",
                                                             data.stratum.column = data_parameters$stratum.name.variable,return.stratum.populations = F
                                                             
)}

# load kobo tool:

questionnaire<-load_questionnaire(data,questions.file = "./internal/input_files/kobo_questions.csv",
                                  choices.file = "./internal/input_files/kobo_choices.csv",
                                  choices.label.column.to.use = data_parameters$choices.label.column.to.use)


analysis_definition_aggregations<-read.csv("./internal/input_files/aggregate all variables.csv",stringsAsFactors = F)




produce_analysis_plan_direct_report_all_variables("enumerator",data,"numeric")

