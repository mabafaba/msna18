rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
 setwd("../")
# getwd()

# clear/create folders
unlink("./output/modified_data/",recursive=TRUE) 
unlink("./output/percent_aggregations_raw_csv",recursive=TRUE) 
dir.create("./output",showWarnings = F)
dir.create("./output/modified_data",showWarnings = F)
dir.create("./output/percent_aggregations_raw_csv",showWarnings = F)
dir.create("./output/mean_aggregations_raw_csv",showWarnings = F)

#load dependencies
source("./internal/R/dependencies.R")

# LOAD INPUT 
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

# load analysis definitions

# aggregating all variables (direct reporting)
# list of variables to disaggregate by:
analysis_definition_aggregations<-read.csv("./internal/input_files/aggregate all variables.csv",stringsAsFactors = F)
# create a data analysis plan with all disaggregation variables as independent variable for all variables as dependent
analysis_plan_direct_reporting<-map_to_analysis_plan_all_vars_as_dependent("marital_status",data)

# APPLY ANALYSIS PLAN:
results<-apply_data_analysis_plan(data,analysis_plan_direct_reporting)


# RESHAPE OUTPUTS FOR MASTER TABLE:
all_summary_statistics<-results %>% lapply(function(x){x$summary.statistic}) %>% do.call(rbind,.) 

all_summary_statistics %>% write.csv("./output/master_table_long.csv")
all_summary_statistics$master_table_column_name<-paste(all_summary_statistics$dependent.var,all_summary_statistics$dependent.var.value,sep="::: ")
all_summary_statistics[,c("independent.var","independent.var.value","master_table_column_name","numbers")] %>% spread(key = c("master_table_column_name"),value = "numbers") %>% write.csv("master_table_wide.csv")
all_summary_statistics$master_table_column_name %>% table
all_summary_statistics[all_summary_statistics$master_table_column_name=="marital_status::: no_latrine_access_who::: married",]

# PLOTS
dir.create("./output/barcharts")

# grouped_barchart_percent(results[[123]]$summary.statistic,filename = "test.jpg")
# all_summary_statistics[all_summary_statistics$dependent.var=="respondent_age",] %>% barchart_average("test.jpg")

plots <- lapply(results, function(result){
  if(is.null(result$summary.statistic)|is.null(result$input.parameters$case)){print(result);return(NULL)}
  filename<-paste0("./output/barcharts/",paste(result$input.parameters %>% unlist,collapse="___"),".jpg")
  print(map_to_visualisation(result$input.parameters$case ))
  theplot<-map_to_visualisation(result$input.parameters$case )(result$summary.statistic,filename = filename)
})




# results[[123]]$summary.statistic
results[[123]]$input.parameters$case

extract_from_result_list<-function(resultlist,what){lapply(resultlist,function(x){
  if(what=="case"){return(x$input.parameters$case)}
}) %>% unlist
  }

numericalcases<-results %>% extract_from_result_list("case") %>% grep("numerical",.)

map_to_visualisation(results[[123]]$summary.statistic,filename = "./test.jpg")




