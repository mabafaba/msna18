rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd("../")
# getwd()
if(!exists("debugging_mode")){
  debugging_mode<-FALSE
}


# clear/create folders
unlink("./output/modified_data/",recursive=TRUE) 
unlink("./output/percent_aggregations_raw_csv",recursive=TRUE)
unlink("./output/barcharts",recursive=TRUE) 

dir.create("./output",showWarnings = F)
dir.create("./output/modified_data",showWarnings = F)
dir.create("./output/percent_aggregations_raw_csv",showWarnings = F)
dir.create("./output/mean_aggregations_raw_csv",showWarnings = F)
dir.create("./output/barcharts",showWarnings = F)

#load dependencies
  source("./internal/R/dependencies.R")

# LOAD INPUT 
# data 
data<-read.csv("./internal/input_files/data.csv",stringsAsFactors = F) %>% to_alphanumeric_lowercase_colnames_df
missing_data_to_NA<-function(data){
  lapply(data,function(x){
    replace(x,which(x %in% c("","N/A","#N/A","NA")),NA)    
  }) %>% as.data.frame(stringsAsFactors=T)# survey needs with factors.
}

## Loading cluster sampling units
cluster_formula <- load_cluster_sampling_units()

# data parameters
data_parameters<-read.csv("./internal/input_files/data_parameters.csv",stringsAsFactors = F) 
data_parameters$stratum.name.variable <- data_parameters$stratum.name.variable %>% to_alphanumeric_lowercase

# load samplingframe (only if data_parameters says it's a stratified sample)
if(data_parameters$stratified[1]=="yes"){sf<-load_samplingframe("./internal/input_files/sampling_frame.csv",
                                                             data.stratum.column = data_parameters$stratum.name.variable[1],
                                                             return.stratum.populations = T)}
# undebug(add_variable_indicators_weighted_count)
questionnaire<-load_questionnaire(data,questions.file = "./internal/input_files/kobo_questions.csv",
                                  choices.file = "./internal/input_files/kobo_choices.csv",
                                  choices.label.column.to.use = data_parameters$choices.label.column.to.use)


#composite_indicators
composite_indicators_definitions_weighted_counts<-load_composite_indicator_definition_weighted_count()
data<-add_variable_indicators_weighted_count(data,composite_indicators_definitions_weighted_counts)
data %>% map_to_file("./output/modified_data/data_with_composite_indicators.csv")

# load analysis definitions
# aggregating all variables (direct reporting)
# list of variables to disaggregate by:
analysis_definition_aggregations<-read.csv("./internal/input_files/aggregate all variables.csv",stringsAsFactors = F)
# create a data analysis plan with all disaggregation variables as independent variable for all variables as dependent
analysis_plan_direct_reporting <- map_to_analysis_plan_all_vars_as_dependent(analysis_definition_aggregations[["summary.statistics.disaggregated.by.variable"]],data)
analysis_plan_direct_reporting[,c("dependent.var", "independent.var")] <- analysis_plan_direct_reporting[,c("dependent.var", "independent.var")]  %>%  lapply(to_alphanumeric_lowercase) %>% as.data.frame(stringsAsFactors = F)
# APPLY ANALYSIS PLAN:
# analyse_indicator(data,dependent.var = "deviceid",independent.var= "marital_status",hypothesis.type = "direct_reporting",sampling.strategy.stratified = TRUE,case = "CASE_direct_reporting_numerical_categorical")
data<-missing_data_to_NA(data)
results<-apply_data_analysis_plan(data,analysis_plan_direct_reporting)

# RESHAPE OUTPUTS FOR MASTER TABLE:
# extract summary statistics from result list and rbind to a single long format table
all_summary_statistics <- results %>% lapply(function(x){x$summary.statistic}) %>% do.call(rbind,.) 
# save as a csv. Long format + pivot table is great for interactive xlsx
all_summary_statistics %>% as.data.frame(stringsAsFactors=F) %>%  map_to_file("./output/master_table_long.csv")

# wide format "master" table: questions and answers for columns
all_summary_statistics$master_table_column_name<-paste(all_summary_statistics$dependent.var,all_summary_statistics$dependent.var.value,sep="::: ")
all_summary_statistics[,c("independent.var","independent.var.value","master_table_column_name","numbers")] %>%
  spread(key = c("master_table_column_name"),value = "numbers") %>% map_to_file("./output/master_table_wide.csv")

# PLOTS

plots <- lapply(results, function(result){
  # printparamlist(result$input.parameters,"Exporting charts (may take a few minutes):")
  if(is.null(result$summary.statistic)|is.null(result$input.parameters$case)){print(result);return(NULL)}
  filename<-paste0("./output/barcharts/",paste(result$input.parameters %>% unlist,collapse="___"),".jpg")
  theplot<-map_to_visualisation(result$input.parameters$case )(result$summary.statistic,filename = filename)
  print(filename)
  
})

cat("\014")  
cat(green("\n\n\nDONE - no issues detected.\n"))
cat(paste0("see ", getwd(),"/","/output/ for results."))

# extract_from_result_list<-function(resultlist,what){lapply(resultlist,function(x){
#   if(what=="case"){return(x$input.parameters$case)}
# }) %>% unlist
#   }
# 




