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
analysis_plan_direct_reporting<-map_to_analysis_plan_all_vars_as_dependent(analysis_definition_aggregations,data)

# APPLY ANALYSIS PLAN:
results<-apply_data_analysis_plan(data,analysis_plan_direct_reporting)

# RESHAPE OUTPUTS:
all_summary_statistics<-results %>% lapply(function(x){x$summary.statistic}) %>% do.call(rbind,.) 
all_summary_statistics$master_table_column_name<-paste(all_summary_statistics$dependent.var,all_summary_statistics$dependent.var.value,sep="::: ")
all_summary_statistics$master_table_column_name %>% table


all_summary_statistics %>% write.csv("./output/master_table_long.csv")

all_summary_statistics %>% spread(key = c("master_table_column_name"),value = "numbers") %>% write.csv("MASTERWOAH.csv")
data$adequate_water %>% table

ggplot(sumstatsdf[sumstatsdf$dependent.var=="hoh_sex",],aes(x=dependent.var.value,y=numbers,fill=independent.var.value))+geom_bar(stat = "identity")+reachtheme()



