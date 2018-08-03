# rm(list=ls())
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
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
dir.create("./output/composite_indicator_visualisation",showWarnings = F)

#load dependencies
  source("./internal/R/dependencies.R")

# LOAD INPUT
# make sure all files exist:
files_needed<-c("./internal/input_files/data.csv",
                "./internal/input_files/data_parameters.csv",
                "./internal/input_files/sampling_frame.csv",
                "./internal/input_files/kobo_questions.csv",
                "./internal/input_files/kobo_choices.csv",
                "./internal/input_files/cluster_sample.csv")

filesexist<-sapply(files_needed,file.exists)
.log<-list()
.logfile<-"./output/log.txt"

.write_to_log<-function(x){
  .log<-c(.log,x)
  sink(.logfile,append = T)
  cat("\n\n\n---------------------\n---------------------\n")
  cat(x)
  sink()
}

.clearlog<-function(){
  sink(.logfile,append = F)
  cat("")
  sink()
}

.clearlog()

if(any(!filesexist)){
missing_sheets<-files_needed[!filesexist] %>%  strsplit("/") %>% lapply(function(x){x[length(x)] %>% gsub(".csv","",.)}) %>% unlist %>% paste(collapse = "\n")
stop(paste0(
"Input information seems to be missing.
Please open the input xlsx sheets for data and analysis definition and click the update button in the readme sheet of each file.
Make sure the input xlsm files  contain all sheets from the template with the names unchanged! Not exported sheets:\n",
missing_sheets))
}

# data 
data<-read.csv("./internal/input_files/data.csv",stringsAsFactors = F) %>% to_alphanumeric_lowercase_colnames_df
missing_data_to_NA<-function(data){
  lapply(data,function(x){
    replace(x,which(x %in% c("","N/A","#N/A","NA", " ")),NA)    
  }) %>% as.data.frame(stringsAsFactors=F)# survey needs with factors.
}

#function that recodes categorical variables using the levels provided in the choices file
#also converts missing data to NA without messing up the factors 
levels_for_cat <- function(data, questionnaire){
  #questionnaire must be loaded
 data_level <-  lapply(names(data), function(x){
   replace(data[[x]],which(data[[x]] %in% c("","N/A","#N/A","NA", " ")),NA)
    if(question_is_categorical(x)){
      data[[x]] %<>% factor(., levels = questionnaire$choices_per_variable[x] %>% as.data.frame %>% extract2(2) %>% unique)}
  return(data[[x]])}) 
 names(data_level) <- names(data)
 return(data_level %>% as.data.frame)
}

## Loading cluster sampling units
cluster_formula <- load_cluster_sampling_units()

# data parameters
data_parameters<-read.csv("./internal/input_files/data_parameters.csv",stringsAsFactors = F) 
data_parameters$stratum.name.variable <- data_parameters$stratum.name.variable %>% to_alphanumeric_lowercase

# load samplingframe (only if data_parameters says it's a stratified sample)
if(data_parameters$stratified[1]=="yes"){
  
  if(is.na(data_parameters$stratum.name.variable)){stop("if the input sheets \"define stratified\":\"yes\"", "then you also must fill a value for \"stratum name variable\" (see input sheet xlsm)")}

    sf<-load_samplingframe("./internal/input_files/sampling_frame.csv",
                                                             data.stratum.column = data_parameters$stratum.name.variable[1],
                                                             return.stratum.populations = T)}
# load questionnaire and create associated functions:
questionnaire<-load_questionnaire(data,questions.file = "./internal/input_files/kobo_questions.csv",
                                    choices.file = "./internal/input_files/kobo_choices.csv",
                                  choices.label.column.to.use = data_parameters$choices.label.column.to.use)
# load cluster ids and create associated functions:

# cleaning and getting the factors out 
data <- levels_for_cat(data, questionnaire)
#composite_indicators
composite_indicators_definitions_weighted_counts<-load_composite_indicator_definition_weighted_count()
visualisation_composite_indicator_definition_graph(composite_indicators_definitions_weighted_counts)
data<-add_variable_indicators_weighted_count(data,composite_indicators_definitions_weighted_counts)
data %>% map_to_file("./output/modified_data/data_with_composite_indicators.csv")
# load analysis definitions
# aggregating all variables (direct reporting)
# list of variables to disaggregate by:
analysis_definition_aggregations<-read.csv("./internal/input_files/aggregate all variables.csv",stringsAsFactors = F) %>% remove.empty.rows 
# create a data analysis plan with all disaggregation variables as independent variable for all variables as dependent




# random sample of analysis plan rows for testing:
# analysis_plan_direct_reporting<-analysis_plan_direct_reporting[sample(1:nrow(analysis_plan_direct_reporting),200),]

analysis_plan_direct_reporting <- map_to_analysis_plan_all_vars_as_dependent(repeat.var = analysis_definition_aggregations[["do.for.each.variable"]][1], 
                                                                             independent.vars = analysis_definition_aggregations[["summary.statistics.disaggregated.by.variable"]], 
                                                                             data = data)
analysis_plan_all_vars_no_disag <- map_to_analysis_plan_all_vars_no_disag(repeat.var = analysis_definition_aggregations[["do.for.each.variable"]][1], 
                                                                          data = data)

analysisplan<-rbind(analysis_plan_direct_reporting,analysis_plan_all_vars_no_disag)

# APPLY ANALYSIS PLAN:
results<-apply_data_analysis_plan(data,analysisplan)

#RESHAPE OUTPUTS FOR MASTER TABLE:
# extract summary statistics from result list and rbind to a single long format table

all_summary_statistics <- results %>% lapply(function(x){x$summary.statistic}) %>% do.call(rbind,.)


dir.create("./internal/log")
all_summary_statistics %>% saveRDS("./internal/log/results.RDS")
all_summary_statistics_labeled <- results %>% lapply(function(x){x$summary.statistic %>% labels_summary_statistic}) %>% do.call(rbind,.)
stay_distinguishable<-c("dependent.var","dependent.var.value","independent.var","independent.var.value")
all_summary_statistics_labelandnames<-all_summary_statistics_labeled
all_summary_statistics_labelandnames[,stay_distinguishable]<-lapply(stay_distinguishable,function(column){
  jointlabel<-all_summary_statistics_labeled[[column]] %>% as.character
  jointlabel[!is.na(all_summary_statistics[[column]])]<-
    paste0(as.character(all_summary_statistics_labeled[[column]][!is.na(all_summary_statistics[[column]])])," (",
           as.character(all_summary_statistics[[column]][!is.na(all_summary_statistics[[column]])]),")")
  jointlabel
})



# save as a csv. Long format + pivot table is great for interactive xlsx

all_summary_statistics_labelandnames %>% as.data.frame(stringsAsFactors=F) %>%  map_to_file("./output/master_table_long.csv")

# wide format "master" table: questions and answers for columns
all_summary_statistics_labelandnames$master_table_column_name<-paste(all_summary_statistics_labelandnames$dependent.var,all_summary_statistics_labelandnames$dependent.var.value,sep="::: ")

all_summary_statistics_labelandnames[,c("independent.var","independent.var.value","master_table_column_name","numbers")] %>%
  spread(key = c("master_table_column_name"),value = "numbers") %>% map_to_file("./output/master_table_wide.csv")

all_summary_statistics_labelandnames$master_table_column_name<-paste(all_summary_statistics_labelandnames$independent.var,":: ",
                                                                     all_summary_statistics_labelandnames$independent.var.value," - ",
                                                                     all_summary_statistics_labelandnames$dependent.var,":: ",
                                                                     all_summary_statistics_labelandnames$dependent.var.value,
                                                                     sep="")

arow_per_repeat_value<-all_summary_statistics_labelandnames[,c("repeat.var.value","master_table_column_name","numbers")] %>%
  spread(key = c("master_table_column_name"),value = "numbers")
arow_per_repeat_value %>% map_to_file("./output/master_table_datamerge.csv")

# PLOTS
plots <- lapply(seq_along(results), function(resultindex){
  result<-results[[resultindex]]

  # printparamlist(result$input.parameters,"Exporting charts (may take a few minutes):")
  if(is.null(result$summary.statistic)|is.null(result$input.parameters$case)){return(NULL)}
  
  result$summary.statistic.labeled<-map_to_labelisation("summary.statistic")(result$summary.statistic)  
  filename<-paste0("./output/barcharts/",paste(result$input.parameters %>% unlist,collapse="___"),".jpg")
  
  if(result$input.parameters$case %in% c("CASE_direct_reporting_categorical_","CASE_group_difference_categorical_categorical")){
    result$summary.statistic.labeled %>% split.data.frame(result$summary.statistic$independent.var.value) %>% lapply(function(sumstat){
      filename<-paste0("./output/barcharts/",paste(result$input.parameters %>% unlist,collapse="___"),"__",sumstat$independent.var[1],"_",sumstat$independent.var.value[1],".jpg")
      theplot<-map_to_visualisation(result$input.parameters$case )(sumstat,filename = filename)
      
    })

    }else{
      theplot<-map_to_visualisation(result$input.parameters$case )(result[["summary.statistic.labeled"]],filename = filename)
      result$plotfilename<-paste0(paste(result$input.parameters %>% unlist,collapse="___"),".jpg")
    }
  
  
  print(filename)
  return(result)
  
})

htmlreport(plots)
if(!debugging_mode){cat("\014")}  
cat(green("\n\n\nDONE - no issues detected.\n"))
cat(paste0("see ", getwd(),"/","/output/ for results."))



# extract_from_result_list<-function(resultlist,what){lapply(resultlist,function(x){
#   if(what=="case"){return(x$input.parameters$case)}
# }) %>% unlist
#   }
# 
