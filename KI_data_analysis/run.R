# rm(list=ls())
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd("../")
# getwd()

dir.create("./output",showWarnings = F)
dir.create("./output/modified_data",showWarnings = F)
dir.create("./output/percent_aggregations_raw_csv",showWarnings = F)
dir.create("./output/mean_aggregations_raw_csv",showWarnings = F)
dir.create("./output/barcharts",showWarnings = F)
dir.create("./output/composite_indicator_visualisation",showWarnings = F)

#load dependencies
source("../internal/R/dependencies.R")

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
questionnaire <- load_questionnaire(data,questions.file = "./internal/input_files/kobo_questions.csv",
                                    choices.file = "./internal/input_files/kobo_choices.csv",
                                    choices.label.column.to.use = data_parameters$choices.label.column.to.use)

#load KI input definitions
ki_aggregation<-read.csv("./internal/input_files/ki_aggregation.csv",stringsAsFactors = F)  %>% as.data.frame(stringsAsFactors=F)

#do KI aggregations if that was the methodology
if(ki_aggregation$key.informant.interviews == "yes"){source("./ki_aggregation.R")}