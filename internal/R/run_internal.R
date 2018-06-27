
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd("../../")
getwd()
source("./internal/R/dependencies.R")
source("./internal/R/recoding.R")
source("./internal/R/load_analysis_definitions.R")
source("./internal/R/composite_indicator_weighted_count.R")
source("./internal/R/survey_design.R")
source("./internal/R/recoding.R")
source("./internal/R/aggregation.R")
source("./internal/R/KI_aggregation.R")

source("./internal/R/errors.R")
source("./internal/R/summary_statistics.R")
source("./internal/R/load_questionnaire.R")

# load data
data<-read.csv("./internal/input_files/data.csv")
# load data metadata
data_parameters<-read.csv("./internal/input_files/data_parameters.csv",stringsAsFactors = F)


# if(readline("previous script outputs will be deleted / overwritten. type 'yes' to confirm:")!="yes"){stop("user cancelled script execution")}

message("deleting all previous script outputs with absolutely no warning lol")
unlink("./output/modified_data/",recursive=TRUE) 
unlink("./output/percent_aggregations_raw_csv",recursive=TRUE) 
dir.create("./output",showWarnings = F)
dir.create("./output/modified_data",showWarnings = F)
dir.create("./output/percent_aggregations_raw_csv",showWarnings = F)
dir.create("./output/mean_aggregations_raw_csv",showWarnings = F)


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

all_percent_disaggregations_all_vars<-
  lapply(analysis_definition_aggregations$summary.statistics.disaggregated.by.variable,
         function(disaggregation.var){

            if(data_parameters$stratified=="yes"){
              this_disag_percentages<-aggregate_percent_weighted(data,split.by = disaggregation.var)
            }else{
              this_disag_percentages<-aggregate_percent(data,split.by = disaggregation.var)
            }
            path<-paste0("./output/percent_aggregations_raw_csv/",disaggregation.var,"/")
            dir.create(path)
            lapply(names(this_disag_percentages),function(x){
              write.csv(this_disag_percentages[[x]],paste0(path,x,".csv"))
            this_disag_percentages
          })

})


all_means_disaggregations_all_vars<-
  lapply(analysis_definition_aggregations$summary.statistics.disaggregated.by.variable,
         function(disaggregation.var){
           if(data_parameters$stratified=="yes"){
             this_disag_percentages<-aggregate_mean_weighted(data,aggregate_by = disaggregation.var)
           }else{
             this_disag_percentages<-aggregate_mean(data,aggregate_by = disaggregation.var)
           }
           path<-paste0("./output/mean_aggregations_raw_csv/",disaggregation.var,"/")
           dir.create(path)
           lapply(names(this_disag_percentages),function(x){
             write.csv(this_disag_percentages[[x]],paste0(path,x,".csv"))
             this_disag_percentages
           })
           
         })





write.csv(data_with_composite_indicators,"./output/modified_data/data_w_weighted_counts.csv")
message(paste0("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nsee '",getwd(),"/output' for your results!"))

