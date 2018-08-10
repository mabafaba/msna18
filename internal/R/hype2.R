setwd("../msna18")
# clear/create folders
unlink("./output/modified_data/",recursive=TRUE) 
unlink("./output/percent_aggregations_raw_csv",recursive=TRUE)
unlink("./output/barcharts",recursive=TRUE) 
dir.create("./output",showWarnings = F)
dir.create("./output/charts",showWarnings = F)
dir.create("./output/composite_indicator_visualisation",showWarnings = F)
dir.create("./output/tables",showWarnings = F)
#load dependencies
source("./internal/R/dependencies.R")

source("./internal/R/survey_design2.R")
source("./internal/R/read_excel_output.R")
source("./internal/R/hypegrammaR/visualisations_barchart_FS_quarter_a4width.R")
# LOAD INPUT
# make sure all files exist:

# load all the excel input files:
source("./internal/R/load_excel_input.R",local = T)
# this creates following objects:
  # data  # questionnaire  # data_parameters # analysis_plan_user
  # cluster_formula()  # weights_of() # question_is_skipped()
  # question_is_numeric()  # question_is_categorical()  # question_is_select_one()  # question_is_select_multiple()  # question_variable_type()
  # question_get_choice_labels()  # question_get_question_label()

# COMPOSITE INDICATORS:
  message(silver("making composite indicators.."))
  composite_indicators_definitions_weighted_counts<-load_composite_indicator_definition_weighted_count()
  visualisation_composite_indicator_definition_graph(composite_indicators_definitions_weighted_counts)
  data<-add_variable_indicators_weighted_count(data,composite_indicators_definitions_weighted_counts)
  data %>% map_to_file("./output/modified_data/data_with_composite_indicators.csv")
  message(green("data with composite indicators exported to ./output/modified_data/data_with_composite_indicators.csv"))

# ANALYSIS 
  analysisplan<-map_to_analysisplan_custom_user_plan(data,analysis_plan_user)

  message(silver("applying analysis plan.."))
  results<-apply_data_analysis_plan(data,analysisplan)
  results$analysisplan$message<-lapply(results$results,function(x){x$message}) %>% unlist

# OUTPUT
  results_labeled_values<-lapply(results$results,function(x){
    x$summary.statistics<-labels_summary_statistic(x$summary.statistics)
   return(x) 
  })
  
  lapply(1:nrow(results$analysisplan),function(i){
    if(is.null(results_labeled_values[[i]]$summary.statistic)){return(NULL)}
    output_path<-"./output/charts/"
    filename_prefix<-results_labeled_values[[i]]$input.parameters %>% unlist(use.names = F) %>% paste(collapse="___")
    
    if(results$analysisplan$output.minimal.chart...width.of.quarter.A4.landscape..FS.[i]=="yes"
       & results$analysisplan$case %in% c("CASE_group_difference_categorical_categorical","CASE_direct_reporting_categorical_")){

      plot_name<-"mini_barchart"
      file_type<-".jpg"
      if(length(unique(results_labeled_values[[i]]$summary.statistic$independent.var.value))>1){
        byind<-results_labeled_values[[i]]$summary.statistic %>% split.data.frame(results_labeled_values[[i]]$summary.statistic$independent.var.value)
        filenames<-paste0(output_path,filename_prefix,unique(results_labeled_values[[i]]$summary.statistic$independent.var.value),file_type)
        multplots<-lapply(seq_along(filenames),function(j){
          plot_to_file_FS_quarter_a4width(data = byind[[j]],filename = filenames[j])
        })
       
      }else{
      filenames<-paste0(output_path,filename_prefix,"_",plot_name,file_type)
      plot_to_file_FS_quarter_a4width(data = results_labeled_values[[i]]$summary.statistic,filename = filenames)
      }
    }
    cbind(filename=filenames,
          analysis_plan_row = i,
          analysis_plan_column ="output.minimal.chart...width.of.quarter.A4.landscape..FS.",
          repeat.var = results$results[[i]]$input.parameters$repeat.var,
          repeat.var.value = results$results[[i]]$input.parameters$repeat.var.value,
          datamerge_column=paste0(results$results[[i]]$input.parameters$dependent.var,
                                  results$results[[i]]$input.parameters$independent.var,
                                  unique(results$results[[i]]$summary.statistices$independent.var.value),
                                  sep="___"
                                  )
    )
          
  })
  
  
  map_analysisplan_to_visualisation<-function(results,type="FS_style_barchart_A4"){
    if(type=="FS_style_barchart_A4"){return()}
  }
  
  # plots <- lapply(seq_along(results), function(resultindex){
  #   result<-results[[resultindex]]
  #   # printparamlist(result$input.parameters,"Exporting charts (may take a few minutes):")
  #   if(is.null(result$summary.statistic)|is.null(result$input.parameters$case)){return(NULL)}
  #   result$summary.statistic.labeled<-map_to_labelisation("summary.statistic")(result$summary.statistic)  
  # 
  #     filename<-paste0("./output/charts/",paste(result$input.parameters %>% unlist,collapse="___"),".jpg")
  #   
  #   if(result$input.parameters$case %in% c("CASE_direct_reporting_categorical_","CASE_group_difference_categorical_categorical")){
  #     result$summary.statistic.labeled %>% split.data.frame(result$summary.statistic$independent.var.value) %>% lapply(function(sumstat){
  #       theplot<-plot_to_file_FS_quarter_a4width(sumstat,filename = filename)
  #       
  #     })
  #     
  #   }else{
  #     theplot<-map_to_visualisation(result$input.parameters$case )(result[["summary.statistic.labeled"]],filename = filename)
  #     result$plotfilename<-paste0(paste(result$input.parameters %>% unlist,collapse="___"),".jpg")
  #   }
  #   
  #   print(filename)
  #   return(result)
  #   
  # })
  
  map_resultlist_to_datamerge(results,rows = NULL) %>% map_to_file("./output/master_table_wide.csv")
  

