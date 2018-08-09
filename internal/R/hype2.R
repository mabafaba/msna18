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

# LOAD INPUT
# make sure all files exist:
verify_excel_input()

# load all the excel input files:
source("./internal/R/load_excel_input.R",local = T)
# this creates following objects:
  # data  # questionnaire  # data_parameters # analysis_plan_user
  # cluster_formula()  # weights_of() # question_is_skipped()
  # question_is_numeric()  # question_is_categorical()  # question_is_select_one()  # question_is_select_multiple()  # question_variable_type()
  # question_get_choice_labels()  # question_get_question_label()

# COMPOSITE INDICATORS:
  composite_indicators_definitions_weighted_counts<-load_composite_indicator_definition_weighted_count()
  visualisation_composite_indicator_definition_graph(composite_indicators_definitions_weighted_counts)
  data<-add_variable_indicators_weighted_count(data,composite_indicators_definitions_weighted_counts)
  data %>% map_to_file("./output/modified_data/data_with_composite_indicators.csv")
  
# ANALYSIS 
  analysisplan<-map_to_analysisplan_custom_user_plan(data,analysis_plan_user)

  results<-apply_data_analysis_plan(data,analysisplan)
  


  lapply(seq_along(results),function(resultindex)){
      if(results$analyisplanè )
  }

  results %>% lapply(function(x){x$summary.statistic })  %>% do.call(rbind,.)

  
# OUTPUT
  all_summary_statistics<-map_resultlist_to_summarystatistic_df(results)
  
  
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
  
  map_resultlist_to_datamerge(results,rows = NULL) %>% map_to_file("./output/master_table_wide.csv")
  
  
  
  results[[10]]$visualisation(results[[10]]$summary.statistic,"test.jpg")

