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
  analysisplan %>% head
undebug(map_to_design)
  results<-apply_data_analysis_plan(data,analysisplan)
  
options(error=recover)


  lapply()

