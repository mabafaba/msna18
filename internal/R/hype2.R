cat("\14")

message(("loading dependencies.."))
# clear/create folders
unlink("./output/modified_data/",recursive=TRUE) 
unlink("./output/percent_aggregations_raw_csv",recursive=TRUE)
unlink("./output/charts",recursive=TRUE) 
dir.create("./output",showWarnings = F)
dir.create("./output/charts",showWarnings = F)
dir.create("./output/composite_indicator_visualisation",showWarnings = F)
dir.create("./output/tables",showWarnings = F)
#load dependencies
source("./internal/R/dependencies.R")
cat("\14")
source("./internal/R/survey_design2.R")
source("./internal/R/read_excel_output.R")
source("./internal/R/hypegrammaR/visualisations_barchart_FS_quarter_a4width.R")
source("./internal/R/rmarkdown_resultlist_utililities.R")
# LOAD INPUT
# make sure all files exist:

# load all the excel input files:
source("./internal/R/load_excel_input.R",local = T)

# this creates following objects:
# data  # questionnaire  # data_parameters # analysis_plan_user
# cluster_formula()  # weights_of() # question_is_skipped()
# question_is_numeric()  # question_is_categorical()  # question_is_select_one()  # question_is_select_multiple()  # question_variable_type()
# question_get_choice_labels()  # question_get_question_label()

# just give weighting a shot to see if the sampling frame is complete:
test_weights<-weights_of(data);rm(test_weights)
# COMPOSITE INDICATORS:
message(silver("making composite indicators.."))
composite_indicators_definitions_weighted_counts<-load_composite_indicator_definition_weighted_count()
if(nrow(composite_indicators_definitions_weighted_counts)>0){
  visualisation_composite_indicator_definition_graph(composite_indicators_definitions_weighted_counts)
  data<-add_variable_indicators_weighted_count(data,composite_indicators_definitions_weighted_counts)
  data %>% map_to_file("./output/modified_data/data_with_composite_indicators.csv")
  message(green("data with composite indicators exported to ./output/modified_data/data_with_composite_indicators.csv"))
}else{
  .write_to_log("\nNo Composite Indicators Defined.\n")
}

# ANALYSIS 
  analysisplan<-map_to_analysisplan_custom_user_plan(data,analysis_plan_user)

  message(silver("applying analysis plan.."))
  results<-apply_data_analysis_plan(data,analysisplan)

  results$analysisplan_log<-results$analysisplan
  results$analysisplan_log$message<-lapply(results$results,function(x){x$message}) %>% unlist
# OUTPUT
  # 
  results_labeled_values<-lapply(results$results,function(x){
    x$summary.statistics<-labels_summary_statistic(x$summary.statistics)
   return(x)
  })
  datamerge<-map_resultlist_to_datamerge(results$results,rows = c("repeat.var","repeat.var.value"),ignore = c("se","min","max"),labelise.values =F,labelise.varnames =F)
  
  results$analysisplan_log<-results$analysisplan
 
  # analysisplan$output.minimal.chart...width.of.quarter.A4.landscape..FS.<-"yes"
  # analysisplan$output.regular.chart..report.<-"yes"
  # analysisplan$output.heatmap<-"yes"
  # 
  
  # make mini barcharts
  mini_barchart_filelists<-map_resultslist_to_output_minibarcharts(results)
  # make report barcharts 
  report_barchart_filelist<-map_resultslist_to_output_reportbarcharts(results)
  
  # make heatmaps
  heatmaps_filelists<-map_resultslist_to_output_heatmap_table(results)
 # add filenames to analysis plan
 if(!is.null(mini_barchart_filelists)){
 filenames<-sapply(mini_barchart_filelists$analysisplan_list,function(x){paste(x$filename,collapse="\n")})
  analysisplan_rows<-sapply(mini_barchart_filelists$analysisplan_list,function(x){x$analysis_plan_row[1]})
 results$analysisplan_log$output.minimal.chart...width.of.quarter.A4.landscape..FS.[analysisplan_rows]<-filenames


 datamerge_row<-match(as.character(datamerge$repeat.var.value),as.character(mini_barchart_filelists$datamerge$repeat.var.value))
 datamerge<-data.frame(datamerge,minibarchart=mini_barchart_filelists$datamerge[datamerge_row,])
 
  }

 if(!is.null(report_barchart_filelist)){
 filenames<-sapply(report_barchart_filelist$analysisplan_list,function(x){paste(x$filename,collapse="\n")})
 analysisplan_rows<-sapply(report_barchart_filelist$analysisplan_list,function(x){x$analysis_plan_row[1]})
 results$analysisplan_log$output.regular.chart..report.[analysisplan_rows]<-filenames
 
 datamerge_row<-match(as.character(datamerge$repeat.var.value),as.character(report_barchart_filelist$datamerge$repeat.var.value))
 datamerge<-data.frame(datamerge,report_barchart=report_barchart_filelist$datamerge[datamerge_row,])
 }

 if(!is.null(heatmaps_filelists)){
 filenames<-sapply(heatmaps_filelists$analysisplan_list,function(x){paste(x$filename,collapse="\n")})
 analysisplan_rows<-sapply(heatmaps_filelists$analysisplan_list,function(x){x$analysis_plan_row[1]}) %>% unname
 results$analysisplan_log$heatmaps<-NA
 results$analysisplan_log$heatmaps[analysisplan_rows]<-filenames
 
 datamerge_row<-match(as.character(datamerge$repeat.var.value),as.character(heatmaps_filelists$datamerge$repeat.var.value))
 datamerge<-data.frame(datamerge,heatmap=heatmaps_filelists$datamerge[datamerge_row,])
 }


  results<-readRDS("./output/results_raw_R.RDS")  


 results$results %>% lapply(function(x){x$summary.statistic %>% labels_summary_statistic()}) %>% do.call(rbind,.) -> allsumstats
 allsumstats %>% saveRDS("./output/allsummarystatistics.RDS")
 
 # output global CSV files
 map_to_file(datamerge,"./output/datamerge.csv")
 results$analysisplan_log %>% as.data.frame %>%  map_to_file("./output/analysisplan_chart_filenames.csv")
 results$results %>% lapply(function(x){x$summary.statistic}) %>% lapply(labels_summary_statistic,T,T,T,T) %>% do.call(rbind,.) %>%  map_to_file("./output/master_table_long.csv")
 
 if(!debugging_mode){cat("\014")}  
 cat(green("\n\n\nDONE - no issues detected.\n"))
 cat(paste0("see ", getwd(),"/","/output/ for results."))
 cat(silver(paste("to process results in R, the following objects are now available:\n",
                bold("results$results: "), "a list with the results. each item has $input.parameters, $summary.statistics and $hypothesis.test subobjects\n",
                bold("results$analysisplan: "), "the analysisplan after preprocessing, as it was used for analysis\n",
                bold("results$analysisplan_log: "), "the analysisplan with plot filenames and data sanitation messages attached\n",
                bold("allsumstats: "), "a single long format (ideal for ggplot) data frame of all results\n",
                bold("a lot more objects and functions:"), "type ls() to see what's around."
                )))
 
 
rmarkdown::render("./internal/report2.rmd")
 