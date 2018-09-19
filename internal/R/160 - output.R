
results_labeled_values<-lapply(results$results,function(x){
  x$summary.statistics<-labels_summary_statistic(x$summary.statistics)
  return(x)
})
datamerge<-map_resultlist_to_datamerge(results$results,rows = c("repeat.var","repeat.var.value"),ignore = c("se","min","max"),labelise.values =F,labelise.varnames =F)
results$analysisplan_log<-results$analysisplan

# analysisplan$output.minimal.chart...width.of.quarter.A4.landscape..FS.<-"yes"
# analysisplan$output.regular.chart..report.<-"yes"
# analysisplan$output.heatmap<-"yes"

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

results %>% saveRDS("./output/ResultsForfactsheets.RDS")


results$results %>% lapply(function(x){x$summary.statistic %>% labels_summary_statistic()}) %>% do.call(rbind,.) -> allsumstats
allsumstats %>% saveRDS("./output/allsummarystatistics.RDS")

# output global CSV files
map_to_file(datamerge,"./output/datamerge.csv")
results$analysisplan_log %>% as.data.frame %>%  map_to_file("./output/analysisplan_chart_filenames.csv")
results$results %>% lapply(function(x){x$summary.statistic}) %>% lapply(labels_summary_statistic,T,T,T,T) %>% do.call(rbind,.) %>%  map_to_file("./output/master_table_long.csv")

logmessage(silver("creating html report output"))

suppressMessages(rmarkdown::render("./internal/report2.rmd",output_file = "../output/results.html"))

cat("\014")  
cat(green("\n\n\nDONE with statistical tests and plots - no issues detected.\n"))
cat(paste0("see ", getwd(),"/","/output/ for results."))
cat(silver(paste("to process results in R, the following objects are now available:\n",
                 bold("results$results: "), "a list with the results. each item has $input.parameters, $summary.statistics and $hypothesis.test subobjects\n",
                 bold("results$analysisplan: "), "the analysisplan after preprocessing, as it was used for analysis\n",
                 bold("results$analysisplan_log: "), "the analysisplan with plot filenames and data sanitation messages attached\n",
                 bold("allsumstats: "), "a single long format (ideal for ggplot) data frame of all results\n",
                 bold("a lot more objects and functions:"), "type ls() to see what's around."
)))

cat(silver("\n\n Now compling outputs into html - this may take a while..\n"))
cat(green("\n\n\nScripts finished. Opening complied results in browser.\n"))
browseURL("./output/results.html")