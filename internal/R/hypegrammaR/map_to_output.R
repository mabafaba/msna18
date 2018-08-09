

map_resultlist_to_summarystatistic_df<-function(results){
  all_summary_statistics <- results %>% lapply(function(x){x$summary.statistic}) %>% do.call(rbind,.)
}


map_resultlist_to_datamerge<-function(results,rows=c("repeat.var","repeat.var.value"),values="numbers",ignore=c("se","min","max")){
  all_summary_statistics <-  results %>% lapply(function(x){x$summary.statistic}) %>% do.call(rbind,.)
  columns<-  names(all_summary_statistics)[!(names(all_summary_statistics) %in% c(rows,ignore,values))]
  all_summary_statistics$master_table_column_name<-all_summary_statistics[,columns] %>% as.list %>% c(sep=":::") %>% do.call(paste,.) 
  all_summary_statistics$master_table_column_name %>% table
  # what to keep rows for:
  wide_format<-all_summary_statistics[,c(rows,"master_table_column_name",values)] %>%
    spread(key = master_table_column_name,value = numbers) %>% map_to_file("./output/master_table_wide.csv")
return(wide_format)
  
  
    
}


