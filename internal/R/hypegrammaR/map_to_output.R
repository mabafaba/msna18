

map_resultlist_to_summarystatistic_df<-function(results){
  all_summary_statistics <- results %>% lapply(function(x){x$summary.statistic}) %>% do.call(rbind,.)
}

map_resultlist_to_datamerge<-function(results,
                                      rows=c("repeat.var","repeat.var.value"),
                                      values="numbers",
                                      ignore=c("se","min","max"),
                                      labelise.values=F,
                                      labelise.varnames=F){
  # rbind all summary statistics
  all_summary_statistics <-  results %>%
    lapply(function(x){
      x$summary.statistic %>% lapply(function(x){
        if(is.factor(x)){return(as.character(x))};x}) %>% as.data.frame(stringsAsFactors=F)
      }) %>% 
    do.call(rbind,.)

    all_summary_statistics_labeled<-results %>% lapply(function(x){x$summary.statistic}) %>% 
      lapply(labels_summary_statistic,
             label.dependent.var.value = labelise.values,
             label.independent.var.value = labelise.values,
             label.dependent.var = labelise.varnames,
             label.independent.var = labelise.varnames) %>%
      do.call(rbind,.)
    
  columns<-  names(all_summary_statistics)[!(names(all_summary_statistics) %in% c(rows,ignore,values))]
  
    if(labelise.varnames){
      all_summary_statistics_labeled$master_table_column_name<-  all_summary_statistics_labeled[,columns] %>% as.list %>% c(sep=":::") %>% do.call(paste,.)     
    }else{
      all_summary_statistics_labeled$master_table_column_name<-  all_summary_statistics[,columns] %>% as.list %>% c(sep=":::") %>% do.call(paste,.)     
      
    }
  

  # what to keep rows for:
  wide_format<-all_summary_statistics_labeled %>% unique %>% .[,c(rows,"master_table_column_name",values)] %>%
    spread(key = master_table_column_name,value = numbers)
return(wide_format)
}


map_resultslist_to_output_minibarcharts<-function(results){
  results_labeled_values<-lapply(results$results,function(x){x$summary.statistic<-x$summary.statistic %>% labels_summary_statistic;x})
  datamerge_table_long<-lapply(1:nrow(results$analysisplan),function(i){
    cat("\14")
    cat("small horizontal barcharts for FS\n")
    cat(silver((i/nrow(results$analysisplan)*100) %>% round(2) %>% paste0("%"))) 
    # lapply(r,function(i){
    if(is.null(results_labeled_values[[i]]$summary.statistic)){return(NULL)}
    if(nrow(results_labeled_values[[i]]$summary.statistic)<1){return(NULL)}
    
    output_path<-"./output/charts/"
    filename_prefix<-results_labeled_values[[i]]$input.parameters %>% unlist(use.names = F) %>% paste(collapse="___")
    
    if(results$analysisplan$output.minimal.chart...width.of.quarter.A4.landscape..FS.[i]=="yes"&!is.na(results$analysisplan$output.minimal.chart...width.of.quarter.A4.landscape..FS.[i])
       & results$analysisplan$case[i] %in% c("CASE_group_difference_categorical_categorical",
                                             "CASE_direct_reporting_categorical_",
                                             "CASE_direct_reporting_categorical_categorical")){
      
      plot_name<-"mini_barchart"
      file_type<-".jpg"
      if(length(unique(results_labeled_values[[i]]$summary.statistic$independent.var.value))>1){
        byind<-results_labeled_values[[i]]$summary.statistic %>% split.data.frame(results_labeled_values[[i]]$summary.statistic$independent.var.value)
        filenames<-paste0(output_path,filename_prefix,unique(results_labeled_values[[i]]$summary.statistic$independent.var.value),file_type)
        multplots<-lapply(seq_along(filenames),function(j){
          if(nrow(byind[[j]])==0){return(NULL)}
          plot_to_file_FS_quarter_a4width(data = byind[[j]],filename = filenames[j])
        })
        
      }else{
        filenames<-paste0(output_path,filename_prefix,"_",plot_name,file_type)
        plot_to_file_FS_quarter_a4width(data = results_labeled_values[[i]]$summary.statistic,filename = filenames)
      }
      data.frame(filename=filenames,
                 analysis_plan_row = i,
                 analysis_plan_column ="output.minimal.chart...width.of.quarter.A4.landscape..FS.",
                 repeat.var = results$results[[i]]$input.parameters$repeat.var,
                 repeat.var.value = results$results[[i]]$input.parameters$repeat.var.value,
                 datamerge_column=paste(results$results[[i]]$input.parameters$dependent.var,
                                        results$results[[i]]$input.parameters$independent.var,
                                        unique(results$results[[i]]$summary.statistic$independent.var.value),sep =  "___"
                 )
      )
      
    }
 
    
       
  }) %>% do.call(rbind,.)
  if(is.null(datamerge_table_long)){return(NULL)}
  datamerge_table<-spread(datamerge_table_long[,c("datamerge_column","filename","repeat.var.value")],key = datamerge_column,value = filename)
  analysisplan_filelist<-datamerge_table_long %>% split.data.frame(datamerge_table_long$analysis_plan_row)
  return(list(datamerge=datamerge_table,analysisplan_list=analysisplan_filelist))
}
















map_resultslist_to_output_reportbarcharts<-function(results){
  
  datamerge_table_long<-lapply(1:nrow(results$analysisplan),function(i){
    cat("\14")
    cat("regular report barcharts\n")
    cat(silver((i/nrow(results$analysisplan)*100) %>% round(2) %>% paste0("%\n"))) 
    # lapply(r,function(i){
    if(is.null(results$results[[i]]$summary.statistic)){return(NULL)}
    if(nrow(results$results[[i]]$summary.statistic)<1){return(NULL)}
    
    output_path<-"./output/charts/"
    filename_prefix<-results$results[[i]]$input.parameters %>% unlist(use.names = F) %>% paste(collapse="___")
    
    if(results$analysisplan$output.regular.chart..report.[i]=="yes"&!is.na(results$analysisplan$output.regular.chart..report.[i])
       & results$analysisplan$case[i] %in% c("CASE_group_difference_categorical_categorical",
                                             "CASE_group_difference_numerical_categorical",
                                             "CASE_direct_reporting_categorical_",
                                             "CASE_direct_reporting_categorical_categorical",
                                             "CASE_direct_reporting_numerical_categorical",
                                             "CASE_direct_reporting_numerical_")){
      
      plot_name<-"report_barchart"
      file_type<-".jpg"
        filenames<-paste0(output_path,filename_prefix,"_",plot_name,file_type)
        map_to_visualisation(results$results[[i]]$input.parameters$case)(results$results[[i]]$summary.statistic,filename = filenames)
      
      data.frame(filename=filenames,
                 analysis_plan_row = i,
                 analysis_plan_column ="output.regular.chart..report.",
                 repeat.var = results$results[[i]]$input.parameters$repeat.var,
                 repeat.var.value = results$results[[i]]$input.parameters$repeat.var.value,
                 datamerge_column=paste(results$results[[i]]$input.parameters$dependent.var,
                                        results$results[[i]]$input.parameters$independent.var,sep =  "___"
                 )
      )
      
    }
    
    
    
  }) %>% do.call(rbind,.)
  if(is.null(datamerge_table_long)){return(NULL)}
  
  datamerge_table<-spread(datamerge_table_long[,c("datamerge_column","filename","repeat.var.value")],key = datamerge_column,value = filename)
  analysisplan_filelist<-datamerge_table_long %>% split.data.frame(datamerge_table_long$analysis_plan_row)
  return(list(datamerge=datamerge_table,analysisplan_list=analysisplan_filelist))
}










map_resultslist_to_output_heatmap_table<-function(results){
  
  datamerge_table_long<-lapply(1:nrow(results$analysisplan),function(i){
    cat("\14")
    cat("heatmap tables\n")
    cat(silver((i/nrow(results$analysisplan)*100) %>% round(2) %>% paste0("%\n"))) 
    # lapply(r,function(i){
    if(is.null(results$results[[i]]$summary.statistic)){return(NULL)}
    if(nrow(results$results[[i]]$summary.statistic)<1){return(NULL)}
    
    output_path<-"./output/charts/"
    filename_prefix<-results$results[[i]]$input.parameters %>% unlist(use.names = F) %>% paste(collapse="___")
    
    if(results$analysisplan$output.heatmap[i]=="yes"&!is.na(results$analysisplan$output.heatmap[i])
       & results$analysisplan$case[i] %in% c("CASE_group_difference_categorical_categorical",
                                             "CASE_group_difference_numerical_categorical",
                                             "CASE_direct_reporting_categorical_",
                                             "CASE_direct_reporting_categorical_categorical",
                                             "CASE_direct_reporting_numerical_categorical")){
      
      plot_name<-"heatmap"
      file_type<-".jpg"
      filenames<-paste0(output_path,filename_prefix,"_",plot_name,file_type)
      map_to_visualisation_heatmap(results$results[[i]]$input.parameters$case)(results$results[[i]]$summary.statistic,filename = filenames)
      
      data.frame(filename=filenames,
                 analysis_plan_row = i,
                 analysis_plan_column ="output.regular.chart..report.",
                 repeat.var = results$results[[i]]$input.parameters$repeat.var,
                 repeat.var.value = results$results[[i]]$input.parameters$repeat.var.value,
                 datamerge_column=paste(results$results[[i]]$input.parameters$dependent.var,
                                        results$results[[i]]$input.parameters$independent.var,sep =  "___"
                 )
      )
      
    }
    
    
    
  }) %>% do.call(rbind,.)
  if(is.null(datamerge_table_long)){return(NULL)}
  
  datamerge_table<-spread(datamerge_table_long[,c("datamerge_column","filename","repeat.var.value")],key = datamerge_column,value = filename)
  analysisplan_filelist<-datamerge_table_long %>% split.data.frame(datamerge_table_long$analysis_plan_row)
  return(list(datamerge=datamerge_table,analysisplan_list=analysisplan_filelist))
}

