percent_with_confints <- function(dependent.var,
                                  design,
                                  data,
                                  na.rm = TRUE){
  if(question_is_select_multiple(dependent.var)){
    return(percent_with_confints_select_mult(dependent.var = dependent.var, design = design))
  }
  return(percent_with_confints_select_one(dependent.var = dependent.var,  design = design))
}


percent_with_confints_select_one <- function(dependent.var,
                                             design,
                                             na.rm = TRUE){
  
  
  # if dependent var have only one value, just return that:
  
  dependent_more_than_1 <- length(unique(data[[dependent.var]])) > 1
  if(!dependent_more_than_1){
    dependent.var.value=unique(data[[dependent.var]])
    return(data.frame(dependent.var,independent.var=NA,dependent.var.value,independent.var.value=NA,numbers=1,se=NA,min=NA,max=NA))}

  tryCatch(expr={result_hg_format<- 
  {
    result_svy_format <- svymean(formula(paste0("~", dependent.var)),design, level=0.95) %>% cbind(.,confint(.))
    colnames(result_svy_format)<-c("numbers","min","max")
    summary_with_confints <- data.frame(dependent.var=dependent.var,
                   independent.var=NA,
                   dependent.var.value=gsub(paste0("^",dependent.var),"",rownames(result_svy_format)),
                   independent.var.value=NA,
                   numbers=result_svy_format[,"numbers"],
                   se=NA,
                   min=result_svy_format[,"min"],
                   max=result_svy_format[,"max"])
    summary_with_confints[,"min"] <- summary_with_confints[,"min"] %>% replace(summary_with_confints[,"min"] < 0 , 0)
    summary_with_confints[,"max"] <- summary_with_confints[,"max"] %>% replace(summary_with_confints[,"max"] > 1 , 1)
    summary_with_confints %>% as.data.frame
  }
  return(result_hg_format)}, error=function(e){
    .write_to_log("percent_with_confints_select_one failed with error:")
    .write_to_log(e$message)}
  )
  
}

percent_with_confints_select_mult <- function(dependent.var,
                                             design,
                                             na.rm = TRUE){
  
  
  # if dependent and independent variables have only one value, just return that:
  choices <- data[,choices_for_select_multiple(dependent.var, data)]
  
  dependent_more_than_1 <- length(unique(data[[dependent.var]])) > 1
  if(!dependent_more_than_1){
    dependent.var.value=unique(data[[dependent.var]])
    return(data.frame(dependent.var,independent.var=NA,dependent.var.value,independent.var.value=NA,numbers=1,se=NA,min=NA,max=NA))}
  
  result_hg_format <- lapply(names(choices), function(x){
    
    result_svy_format <- svymean(formula(paste0("~", x)),design, level=0.95) %>% cbind(.,confint(.))
    
    colnames(result_svy_format)<-c("numbers","min","max")
    summary_with_confints <- data.frame(dependent.var=dependent.var,
                                        independent.var=NA,
                                        dependent.var.value=gsub(paste0("^",dependent.var,"."),"",x),
                                        independent.var.value=NA,
                                        numbers=result_svy_format[,"numbers"],
                                        se=NA,
                                        min=result_svy_format[,"min"],
                                        max=result_svy_format[,"max"])})
  
  result_hg_format %<>% do.call(rbind,.)
    
  result_hg_format[,"min"] <- result_hg_format[,"min"] %>% replace(result_hg_format[,"min"] < 0 , 0)
  result_hg_format[,"max"] <- result_hg_format[,"max"] %>% replace(result_hg_format[,"max"] > 1 , 1)
  result_hg_format %>% as.data.frame

  return(result_hg_format)}



percent_with_confints_groups <- function(dependent.var,
                                  independent.var,
                                  design,
                                  data,
                                  na.rm = TRUE){
  if(question_is_select_multiple(dependent.var)){
    return(percent_with_confints_select_mult_groups(dependent.var = dependent.var, independent.var = independent.var, design = design, data = data))
  }
  return(percent_with_confints_select_one_groups(dependent.var = dependent.var, independent.var = independent.var, design = design))
}


percent_with_confints_select_one_groups <- function(dependent.var,
                                             independent.var,
                                             design,
                                             na.rm = TRUE){
  
  
  # if dependent and independent variables have only one value, just return that:
  
  if(length(unique(as.character(data[[dependent.var]])))==1){
    dependent.var.value=unique(data[[dependent.var]])
    if(length(unique(as.character(data[[independent.var]])))==1){
      independent.var.value=unique(data[[independent.var]])	
      return(data.frame(dependent.var,independent.var,dependent.var.value,independent.var.value,numbers=1,se=NA,min=NA,max=NA))
      
    }
  }
  
  formula_string <- paste0("~",dependent.var ,sep = "")
  by <- paste0("~", independent.var ,sep = "")
  
  result_hg_format<- # tryCatch(
  {
    result_svy_format <- svyby(formula(formula_string), formula(by), design, svymean, na.rm = T, keep.var = T,vartype = "ci")
    unique.dependent.var.values<- design$variables[[dependent.var]] %>% unique
    summary_with_confints<-unique.dependent.var.values %>%
      lapply(function(x){
        summary_stat_colname<-paste0(dependent.var,x)
        lower_confint_colname<-paste0("ci_l.",summary_stat_colname)
        upper_confint_colname<-paste0("ci_u.",summary_stat_colname)
        
        dependent_value_x_stats<-result_svy_format[,c(independent.var,summary_stat_colname,lower_confint_colname,upper_confint_colname)]
        colnames(dependent_value_x_stats)<-c("independent.var.value","numbers","min","max")
        data.frame(dependent.var=dependent.var,
                   independent.var=independent.var,
                   dependent.var.value=x,
                   independent.var.value=dependent_value_x_stats[,"independent.var.value"],
                   numbers=dependent_value_x_stats[,"numbers"],
                   se=NA,
                   min=dependent_value_x_stats[,"min"],
                   max=dependent_value_x_stats[,"max"])
      }) %>% do.call(rbind,.)
    
    
    summary_with_confints[,"min"] <- summary_with_confints[,"min"] %>% replace(summary_with_confints[,"min"] < 0 , 0)
    summary_with_confints[,"max"] <- summary_with_confints[,"max"] %>% replace(summary_with_confints[,"max"] > 1 , 1)
    summary_with_confints %>% as.data.frame
  }
  
  return(result_hg_format)
}




#should only be called if the question is select multiple 
percent_with_confints_select_mult_groups <- function(dependent.var,
                                              independent.var,
                                              design,
                                              data,
                                              na.rm = TRUE){
  
  # if dependent and independent variables have only one value, just return that:
  choices <- data[,choices_for_select_multiple(dependent.var, data)]
  
  result_hg_format <- lapply(names(choices), function(x){
    if(length(unique(data[[x]]))==1){
      dependent.var.value= x
      if(length(unique(data[[independent.var]]))==1){
        independent.var.value=unique(data[[independent.var]])	
        return(data.frame(dependent.var,independent.var,dependent.var.value,independent.var.value,numbers=as.numeric(unique(data[[x]])),se=NA,min=NA,max=NA))}}
    formula_string_sans_tilde<-paste0("as.numeric(",x ,")",sep = "")
    formula_string <- paste0("~as.numeric(",x ,")",sep = "")
    by <- paste0("~", independent.var ,sep = "") 
    
    result_svy_format <- svyby(formula(formula_string), formula(by), design, svymean, na.rm = T, keep.var = T,vartype = "ci")
           
    summary_stat_colname <- formula_string_sans_tilde
    lower_confint_colname<-paste0("ci_l")
    upper_confint_colname<-paste0("ci_u")
    
    dependent_value_x_stats <- result_svy_format[,c(independent.var, summary_stat_colname,lower_confint_colname,upper_confint_colname)]
    colnames(dependent_value_x_stats)<-c("independent.var.value","numbers","min","max")
    data.frame(dependent.var=dependent.var,
               independent.var=independent.var,
               dependent.var.value=gsub(paste0("^",dependent.var,"."),"",x),
               independent.var.value=dependent_value_x_stats[,"independent.var.value"],
               numbers=dependent_value_x_stats[,"numbers"],
               se=NA,
                   min=dependent_value_x_stats[,"min"],
                   max=dependent_value_x_stats[,"max"])})
  
  result_hg_format %<>% do.call(rbind,.)
    
  result_hg_format[,"min"] <- result_hg_format[,"min"] %>% replace(result_hg_format[,"min"] < 0 , 0)
  result_hg_format[,"max"] <- result_hg_format[,"max"] %>% replace(result_hg_format[,"max"] > 1 , 1)
  result_hg_format %>% as.data.frame

  
  return(result_hg_format)
}
 # check if we actually got  a frequency table back; problems can arise here if independent.var has only 1 unique value 
  # if(!(nrow(as.data.frame(p.table)>1))){stop("DEV: unexpected edge case in percent_with_confints - freq table has 1 or less rows. contact development team about this error.")}
  # 
  #   p.table %>% melt -> ftable_flipped
  # 
  #   colnames(ftable_flipped)<-c("dependent.var.value","independent.var.value","numbers")
  #   results<-data.frame( dependent.var = dependent.var,
  #                        independent.var = independent.var,
  #                        ftable_flipped,
  #                        se=NA,
  #                        min=confints[,1],
  #                        max=confints[,2])

  # results<-list(
  #   independent.var.value=ftable
  # )
  # results<-list()
  # results$independent.var.value <- stat[,independent.var]
  # results$dependent.var.value <- stat[,"variable"]
  # results$numbers <-stat[,"value"]
  # results$se <- standard_error[,"value"]
  # results$min <- results$numbers - results$se
  # results$max <- results$numbers + results$se
  # results<-f.table
#   return(results)
# }


confidence_intervals_mean <- function(dependent.var,
                                     independent.var = NULL,
                                     design,
                                     data = data){
    if(!is.null(independent.var)){warning("confidence intervals calculated without disaggregation, but received data for an independent variable.")}
    formula_string<-paste0("~as.numeric(", dependent.var, ")")
    summary <- svymean(formula(formula_string), design, na.rm = T)
    confints <- confint(summary, level = 0.95)
    results <- data.frame(dependent.var = dependent.var,
               independent.var = "NA",
               dependent.var.value = "NA",
               independent.var.value = "NA",
               numbers=summary[1],
               se=summary[2],
               min=confints[1],
               max=confints[2])
    return(results)
 }

  confidence_intervals_mean_groups <- function(dependent.var,
                                     independent.var,
                                     design,
                                     data){

  formula_string <- paste0("~as.numeric(", dependent.var,")")
  by <- paste0("~", independent.var, sep = "")
  
  result_svy_format <- svyby(formula(formula_string), formula(by), design, svymean, na.rm = T, keep.var = T,vartype = "ci")
  unique.independent.var.values <- design$variables[[independent.var]] %>% unique
  results<-unique.independent.var.values %>%
    lapply(function(x){
      dependent_value_x_stats <- result_svy_format[as.character(x),]
      colnames(dependent_value_x_stats)<-c("independent.var.value","numbers","min","max")
      data.frame(dependent.var=dependent.var,
                 independent.var=independent.var,
                 dependent.var.value= NA,
                 independent.var.value=x,
                 numbers=dependent_value_x_stats[2],
                 se=NA,
                 min=dependent_value_x_stats[3],
                 max=dependent_value_x_stats[4])
    }) %>% do.call(rbind,.)

  return(results)
}

  ### for select_one and select multiple answers, returns the most common answer for that group 
  # only works for select_one and select_multiple
  
  
  summary_statistic_mode <- function(dependent.var,independent.var, design,data){
    percent<-percent_with_confints(dependent.var,independent.var, design,data)
    modes <- percent %>% split.data.frame(percent$independent.var.value, drop = T) %>% lapply(function(x){
      x[which.max(x$numbers),]}) %>% do.call(rbind, .)
    return(modes)}
  
  summary_statistic_rank<- function(dependent.var,independent.var, design,data){
    percent<-percent_with_confints(dependent.var,independent.var, design,data)
    ranked <- percent %>% split.data.frame(percent$independent.var.value, drop = T) %>% lapply(function(x){
      mutate(x, rank = rank(x$numbers, ties.method = "min"))}) %>% do.call(rbind, .) 
    return(ranked)
  }
  
