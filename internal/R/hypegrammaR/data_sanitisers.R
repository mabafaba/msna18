datasanitation_design<-function(design,dependent.var,independent.var,sanitation_function){
  sanitised<-sanitation_function(design$variables,dependent.var,independent.var)
  if(sanitised$success){
    sanitised$design<-map_to_design(sanitised$data)
  }else{
    sanitised$design<-NULL
  }
return(sanitised)
}


# BLOCK SPECIFIC SANITATIONS:
datasanitation_hypothesistest_chisq<-function(data,dependent.var,independent.var){
  # apply an exquisite selection of sanitations functions relevant to chisquare hypothesis tests:
  
  
  apply_data_sanitations(data,           # all functions take these parameters
                         dependent.var,  # all functions take these parameters
                         independent.var,# all functions take these parameters
                         datasanitation_morethan_1_unique_dependent,
                         datasanitation_morethan_1_unique_independent,
                         datasanitation_independent_max_unique,
                         datasanitation_morethan_1_record_per_independent_value
                         
  )
}

datasanitation_hypothesistest_t<-function(data,dependent.var,independent.var){
  # apply an exquisite selection of sanitations functions relevant to chisquare hypothesis tests:
  
  
  apply_data_sanitations(data,           # all functions take these parameters
                         dependent.var,  # all functions take these parameters
                         independent.var,# all functions take these parameters
                         datasanitation_morethan_1_unique_dependent,
                         datasanitation_morethan_1_unique_independent,
                         datasanitation_dependent_numeric,
                         datasanitation_independent_max_unique,
                         datasanitation_morethan_1_record_per_independent_value
                         
  )
}


# GENERIC SANITATION GROUPS:
datasanitation_always_applicable_before<-function(data,dependent.var,independent.var,...){
  apply_data_sanitations(data,dependent.var,independent.var,
                         datasanitation_is_good_dataframe,
                         datasanitation_variables_in_data_colnames,
                         datasanitation_remove_missing,
                         BEFORE=NULL, # apply_data_sanitations() applies this function; overwritting it to prevent INFINITE RECURSION (scary innit)
                         AFTER=NULL   # apply_data_sanitations() applies this function; overwritting it to prevent INFINITE RECURSION 
                         )
}

datasanitation_always_applicable_after<-function(data,dependent.var,independent.var,...){
  apply_data_sanitations(data,dependent.var,independent.var,
                         datasanitation_is_good_dataframe,
                         datasanitation_morethan_2_records_total,
                         BEFORE=NULL, # apply_data_sanitations() applies this function; overwritting it to prevent INFINITE RECURSION 
                         AFTER=NULL   # apply_data_sanitations() applies this function; overwritting it to prevent INFINITE RECURSION 
  )
}


# STANDARD FORMATS
# centralising the output format (insuring it's standardised and preventing repetition):
failed_sanitation<-function(message){
  return(list(data=NULL,message=message,success=F))
}

successfull_sanitation<-function(data){
  return(list(data=data,message=NA,success=T))
}

datasanitation_generic_check<-function(data,dependent.var,independent.var,valid,message=""){
  if(valid){return(successfull_sanitation(data))}else{return(failed_sanitation(message))}
}

# CHAINING SANITATION FUNCTIONS:
apply_data_sanitations<-function(data,dependent.var,independent.var,...){
  # ... should be sanitation functions.
  # call like this:
  # apply_data_sanitations(data,"myvarname","myothervarname",
  #                        datasanitation_remove_missing_data,
  #                        datasanitation_dependent_morethan_1_unique,
  #                        datasanitation_independent_morethan_1_unique)
  # get the "..." parameters as a list; add generic tests (can be overwritten by passing datasanitation_always_applicable_before/after as parameters!) 
      params<-list(...)
  # allow overwriting 'before' and 'after' generic tests by passing 'BEFORE' and 'AFTER' named arguments:
    if("BEFORE" %in% names(list(...))){before<-params$BEFORE}else{before<-datasanitation_always_applicable_before}
    if("AFTER" %in% names(list(...))){after<-params$AFTER}else{after<-datasanitation_always_applicable_after}
  
      sanitation_functions<-list(before,...,after)


  
  # this has to be sequential, so here's a loop
  
  data_sanitised<-successfull_sanitation(data) # starting 
  # for each sanitation function..
  
  for(i in c(1:length(sanitation_functions))){
    # take the i'th function
    
    currentfun<-sanitation_functions[[i]]
    if(is.null(currentfun)){next}
    # apply the function
    data_sanitised<-currentfun(data,dependent.var,independent.var)
    # if sanitation failed, quit sanitation (return), and return an empty sanitation with the message:
    if(data_sanitised$success==F){return(data_sanitised)}
    # otherwise, go ahead with the next sanitation
  }
  return(data_sanitised)
}






# GENERIC LOW LEVEL SANITATIONS:



datasanitation_is_good_dataframe<-function(data,...){
  if(!is.data.frame(data)){return(failed_sanitation("data is not a data frame"))}
  if(ncol(data)<1){return(failed_sanitation("data has no columns"))}
  if(nrow(data)<1){return(failed_sanitation("data has no rows"))}
  if(as.vector(data) %>% is.na %>% all){return(failed_sanitation("all data is NA"))}
  return(successfull_sanitation(data))
}


datasanitation_morethan_1_unique_dependent<-function(data,dependent.var,independent.var){
  dependent_more_than_1 <- length(unique(data[[dependent.var]])) > 1
  if(!dependent_more_than_1){return(failed_sanitation("less than two unique values in the dependent variable"))}
  return(successfull_sanitation(data))
  }

datasanitation_morethan_1_unique_independent<-function(data,dependent.var,independent.var){
  dependent_more_than_1 <- length(unique(data[[independent.var]])) > 1
  if(!dependent_more_than_1){return(failed_sanitation("less than two unique values in the independent variable"))}
  return(successfull_sanitation(data))
}

datasanitation_remove_missing<-function(data,dependent.var,independent.var,...){
  data<-data[!is.na(data[[dependent.var]]),]
  data<-data[(data[[dependent.var]]!=""),]
  if(nrow(data)<=2){return(failed_sanitation("less than 3 records have valid values in the dependent variable and in the independent variable"))}
  return(successfull_sanitation(data))
  }

datasanitation_variables_in_data_colnames<-function(data,dependent.var,independent.var,...){
  dep_var_name_in_data_headers<- grep(paste0("^",dependent.var),colnames(data),value = T)
  indep_var_name_in_data_headers<- grep(paste0("^",independent.var),colnames(data),value = T)
  if(length(dep_var_name_in_data_headers)==0){return(failed_sanitation(paste0("dependent variable \"",dependent.var,"\" not found in data.")))}
  if(length(indep_var_name_in_data_headers)==0 & !is.null(independent.var)){
    return(failed_sanitation(paste0("independent variable \"",independent.var,"\" not found in data.")))}
  successfull_sanitation(data)    
}

datasanitation_independent_max_unique<-function(data,dependent.var,independent.var){
  n_max<-30
  valid<-length(unique(data[[independent.var]])) <= n_max
  datasanitation_generic_check(data,dependent.var,independent.var,valid,paste0("too many (>=",n_max,") unique values in independent variable"))
}

datasanitation_morethan_1_record_per_independent_value<-  function(data,dependent.var,independent.var){
  which_independent_more_than_one_record <- table(data[[independent.var]])
  which_independent_more_than_one_record <- which_independent_more_than_one_record[which(which_independent_more_than_one_record>1)]
  which_independent_more_than_one_record <- names(which_independent_more_than_one_record)
  data <- data[data[[independent.var]] %in% which_independent_more_than_one_record,]
  successfull_sanitation(data)
}

datasanitation_morethan_2_records_total<-function(data,dependent.var,independent.var,...){
  datasanitation_generic_check(data,dependent.var,independent.var,valid=nrow(data)>2,"less than 2 records two samples with valid data available for this combination of dependent and independent variable")
}




datasanitation_dependent_numeric<-function(data,dependent.var,independent.var,...){
  if(is.factor(data[[dependent.var]])){data[[dependent.var]]<-as.character(data[[dependent.var]])}
  data[[dependent.var]]<-suppressWarnings(as.numeric(data[[dependent.var]]))
  if(all(is.na(data[[dependent.var]]))){return(failed_sanitation("dependent variable is not numeric"))}
  data<-data[!is.na(data[[dependent.var]]),]
  return(successfull_sanitation(data))
}


datasanitation_independent_numeric<-function(data,dependent.var,independent.var,...){
  if(is.factor(data[[dependent.var]])){data[[dependent.var]]<-as.character(data[[dependent.var]])}
  data[[dependent.var]]<-as.numeric(data[[dependent.var]])
  if(all(is.na(data[[dependent.var]]))){return(failed_sanitation("independent variable is not numeric"))}
  data<-data[!is.na(data[[dependent.var]]),]
  return(successfull_sanitation(data))
}
























