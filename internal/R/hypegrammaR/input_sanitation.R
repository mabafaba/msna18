sanitise_group_difference<-function(data,dependent.var,independent.var){
  independent_less_than_30 <- length(unique(data[[independent.var]])) <= 30

  if(!independent_less_than_30){
  return(list(success=FALSE,message="can not test group difference with 20 or more unique values in the independent variable"))
  }

  dependent_more_than_1 <- length(unique(data[[dependent.var]])) > 1
  if(!dependent_more_than_1){
    return(list(success=FALSE,message="can not test group difference with <2 different values in the dependent variable"))
  }


  which_independent_more_than_one_record <- table(data[[independent.var]])
  which_independent_more_than_one_record <- which_independent_more_than_one_record[which(which_independent_more_than_one_record>1)]
  which_independent_more_than_one_record <- names(which_independent_more_than_one_record)
  data <- data[data[[independent.var]] %in% which_independent_more_than_one_record,]


  at_least_two_independent_groups <- (data[[independent.var]] %>% unique %>% length) > 1


  if(!at_least_two_independent_groups){
    return(list(success=FALSE,message="can not test group difference with <2 unique values in the independent variable with at least 2 records)"))
  }

  return(list(success=TRUE,data=data))
}


sanitise_data <- function(data, dependent.var,independent.var,case){
  
  data<-tryCatch({
    numerics<-sapply(names(data),question_is_numeric)
    data[,numerics]<-data[,numerics] %>% lapply(as.numeric) %>% as.data.frame(stringsAsFactors=F)
    data
    },
    error=function(e){return(data)}
  )
  
  
  if(is.null(independent.var)){
    sanitise_data_no_independent(data = data, dependent.var = dependent.var, case = case)}else{
      sanitise_data_independent(data = data, independent.var = independent.var, dependent.var = dependent.var, case = case)
    }
}

sanitise_data_no_independent<-function(data,
                                 dependent.var,
                                 case){

  dep_var_name_in_data_headers<- grep(paste0("^",dependent.var),colnames(data),value = T)
  indep_var_name_in_data_headers<- grep(paste0("^",dependent.var),colnames(data),value = T)
  if(length(dep_var_name_in_data_headers)==0){
    stop(paste0("dependent.var: \"",dependent.var,"\" not found in data"))
  }
  

  
  if(!sanitise_is_good_dataframe(data)){return(list(success=F,message="not a data frame or data frame without data"))}
  
  # remove records with NA in dependent or independent
  data<-data[!is.na(data[[dependent.var]]),]
  data<-data[(data[[dependent.var]]!=""),]
  

  # still have enough data?
  
  dependent_more_than_1 <- length(unique(data[[dependent.var]])) > 1
  if(!dependent_more_than_1){
    return(list(success=FALSE,message="can not summarise statistics with <2 different values in the dependent variable"))
  }
  
  
  if(!sanitise_is_good_dataframe(data)){return(list(success=F,message="no data (after removing records with NA in dependent variable)"))}
  return(list(success=T,data=data))
}

sanitise_data_independent<-function(data,
                        dependent.var,
                        independent.var,
                        case){


  dep_var_name_in_data_headers<- grep(paste0("^",dependent.var),colnames(data),value = T)
  indep_var_name_in_data_headers<- grep(paste0("^",dependent.var),colnames(data),value = T)
  if(length(dep_var_name_in_data_headers)==0){
    stop(paste0("dependent.var: \"",dependent.var,"\" not found in data"))
    
      }
  if(length(indep_var_name_in_data_headers)==0){
    stop(paste0("independent.var: \"",independent.var,"\" not found in data"))
    
  }

  if(!sanitise_is_good_dataframe(data)){return(list(success=F,message="not a data frame or data frame without data"))}

  # remove records with NA in dependent or independent
  data<-data[!is.na(data[[dependent.var]]) & !is.na(data[[independent.var]]),]
  data<-data[(data[[dependent.var]]!="") & (data[[independent.var]]!=""),]


  # still have data?
  if(!sanitise_is_good_dataframe(data)){return(list(success=F,message="no data (after removing records with NA in dependent or independent variable)"))}


  if((grep("group_difference",case) %>% length)>0){
    group_difference<-sanitise_group_difference(data,
                                                dependent.var = dependent.var,
                                                independent.var = independent.var)
    if(group_difference$success==F){return(group_difference)}

  }
  if((grep("direct_reporting",case) %>% length)>0){
    group_difference<-sanitise_group_difference(data,
                                                dependent.var = dependent.var,
                                                independent.var = independent.var)
    if(group_difference$success==F){return(group_difference)}
    
  }

  if(case%in%c("CASE_group_difference_categorical_categorical","CASE_direct_reporting_categorical_categorical","CASE_direct_reporting_categorical_")){
    if(question_is_select_one(dependent.var) & length(unique(data[[dependent.var]]))>50){
      return(list(success=F,message="can not perform chisquared test (and won't calculate summary statistics) on >50 unique values in the dependent variable."))

    }
  }
if(!(nrow(data)>=2)){return(list(success=F,message="less than two samples with valid data available for this combination of dependent and independent variable"))}
return(list(success=T,data=data))

  
}




sanitise_is_good_dataframe<-function(data){

  if(!is.data.frame(data)){return(FALSE)}
  if(ncol(data)<1){return(FALSE)}
  if(nrow(data)<1){return(FALSE)}
  if(as.vector(data) %>% is.na %>% all){return(FALSE)}
  return(TRUE)

  }








data_sanitation_remove_not_in_samplingframe<-function(data,samplingframe_object,name="samplingframe"){
  records_not_found_in_sf<-!(samplingframe_object$add_stratum_names_to_data(data)[,samplingframe_object$stratum_variable] %in% samplingframe_object$sampling.frame$stratum)
  if(length(which(records_not_found_in_sf))==0){
    .write_to_log(paste("samplingframe",name,"complete.\n"))
    return(data)
  }
        message(paste("FATAL:", length(which(records_not_found_in_sf)),"records discarded, because they could not be matched with samplingframe",name,"\n"))
  .write_to_log(paste("FATAL:", length(which(records_not_found_in_sf)),"records discarded, because they could not be matched with samplingframe",name,"\n"))
  
  .write_to_log(paste("names not found in sampling frame:\n",
                      paste(samplingframe_object$add_stratum_names_to_data(data)[,samplingframe_object$stratum_variable] %>% unique,collapse="\n")
  ))
  
  return(data[!records_not_found_in_sf,])
  
}












