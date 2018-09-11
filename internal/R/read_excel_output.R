
excel_csv_inputs_sampling_frame_stratification_to_weighting_function<-function(file="./internal/input_files/stratification_samplingframe.csv"){
  # read csv file
  rawsf<-read.csv(file,stringsAsFactors = F, skip = 1, header = T) %>% remove.empty.rows
  stratum_var_separator_string<-"/---STRATA---/"
  stratanames.rawcols<-paste0(c("first","second","third"),".strata.name.variable")
  stratanames.rawcols<-stratanames.rawcols[!is.na(rawsf[1,stratanames.rawcols]) &
                                             rawsf[1,stratanames.rawcols] != ""] %>% as.character

  strata.variable.names<-rawsf[1,stratanames.rawcols] %>% to_alphanumeric_lowercase 
  # clean up in case there's NA's or empty strings
  
  # remove the top row from input csv, store it as the variable names, turn rest into sampling frame with concatenated strata names:
  sf_strata_names<-rawsf[-1,stratanames.rawcols] %>% as.data.frame %>% apply(1,paste0,collapse=stratum_var_separator_string)
  
  sf_strata_populations<-rawsf[-1,"population"] %>% as.numeric
  sampling.frame <- data.frame(stratum=sf_strata_names, population=sf_strata_populations)  
  
  
  # a function that creates combined strata names from multiple variables
  add_stratum_names_to_data<-function(data){
    if(any(!(strata.variable.names %in% names(data)))){
      stop(paste("sampling frame references variable(s) that can not be found in the data:",
                                                                  strata.variable.names[!(strata.variable.names %in% names(data))]))}
    # when combining stratum names from different variables, what string to use as separator?
    data_strata_names<-data[,strata.variable.names] %>% as.data.frame(stringsAsFactors=F) %>% apply(1,paste0,collapse=stratum_var_separator_string)
    data$these_are_all_the_strata_names<-data_strata_names
    return(data)
  }
  
  # make a weighting function from the sampling frame, that assumes that the strata names are in the data
  strat_weighting<-weighting_fun_from_samplingframe(sampling.frame,
                                                             data.stratum.column = "these_are_all_the_strata_names", # <- matching name used by add_stratum_names_to_data()
                                                             sampling.frame.stratum.column = "stratum",
                                                             sampling.frame.population.column = "population")
  
  # make a weighting function that adds the strata names to the data, then returns the weights using the function above:
  weights_of<-function(df){
    df<-add_stratum_names_to_data(df)
    strat_weighting(df)
  }
  # that's the function we want:
  return(list(weights_of=weights_of,sampling.frame=sampling.frame,add_stratum_names_to_data=add_stratum_names_to_data,stratum_variable="these_are_all_the_strata_names"))    
}


# BLIND CODE
# must fix 'first.strata.name.variable' etc. 
# other than that should be fine
excel_csv_inputs_sampling_frame_cluster_to_weighting_function<-function(file="./internal/input_files/cluster_samplingframe.csv"){
  # read csv file
  rawsf<-read.csv(file,stringsAsFactors = F,skip = 1,header = T) %>% remove.empty.rows
  # KNOWN ISSUE: fails if first row is  empty in all cells
  stratum_var_separator_string<-"/---CLUSTER---/"
  stratanames.rawcols<-paste0(c("first","second","third"),".cluster.name.variable")
  
  if(!all(stratanames.rawcols %in% colnames(rawsf))){stop("Issue with sampling frames. It's a weird one, but you might be able to fix it by writing 'please work!' in the cell A1 of the samplingframe sheets. Otherwise please double check that the input file sampling frame sheets are exactly identical to the latest input template.")}
  stratanames.rawcols<-stratanames.rawcols[!is.na(rawsf[1,stratanames.rawcols]) &
                                             rawsf[1,stratanames.rawcols] != ""] %>% as.character
  
  strata.variable.names<-rawsf[1,stratanames.rawcols] %>% to_alphanumeric_lowercase 
  # clean up in case there's NA's or empty strings
  # remove the top row from input csv, store it as the variable names, turn rest into sampling frame with concatenated strata names:
  sf_strata_names<-rawsf[-1,stratanames.rawcols] %>% as.data.frame %>% apply(1,paste0,collapse=stratum_var_separator_string)
  sf_strata_populations<-rawsf[-1,"population"] %>% as.numeric
  sampling.frame <- data.frame(stratum=sf_strata_names, population=sf_strata_populations)  
  
  
  # a function that creates combined strata names from multiple variables
  add_stratum_names_to_data<-function(data){
    if(any(!(strata.variable.names %in% names(data)))){stop(paste("sampling frame references variable(s) that can not be found in the data:",
                                                                  strata.variable.names[!(strata.variable.names %in% names(data))]))}
    # when combining stratum names from different variables, what string to use as separator?
    data_strata_names<-data[,strata.variable.names] %>% as.data.frame(stringsAsFactors=F) %>% apply(1,paste0,collapse=stratum_var_separator_string)
    data_strata_names[data[,strata.variable.names] %>% as.data.frame(stringsAsFactors=F) %>% apply(1,function(x){any(is.na(x))})]<-NA
    data$these_are_all_the_cluster_names<-data_strata_names
    return(data)
  }

  # make a weighting function from the sampling frame, that assumes that the strata names are in the data
  stratification_weigthing<-weighting_fun_from_samplingframe(sampling.frame,
                                                             data.stratum.column = "these_are_all_the_cluster_names", # <- matching name used by add_stratum_names_to_data()
                                                             sampling.frame.stratum.column = "stratum",
                                                             sampling.frame.population.column = "population")
  
  # make a weighting function that adds the strata names to the data, then returns the weights using the function above:
  weights_of<-function(df){
    df<-add_stratum_names_to_data(df)
    stratification_weigthing(df)
  }
  # that's the function we want:
  return(list(weights_of=weights_of,sampling.frame=sampling.frame,add_stratum_names_to_data=add_stratum_names_to_data,stratum_variable="these_are_all_the_cluster_names"))
  
}





