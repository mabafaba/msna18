excel_csv_inputs_sampling_frame_cluster_to_weighting_function
excel_csv_inputs_sampling_frame_stratification_to_weighting_function<-function(file="./internal/input_files/stratification_samplingframe.csv"){
  # read csv file
  rawsf<-read.csv(file,stringsAsFactors = F) %>% remove.empty.rows
  
  # parameters:
  stratum_var_separator_string<-"/---STRATA---/"
  stratum.name.variables<-rawsf[1,c("first.strata.name.variable",
                                    "second.strata.name.variable",
                                    "third.strata.name.variale")] 
  
  # clean up in case there's NA's or empty strings
  stratum.name.variables<-stratum.name.variables[!is.na(stratum.name.variables) &
                                                   stratum.name.variables != ""] %>% as.character
  
  # remove the top row from input csv, store it as the variable names, turn rest into sampling frame with concatenated strata names:
  sf_strata_names<-rawsf[-1,stratum.name.variables] %>% apply(1,paste0,collapse=stratum_var_separator_string)
  sf_strata_populations<-rawsf[-1,"population"] %>% as.numeric
  sampling.frame <- data.frame(stratum=sf_strata_names, population=sf_strata_populations)  
  
  
  
  # a function that creates combined strata names from multiple variables
  add_stratum_names_to_data<-function(data){
    # when combining stratum names from different variables, what string to use as separator?
    data_strata_names<-data[,to_alphanumeric_lowercase(all.stratification.variables)] %>% apply(1,paste0,collapse=stratum_var_separator_string)
    data_strata_names$these_are_all_the_strata_names<-data_strata_names
    return(data_strata_names)
  }
  
  # make a weighting function from the sampling frame, that assumes that the strata names are in the data
  stratification_weigthing<-weighting_fun_from_samplingframe(sf_stratification,
                                                             data.stratum.column = "these_are_all_the_strata_names", # <- matching name used by add_stratum_names_to_data()
                                                             sampling.frame.stratum.column = "stratum",
                                                             sampling.frame.population.column = "population")
  
  # make a weighting function that adds the strata names to the data, then returns the weights using the function above:
  weights_of<-function(df){
    df<-add_stratum_names_to_data(df)
    stratification_weigthing(df)
  }
  # that's the function we want:
  return(weights_of)    
}






# BLIND CODE
# must fix 'first.strata.name.variable' etc. 
# other than that should be fine
excel_csv_inputs_sampling_frame_cluster_to_weighting_function<-function(file="./internal/input_files/cluster_samplingframe.csv"){
  # read csv file
  rawsf<-read.csv(file,stringsAsFactors = F) %>% remove.empty.rows
  
  # parameters:
  stratum_var_separator_string<-"/---STRATA---/"
  stratum.name.variables<-rawsf[1,c("first.strata.name.variable",
                                    "second.strata.name.variable",
                                    "third.strata.name.variale")] 
  
  # clean up in case there's NA's or empty strings
  stratum.name.variables<-stratum.name.variables[!is.na(stratum.name.variables) &
                                                   stratum.name.variables != ""] %>% as.character
  
  # remove the top row from input csv, store it as the variable names, turn rest into sampling frame with concatenated strata names:
  sf_strata_names<-rawsf[-1,stratum.name.variables] %>% apply(1,paste0,collapse=stratum_var_separator_string)
  sf_strata_populations<-rawsf[-1,"population"] %>% as.numeric
  sampling.frame <- data.frame(stratum=sf_strata_names, population=sf_strata_populations)  
  
  
  
  # a function that creates combined strata names from multiple variables
  add_stratum_names_to_data<-function(data){
    # when combining stratum names from different variables, what string to use as separator?
    data_strata_names<-data[,to_alphanumeric_lowercase(all.stratification.variables)] %>% apply(1,paste0,collapse=stratum_var_separator_string)
    data_strata_names$these_are_all_the_strata_names<-data_strata_names
    return(data_strata_names)
  }
  
  # make a weighting function from the sampling frame, that assumes that the strata names are in the data
  stratification_weigthing<-weighting_fun_from_samplingframe(sf_stratification,
                                                             data.stratum.column = "these_are_all_the_strata_names", # <- matching name used by add_stratum_names_to_data()
                                                             sampling.frame.stratum.column = "stratum",
                                                             sampling.frame.population.column = "population")
  
  # make a weighting function that adds the strata names to the data, then returns the weights using the function above:
  weights_of<-function(df){
    df<-add_stratum_names_to_data(df)
    stratification_weigthing(df)
  }
  # that's the function we want:
  return(weights_of)    
}





