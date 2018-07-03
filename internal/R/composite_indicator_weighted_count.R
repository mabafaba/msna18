add_variable_indicators_weighted_count<-function(data,composite_indicator_definitions){
  if(!("new.var.name" %in% names(composite_indicator_definitions))){stop("indicator definition must have a column called 'new.var.name'")}
  if(!("var" %in% names(composite_indicator_definitions))){stop("indicator definition must have a column called 'var'")}
  if(!("value" %in% names(composite_indicator_definitions))){stop("indicator definition must have a column called 'value'")}
  if(!("weight" %in% names(composite_indicator_definitions))){stop("indicator definition must have a column called 'weight'")}
  
  list.of.new.indicator.definitions <- composite_indicator_definitions %>%
    split.data.frame(composite_indicator_definitions$new.var.name) 
    
  # this has to be a loop, because composite indicators may depend on previous composite indicators.
  for(i in seq_along(list.of.new.indicator.definitions)){
      data[[unique(composite_indicator_definitions$new.var.name)[1]]]<-composite_indicator_weighted_count(data,indicator_definition = list.of.new.indicator.definitions[[i]])
      }

  return(data)
}


composite_indicator_weighted_count<-function(data,indicator_definition){
  if(is.null(data)|is.null(indicator_definition)){stop("input can not be null")}
  if(!("new.var.name" %in% names(indicator_definition))){stop("indicator definition must have a column called 'new.var.name'")}
  if(!("var" %in% names(indicator_definition))){stop("indicator definition must have a column called 'var'")}
  if(!("value" %in% names(indicator_definition))){stop("indicator definition must have a column called 'value'")}
  if(!("weight" %in% names(indicator_definition))){stop("indicator definition must have a column called 'weight'")}
  if(length(unique(indicator_definition$new.var.name))!=1){stop("trying to make composite indicator with more than one new name for the newly created variable")}
  
  # split indicator definition by source variable: 
  recoded <- indicator_definition %>% split.data.frame(indicator_definition$var) %>% 
    # for each source variable...
    lapply(function(x){
      # get the name of the variable to recode
      var.to.recode<-as.character(unique(x$var))
      # return the weights corresponding to each value
      if(x$condition == "EQUAL"){
      return(data[,var.to.recode] %>% recode_equal(x$value,x$weight))}
      if(x$condition == "SMALLER OR EQUAL"){
      return(data[,var.to.recode] %>% recode_less_than_equal(x$value,x$weight))}
      if(x$condition == "MORE"){
      return(data[,var.to.recode] %>% recode_more(x$value,x$weight))} 
      })
  # then take the rowsums and return them
  recoded %>% as.data.frame %>% sapply(ass.numeric) %>% rowSums %>% return
}

ass.numeric<-function(x){
  # as.numeric, but without factor mayham
  if(is.factor(x)){return(as.numeric(levels(x))[x])}else{
    return(as.numeric(x))
  }
}




