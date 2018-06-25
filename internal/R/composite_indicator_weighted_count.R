add_variable_indicators_weighted_count<-function(data,composite_indicator_definitions){
  if(!("new.var.name" %in% names(composite_indicator_definitions))){stop("indicator definition must have a column called 'new.var.name'")}
  if(!("var" %in% names(composite_indicator_definitions))){stop("indicator definition must have a column called 'var'")}
  if(!("value" %in% names(composite_indicator_definitions))){stop("indicator definition must have a column called 'value'")}
  if(!("weight" %in% names(composite_indicator_definitions))){stop("indicator definition must have a column called 'weight'")}
  
  composite_indicators <- composite_indicator_definitions %>%
    split.data.frame(composite_indicator_definitions$new.var.name) %>%
    sapply(composite_indicator_weighted_count,data=data)
  
  names(composite_indicators)<-unique(composite_indicator_definitions$new.var.name)
  return(data.frame(data,composite_indicators,stringsAsFactors = F))
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
      return(data[,var.to.recode] %>% recode(x$value,x$weight))
    })
  # then take the rowsums and return them
  recoded %>% as.data.frame %>% sapply(ass.numeric) %>% rowSums %>% return
}

recode<-function(x,from,to){
  return(to[match(x,from)])
}






ass.numeric<-function(x){
  # as.numeric, but without factor mayham
  if(is.factor(x)){return(as.numeric(levels(x))[x])}else{
    return(as.numeric(x))
  }
}




