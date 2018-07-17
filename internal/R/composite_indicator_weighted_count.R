add_variable_indicators_weighted_count<-function(data,composite_indicator_definitions){
  if(!("new.var.name" %in% names(composite_indicator_definitions))){stop("indicator definition must have a column called 'new.var.name'")}
  if(!("var" %in% names(composite_indicator_definitions))){stop("indicator definition must have a column called 'var'")}
  if(!("value" %in% names(composite_indicator_definitions))){stop("indicator definition must have a column called 'value'")}
  if(!("weight" %in% names(composite_indicator_definitions))){stop("indicator definition must have a column called 'weight'")}
  
  list.of.new.indicator.definitions <- composite_indicator_definitions %>%
    # split definitions by new indicators to create. specify levels to insure order stays consistent
    split.data.frame(factor(composite_indicator_definitions$new.var.name,levels=unique(composite_indicator_definitions$new.var.name)))
  composite_indicator_definitions$new.var.name <- to_alphanumeric_lowercase(composite_indicator_definitions$new.var.name)
  
  # this has to be a loop, because composite indicators may depend on previous composite indicators.
  for(i in seq_along(list.of.new.indicator.definitions)){
    data[[unique(list.of.new.indicator.definitions[[i]]$new.var.name)[1]]] <- composite_indicator_weighted_count(data,indicator_definition = list.of.new.indicator.definitions[[i]])
  }
  
  return(data)
}


composite_indicator_weighted_count<-function(data,indicator_definition){
  if(is.null(data)|is.null(indicator_definition)){stop("input can not be null")}
  if(!("new.var.name" %in% names(indicator_definition))){stop("indicator definition must have a column called 'new.var.name'")}
  if(!("var" %in% names(indicator_definition))){stop("indicator definition must have a column called 'var'")}
  if(!("value" %in% names(indicator_definition))){stop("indicator definition must have a column called 'value'")}
  if(!("weight" %in% names(indicator_definition))){stop("indicator definition must have a column called 'weight'")}
 # if(length(unique(indicator_definition$new.var.name))!=1){stop("trying to make composite indicator with more than one new name for the newly created variable")}
  
  # split indicator definition by source variable: 
  indicator_definition_by_variable <- indicator_definition %>% split.data.frame(indicator_definition$var) 
  
  all_recoded_vars<-lapply(indicator_definition_by_variable,

                           function(this_var_recoding_definition){
                             var.to.recode <- as.character(unique(this_var_recoding_definition$var))
                             # return the weights corresponding to each value
                             x_recoded <- rep(NA,length(data[,var.to.recode]))
                             for(i in 1:nrow(this_var_recoding_definition)){
                               x <- this_var_recoding_definition[i,,drop = F]
                               recoded_generic <- recode_generic(data, data[,x$var], x$value, x$condition, x$weight, x$var)
                               # all recodings overwrite the previous ones; except for "else" type of condition - so we need to decise what values replace:
                               if(x$condition == "else"){
                                 # subset(x_recoded, !is.na(x_recoded) & (!is.na(data[,x$var]) | question_is_skipped(data, data[,x$var]))) <- recode_else(data = data, x = x, to = to)
                                 to_replace<- (is.na(x_recoded) & ((!is.na(data[,x$var]) | question_is_skipped(data, data[,x$var]))))
                               }else{
                                 to_replace<-!is.na(recoded_generic) 
                                 # (apart from "else" and skipped, no condition can overwrite NAs in original data)
                               }
                               x_recoded[to_replace] <- recoded_generic[to_replace]
                             }
                             
                             # default else to 0:
                             x_recoded[(is.na(x_recoded) & ((!is.na(data[,x$var]) | question_is_skipped(data, data[,x$var]))))]<-0
                             
                             return(x_recoded)
                             
                           }
  )
  all_recoded_vars %>% as.data.frame %>% sapply(ass.numeric) %>% rowSums %>% return
}

ass.numeric<-function(x){
  # as.numeric, but without factor mayham
  if(is.factor(x)){return(as.numeric(levels(x))[x])}else{
    return(as.numeric(x))
  }
}




