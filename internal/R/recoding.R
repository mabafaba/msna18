#' Recode select_one to binary TRUE/FALSE
#'
#'@param x vector of select_one
#'@param becomes.TRUE values to recode to TRUE
#'@param becomes.FALSE values to change to FALSE
#'@details NA's stay NA. All values not specified in becomes.TRUE or becomes.FALSE become NA.
#'@return logical vector of the same length as the input vector, all values in becomes.TRUE changed to TRUE, and in becomes.FALSE to FALSE
recode_select_one_to_logical <- function(x, becomes.TRUE, becomes.FALSE){
  make_true <- x %in% becomes.TRUE # check which value matches the critical value
  make_false<- x %in% becomes.FALSE
  x_recoded<-rep(NA,length(x))
  x_recoded[make_false] <- FALSE  # recode to "to" value where condition met
  x_recoded[make_true] <- TRUE  # recode to "to" value where condition met
  return(x_recoded)
}



data<-read.csv("./internal/input_files/data.csv")
recode_definition<-read.csv("./internal/input_files/recoding.csv")




fill_blanks_down<-function(df,fill.columns){
  
}



ass.numeric<-function(x){
  # as.numeric, but without factor mayham
  if(is.factor(x)){return(as.numeric(levels(x))[x])}else{
    return(as.numeric(x))
  }
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

  



indicator_definition<-data.frame(new.var.name="call.success",
                                 var=    c(rep("call.status",3),"idp.ref","idp.ref"),
                                 value=  c("off","wrong_number","answered","refugee","returnee")
                                 ,weight=c(0,1,2,10,20),stringsAsFactors = F)





add_variable_indicators_weighted_count<-function(data,composite_indicator_definitions){
  if(!("new.var.name" %in% names(indicator_definition))){stop("indicator definition must have a column called 'new.var.name'")}
  if(!("var" %in% names(indicator_definition))){stop("indicator definition must have a column called 'var'")}
  if(!("value" %in% names(indicator_definition))){stop("indicator definition must have a column called 'value'")}
  if(!("weight" %in% names(indicator_definition))){stop("indicator definition must have a column called 'weight'")}

  composite_indicators <- composite_indicator_definitions %>%
    split.data.frame(composite_indicator_definitions$new.var.name) %>%
    sapply(composite_indicator_weighted_count,data=data)
  
  names(composite_indicators)<-unique(composite_indicator_definitions$new.var.name)
  return(data.frame(data,composite_indicators,stringsAsFactors = F))
}






