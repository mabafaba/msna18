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
  x_recoded[make_true] <- TRUE  # recode to "to" value where condition not met
  return(x_recoded)
}

#' Recode select_multiple to binaryw TRUE/FALSE
#'
#'@param x vector of select_multiple
#'@param selected.any.in  vector of possible respones; returns TRUE only if ANY of these were selected (and all other paramteters conditions are fulfilled)
#'@param selected.all.in  vector of possible respones; returns TRUE only if ALL of these were selected (and all other paramteters conditions are fulfilled)
#'@param selected.none.in vector of possible respones; returns TRUE only if NONE of these were selected (and all other paramteters conditions are fulfilled)
#'@details NA's stay NA. All values not specified in becomes.TRUE or becomes.FALSE become NA.
#'@return logical vector of the same length as th einput vector, all values in becomes.TRUE changed to TRUE, and in becomes.FALSE to FALSE
recode_select_multiple_to_logical <- function(x, selected.any.in, selected.all.in = NULL, selected.none.in = NULL){
  if(is.null(c(selected.any.in, selected.all.in, selected.none.in))){stop("At least one parameter must be provided")}
  x_recoded <- rep(FALSE, length(x))
  ####match any
  if(!is.null(selected.any.in)){
  match_any <- x %>% strsplit(" ") %>% lapply(function(x){
    match(selected.any.in, x)})
  make_false_any <- lapply(match_any, function(x){all(is.na(x))}) %>% unlist
  x_recoded[!make_false_any] <- TRUE}
  ####match all
  if(!is.null(selected.all.in)){
  make_false_all <- x %>% strsplit(" ") %>% sapply(function(x){
    match(selected.all.in,x) %>% is.na %>% any}) 
  x_recoded[!make_false_all] <- TRUE} 
  ####match none
  if(!is.null(selected.none.in)){
  match_none <- x %>% strsplit(" ") %>% lapply(function(x){
    match(selected.none.in, x)})
  make_true_none <- lapply(match_none, function(x){all(is.na(x))}) %>% unlist
  x_recoded[make_true_none] <- TRUE}
  return(x_recoded)
}


### input x should be the line in the data with variable name, 
recode_generic <- function(x, value, condition, to){
  recoded <- rep(NA,length(x))
  if(condition == "smaller.or.equal"){
    recoded <- recode_smaller_equal(x = x, from = value, to = to)
  }
  if(condition == "equal")
  {
    recoded <- recode_equal(x = x, from = value, to = to)
  }
  if(condition == "larger"){
    recoded <- recode_larger(x = x, from = value, to = to)
  }
  return(recoded)
}


recode_equal<-function(x,from,to){
to %<>% as.numeric
recoded <- to[match(x,from)]
return(recoded)
  }

recode_smaller_equal<-function(x,from,to){
  from %<>% as.numeric
  to %<>% as.numeric
  condition_met <- x <= from
  recoded_empty <- rep(NA,length(x))
  recoded_empty[condition_met] <- to
  return(recoded_empty)
}

recode_larger<-function(x,from,to){
  from %<>% as.numeric
  to %<>% as.numeric
  condition_met <- x > from
  recoded_empty <- rep(NA,length(x))
  recoded_empty[condition_met] <- to
  return(recoded_empty)
}


recode_contains_suficcient<-function(x,from){
  recode_select_multiple_to_logical(x,selected.any.in = from)
}

recode_contains_necessary<-function(x,from,to){
  return(to[match(x,from)])
}



ass.numeric<-function(x){
  # as.numeric, but without factor mayham
  if(is.factor(x)){return(as.numeric(levels(x))[x])}else{
    return(as.numeric(x))
  }
}









