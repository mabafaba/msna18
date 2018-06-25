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





add_variables_recode_select_one_to_logical<-function(data,recode_definition){
  recode_definition %>% split.data.frame(recode_definition$new.variable.name) %>% lapply(function(def){
    new.var.name<-unique(def$new.variable.name)[1]
    

  })
}



