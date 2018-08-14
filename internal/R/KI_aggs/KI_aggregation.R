#### add a mapping function from the parameters (what you are aggregating to) to the median / mode / means
source("./internal/R/triangulate_KI.R")


###FIRST YOU TRIANGULATE 
if(!is.null(ki_aggregation$level.to.triangualte.to)){
  trig_data <- triangulate(data, ki_aggregation$level.to.triangualte.to)
  }




#' Aggregating by median
#'
#' @param
#' @param
#' @param
#' @return
#' @seealso \code{\link{function_name}}
#' @export
#' @examples
#'
#'
#'
aggregate_median<-function(df,aggregate_by){
  if(length(aggregate_by)!=1){stop("aggregate_by should be a single value matching exactly one of the data frame columns")}
  if(!(aggregate_by%in%names(df))){stop("aggregate_by must match one of the column names of the data frame")}
  numeric<-df %>% lapply(function(x){is.numeric.fuzzy(as.character(x))}) %>% unlist
    if(any(!numeric)){
    message(paste("aggregated by mode instead of median:",paste(names(df)[!numeric],collapse="\n"),collapse='\n'))
  }
  # aggregate
  df<-aggregate.data.frame(df,by = list(df[[aggregate_by]]),FUN = medianORmode)
  # df<-dplyr::select(df,-contains(aggregate_by))
  df<-df[, !names(df) %in% aggregate_by]
  names(df)[names(df)=="Group.1"] <- aggregate_by
  if(!is.null(write.to.file)){write.csv.untidy(df,write.to.file)}
  return(df)
}

#' Aggregating by mean
#'
#' @param
#' @param
#' @param
#' @return
#' @seealso \code{\link{function_name}}
#' @export
#' @examples
#'
#'
#'
aggregate_mean<-function(df, aggregate_by = NULL, when.tie=NA){
  # throw errors
  #  if(!is.data.frame(df)){stop("input for 'df' must be a data frame")}
  if(length(aggregate_by)!=1){stop("aggregate_by should be a single value matching exactly one of the data frame columns")}
  if(!(aggregate_by%in%names(df))){stop("aggregate_by must match one of the column names of the data frame")}
  numeric<-df %>% lapply(function(x){is.numeric.fuzzy(as.character(x))}) %>% unlist
  if(any(!numeric)){
    message(paste("could not calculate mean for non numeric data",paste(names(df)[!numeric],collapse="\n"),collapse='\n'))
    
  }
  # aggregate
  df<-aggregate.data.frame(df,by = list(df[[aggregate_by]]),FUN = mean_R)
  # df<-dplyr::select(df,-contains(aggregate_by))
  df<-df[, !names(df) %in% aggregate_by]
  return(df)
}



#' Aggregating by weighted mean
#'
#' @param
#' @param
#' @param
#' @return
#' @seealso \code{\link{function_name}}
#' @export
#' @examples
#'
#'
#'
aggregate_mean_weighted<-function(df, weight.by = NULL,aggregate_by = NULL, when.tie=NA){
  # throw errors
  #  if(!is.data.frame(df)){stop("input for 'df' must be a data frame")}
  if(length(aggregate_by)!=1){stop("aggregate_by should be a single value matching exactly one of the data frame columns")}
  if(!(aggregate_by%in%names(df))){stop("aggregate_by must match one of the column names of the data frame")}
  numeric<-df %>% lapply(function(x){is.numeric.fuzzy(as.character(x))}) %>% unlist
  if(any(!numeric)){
    message(paste("could not calculate mean for non numeric data",paste(names(df)[!numeric],collapse="\n"),collapse='\n'))
    
  }
  # aggregate
  temp.weights<-auto.weight(df,weight.by)
  df<-aggregate.data.frame(df,by = list(df[[aggregate_by]]),FUN = mean_R_weighted, weights = temp.weights)
  # df<-dplyr::select(df,-contains(aggregate_by))
  df<-df[, !names(df) %in% aggregate_by]
  return(df)
}




#' #' Aggregating to mode
#' #'
#' #' @param df the data frame to aggregate
#' #' @param aggregate.by the name of the column to aggregate by
#' #' @param when.tie what value to return when there is a tie (defaults to NA)
#' #' @param ignore.missing when TRUE, first removes NA, empty strings, -Inf, Inf  (default TRUE)
#' #' @return data frame of Modes for all variables in df, with one row per unique value in the column named in aggregate.by
#' #' @seealso \code{\link{function_name}}
#' #' @export
#' #' @examples
#' #'
#' aggregate_mode<-function(df,aggregate.by,when.tie=NA,ignore.missing=T){
#'   
#'   df<-df %>% factors2strings
#'   # if(ignore.missing){
#'   #   df <- lapply(df,hasdata)
#'   # }
#'   
#'   df  %>%
#'     split.data.frame(df[[aggregate.by]]) %>%
#'     lapply(function(x){
#'       
#'       
#'       lapply(x,function(x){
#'         if(ignore.missing){x<-hasdata(x)}
#'         Mode(x,when.tie=NA)
#'       }) %>%
#'         unlist
#'     }) %>%
#'     do.call(rbind,.)
#'   
#' }
