#' Aggregating by counts
#'
#' @param df data frame to aggregate
#' @param split.by aggregating in groups. Creates one row per unique value in the data column with the name speficied here
#' @param ignore.missing.data if TRUE, NA and empty strings are not counted as a category
#' @return a list of counts for each column in df 
#' @seealso \code{\link{aggregate_count_weighted}}
#' @export
#' @examples
aggregate_count<-function(df,split.by=NULL,ignore.missing.data=T){
  # throw errors
  if(!is.null(split.by)){insure.string.is.column.header(df,split.by)}
  if(!is.null(split.by)){insure.is.single.value(split.by)}
  counts<- lapply(df,function(d){
    wtd.table(x  = d,
              y = (if(is.null(split.by)){NULL}else{df[[split.by]]}),
              na.rm = ignore.missing.data)})
  return(counts)
}


#' Aggregating by weighted counts
#'
#' @param df data frame to aggregate
#' @param split.by aggregating in groups. Creates one row per unique value in the data column with the name speficied here
#' @param ignore.missing.data if TRUE, NA and empty strings are not counted as a category
#' @return a list of weighted counts for each column in df 
#' @seealso \code{\link{function_name}}
#' @export
#' @examples
#'
aggregate_count_weighted<-function(df,weight.by=NULL,split.by=NULL,ignore.missing.data=T){
  # throw errors
  temp.weights<-auto.weight(df,weight.by)
  insure.has.data(temp.weights)
# ISSUE: weights should be done per column, excluding missing data!!!
    counts<- lapply(df,function(d){
    wtd.table(x = d,
              y = (if(is.null(split.by)){NULL}else{as.vector(df[[split.by]])}),
              weights = temp.weights,
              na.rm = ignore.missing.data)})}


#' Aggregating to percentages for a select one question
#'
#' @param df data frame to aggregate
#' @param split.by aggregating in groups. Creates one row per unique value in the data column with the name speficied here
#' @param ignore.missing.data if TRUE, NA and empty strings are not counted as a category
#' @return a list of percentages for each column in df, disaggregated by split.by 
#' @seealso \code{\link{function_name}}
#' @export
#' @examples
#'
aggregate_percent<-function(df,split.by=NULL,ignore.missing.data=TRUE){
  # df %>% lapply(function(x){is.numeric.fuzzy(as.character(x))}) %>% unlist -> numericcols
  # df[,numericcols] %>% lapply(as.numeric) -> df
  if(ignore.missing.data){
    df<-lapply(df,hasdata)
  }
  perc <- df %>% lapply(wtd.table.fraction,
                        y=(if(is.null(split.by)){NULL}else{df[[split.by]]}),
                        weights=NULL,
                        ignore.missing.data=ignore.missing.data)
  return(perc)
}


#' Aggregating to weighted percentages
#'
#' @param df data frame to aggregate
#' @param split.by aggregating in groups. Creates one row per unique value in the data column with the name speficied here
#' @param ignore.missing.data if TRUE, NA and empty strings are not counted as a category
#' @return a list of weighted percentages for each column in df, disaggregated by split.by 
#' @seealso \code{\link{function_name}}
#' @export
#' @examples
#'
#'
#'
aggregate_percent_weighted<-function(df,weight.by=NULL,split.by=NULL,ignore.missing.data=T){
  # throw errors
  if(!is.null(weight.by)){insure.string.is.column.header(df,weight.by)}
  if(!is.null(weight.by)){insure.is.single.value(weight.by)}
  if(!is.null(split.by)){insure.string.is.column.header(df,split.by)}
  if(!is.null(split.by)){insure.is.single.value(split.by)}
  temp.weights<-auto.weight(df,weight.by)
  perc<- df %>% lapply(wtd.table.fraction
                       ,y=(if(is.null(split.by)){NULL}else{df[[split.by]]})
                       ,weights=temp.weights
                       ,ignore.missing.data=ignore.missing.data)
  return(perc)
}

#' Aggregating to most frequent values
#'
#' @param
#' @param
#' @param
#' @return
#' @seealso \code{\link{function_name}}
#' @export
#' @examples
#'
aggregate_frequent<-function(df,n=3,split.by=NULL){
  # short function to get the names of the top n in a named vector:
  names.of.top.n<-function(x,n){names(x)[rev(order(x))[1:n]]}
  # make frequency tables
  freqtables<-aggregate_count(df,split.by=split.by)
  # for all tables...
  top.n.names<-freqtables %>% lapply(.,function(fqtable){
    # IF MATRIX for all columns...
    if(is.matrix.table(fqtable)){
      tops<-apply(fqtable,2,function(x){
        # get the n most frequent ones
        names.of.top.n(x,n)
      })
    }
    # if NOT MATRIX just once..
    else{tops<-names.of.top.n(fqtable,n)}
    return(tops)
  })
  return(top.n.names)
}


#' Aggregate most frequent values with stratification weights
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
aggregate_frequent_weighted<-function(df,n=3,split.by=NULL,weight.by,write.to.file=NULL){
  # short function to get the names of the top n in a named vector:
  names.of.top.n<-function(x,n){names(x)[rev(order(x))[1:n]]}
  # make frequency tables
  freqtables<-aggregate_count_weighted(df,split.by = split.by,weight.by = weight.by)
  # for all tables...
  top.n.names<-freqtables %>% lapply(.,function(fqtable){
    # IF MATRIX for all columns...
    if(is.matrix.table(fqtable)){
      tops<-apply(fqtable,2,function(x){
        # get the n most frequent ones
        names.of.top.n(x,n)
      })}
    # if NOT MATRIX just once..
    else{tops<-names.of.top.n(fqtable,n)}
    return(tops)
  })
  return(top.n.names)
}




wtd.table.fraction<-function(...,ignore.missing.data=T){
  args<-list(...)
  if(ignore.missing.data){args$na.rm<-T}
  if(!ignore.missing.data){args$na.rm<-F}
  
  wt<-do.call(wtd.table,args)
  if(is.matrix.table(wt)){
    wt<-apply(wt,2,function(x){
      x/sum(x)
    }
    )}else{
      wt<-wt/sum(wt)
    }
  return(wt)
}


is.matrix.table<-function(x){
  # determins if a table (output from table() or wtd.table() usually) has multiple rows.
  !is.na(ncol(x))
}
