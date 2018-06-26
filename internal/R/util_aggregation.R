#' Aggregating by counts
#'
#' @param df data frame to aggregate
#' @param split.by aggregating in groups. Creates one row per unique value in the data column with the name speficied here
#' @param ignore.missing.data if TRUE, NA and empty strings are not counted as a category
#' @param write.to.file specify a CSV file name to write results to
#' @return
#' @seealso \code{\link{function_name}}
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
#' @param
#' @param
#' @param
#' @return
#' @seealso \code{\link{function_name}}
#' @export
#' @examples
#'
aggregate_count_weighted<-function(df,weight.by=NULL,split.by=NULL,ignore.missing.data=T,write.to.file=NULL){
  # throw errors
  temp.weights<-auto.weight(df,weight.by)
  insure.has.data(temp.weights)
# ISSUE: weights should be done per column, excluding missing data!!!
    counts<- lapply(df,function(d){
    wtd.table(x = d,
              y = (if(is.null(split.by)){NULL}else{as.vector(df[[split.by]])}),
              weights = temp.weights,
              na.rm = ignore.missing.data)})

  if(!is.null(write.to.file)){write.csv.untidy(counts,write.to.file)}
  return(counts)
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
aggregate_median<-function(df,aggregate_by,write.to.file=NULL){

  # throw errors
  if(!is.data.frame(df)){stop("input for 'df' must be a data frame")}
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
aggregate_mean<-function(df, aggregate_by = NULL, when.tie=NA, write.to.file=NULL){

  # throw errors
  if(!is.data.frame(df)){stop("input for 'df' must be a data frame")}
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
  names(df)[names(df)=="Group.1"] <- aggregate_by
  if(!is.null(write.to.file)){write.csv.untidy(df,write.to.file)}
  return(df)
}




#' Aggregating to mode
#'
#' @param df the data frame to aggregate
#' @param aggregate.by the name of the column to aggregate by
#' @param when.tie what value to return when there is a tie (defaults to NA)
#' @param ignore.missing when TRUE, first removes NA, empty strings, -Inf, Inf  (default TRUE)
#' @return data frame of Modes for all variables in df, with one row per unique value in the column named in aggregate.by
#' @seealso \code{\link{function_name}}
#' @export
#' @examples
#'
aggregate_mode<-function(df,aggregate.by,when.tie=NA,ignore.missing=T,write.to.file){

  df<-df %>% factors2strings
  # if(ignore.missing){
  #   df <- lapply(df,hasdata)
  # }

  df  %>%
    split.data.frame(df[[aggregate.by]]) %>%
    lapply(function(x){


      lapply(x,function(x){
        if(ignore.missing){x<-hasdata(x)}
        Mode(x,when.tie=NA)
      }) %>%
        unlist
    }) %>%



    do.call(rbind,.)

}


#' Aggregating to percentages
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
aggregate_percent<-function(df,split.by=NULL,ignore.missing.data=TRUE,write.to.file=NULL){

  # df %>% lapply(function(x){is.numeric.fuzzy(as.character(x))}) %>% unlist -> numericcols
  # df[,numericcols] %>% lapply(as.numeric) -> df
  if(ignore.missing.data){
    df<-lapply(df,hasdata)
  }
  perc <- df %>% lapply(wtd.table.fraction,
                        y=(if(is.null(split.by)){NULL}else{df[[split.by]]}),
                        weights=NULL,
                        ignore.missing.data=ignore.missing.data)

  if(!is.null(write.to.file)){write.csv.untidy(perc,write.to.file)}
  return(perc)
}


#' Aggregating to weighted percentages
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
aggregate_percent_weighted<-function(df,weight.by=NULL,split.by=NULL,ignore.missing.data=T,write.to.file=NULL){
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

  if(!is.null(write.to.file)){write.csv.untidy(perc,write.to.file)}
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
aggregate_frequent<-function(df,n=3,split.by=NULL,write.to.file=NULL){
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


auto.weight<-function(df,weight.by=NULL){
  # A) if no weight.by provided, I'll try calculating myself:
  if(is.null(weight.by)){temp.weights<-weights_of(df)}
  # B) character string provided
  else if(is.character(weight.by)){
    warning("using the weight.by argument is not recommended. You should call load_samplingframe(), and then weighted functions without weight.by argument.")
    insure.string.is.column.header(df,weight.by)
    insure(length(weight.by)==1,err="weight by should be NULL for auto weighting, or a single character string naming the data column that stores the weights")
    df[[weight.by]] <- gsub(",", ".", df[[weight.by]]) %>% as.numeric
    temp.weights<-df[[weight.by]]
  }else{
    insure.has.data(temp.weights)
  }
  return(temp.weights)
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


#' percent_increase
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
percent_increase<-function(from,to, match.column){

# check input
  insure(all(names(from)==names(to)),"from and to must have exactly the same columns")
  insure.is.single.value(match.column)
  insure.no.duplicates(from[[match.column]])
  insure.no.duplicates(to[[match.column]])
# remove "from" rows that are not in "to" (according to 'match.column')
  from<-from[which(from[[match.column]]%in%to[[match.column]]),]
# remove "to rows that are not in "from" (according to 'match.column')
  to<-to[which(to[[match.column]]%in%from[[match.column]]),]

  percentincrease<-lapply(names(from),function(col){
  if(is.numeric(from[[col]]) & is.numeric(to[[col]])){
  return(from[[col]] %percent_increase.vec% to[[col]] )
  }else{
    return(from[[col]])
  }
  }) %>% as.data.frame
  names(percentincrease)<-names(from)
  return(percentincrease)
  }





'%percent_increase.vec%'<-function(old,new){
  if(is.numeric(old) & is.numeric(new)){
  insure.same.length(old,new)
  inc<-new-old
  return(inc/old)
  }else{
    stop("'old' and 'new' must be numeric")
  }
}




# some fake data:

# testdata<-data.frame( info.governorate = sample(c("gov1","gov2","gov3"),1000,T),
#                       info.settlement  = sample(letters[1:20],1000,T),
#                       someintegers = sample(c(1:10000),1000),
#                       somefactors = as.factor(sample(letters[1:10],1000,T)),
#                       somenumbers = runif(1000)*10,
#                       fake.weights=runif(1000)*2)
#
# # top 5 most frequent answers for all variables weighted:
# aggregate_count_weighted( df = testdata,
#                  weight.by = "fake.weights",
#                  split.by = "info.governorate")
#
#
# # top 5 most frequent answers for all variables weighted:
# aggregate_count_weighted( df = testdata,
#                           weight.by = "fake.weights",
#                           split.by = "info.governorate")




aggregate_quantiles<-function(df,proportions=c(0,0.25,0.5,0.75,1),split.by=NULL){
  if(is.null(split.by)){
    lapply(df,function(x){
      if(is.numeric(x)){
        quants<-quantile(x,probs = proportions,na.rm=T)
      }else{quants<-rep(Mode(x),length(proportions))
      return(quants)
      }
    })

  }else{

    df %>% split.data.frame(df[[split.by]]) %>% lapply(function(x){
      lapply(x,function(x){
        if(is.numeric(x)){
          quants<-quantile(x,probs = proportions,na.rm=T)
        }else{quants<-rep(Mode(x),length(proportions))
        quants
        }
      })


    })

  }
}








