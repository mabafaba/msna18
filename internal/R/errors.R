insure.string.is.column.header<-function(df,string,warning=FALSE){
  if(!all(string%in%names(df))){
    if(!warning){ stop(paste(string[!string%in%names(df)],"does not match a column name in the data frame.\n"))}
    else        { warning(paste(string[!string%in%names(df)],"does not match a column name in the data frame.\n"))}
  }
}

insure.is.single.value<-function(x,warning=F){
  if(length(x)!=1){
    if(!warning){stop(paste(deparse(substitute(x)),"must be a single value"))}
    else         {warning(paste(deparse(substitute(x)),"must be a single value"))}
  }
}

insure.has.data<-function(x){
  if(length(hasdata(x))==0){stop(paste(deparse(substitute(x)), "can not be NULL, NA or an empty string."))}
}

insure.is.matrix<-function(x){
  if(!is.matrix(x)){stop(paste(deparse(substitute(x)), "must be a matrix"))}
}

insure.same.length<-function(x,y){
  if(length(x)!=length(y)){stop(paste(deparse(substitute(x)),"and",deparse(substitute(y)),"must have the same length"))}
}



