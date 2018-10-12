is.numeric.fuzzy<-function(x,minfrac=0.97){
  # are at least minfrac of the values that have data still values after trying to convert them to numbers?
  suppressWarnings(isnum<-(x %>% as.numeric %>% hasdata %>% length)/ (x %>% hasdata %>% length) >=minfrac)
  # this relies on NA's created by as.numeric, which issues a warning we don't want to see
  if(is.na(isnum)){isnum<-FALSE}
  if(is.null(isnum)){isnum<-FALSE}
  return(isnum)
}