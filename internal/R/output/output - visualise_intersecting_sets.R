##########
# FUNCTIONS (ignore this part and see below 'example use')
##########



##### Function that returns the most common unique intersections (i.e. a record that appears in columns 
##### 1,2 and 3 will appear in the 1&2&3 intersection as well as in the 1&2, 2&3, 1&3 intersections) 
###   The set size on the right is not significant and should be ignored in the analysis
expand_composite_indicators_to_set_intersections<-function(data,compnames){
  newvarnames<-lapply(2:length(compnames),function(x){
    combn(compnames,x) %>% apply(2,paste,collapse="&")
  }) %>% unlist
  data <- lapply(data[,compnames],as.logical)
  attach(data)
setintersections <- lapply(newvarnames,function(x){
    eval(parse(text = x)) 
  })
detach(data)
setintersections<-as.data.frame(setintersections)
names(setintersections)<-newvarnames
# for msna tool, you might want to use a non-special-character placeholder for "&":
# names(setintersections)<-gsub("&","._.a.n.d._.",newvarnames)
return(setintersections)

}


##### Function that returns the most common unique intersections (i.e. a record that appears in columns 
##### 1,2 and 3 will appear in the 1&2&3 intersection but not in the 1&2 intersection) 
###   The set size on the right is significant 
expand_composite_indicators_to_set_intersections_unique<-function(data,compnames){
  newvarnames<-lapply(2:length(compnames),function(x){
    combn(compnames,x) %>% apply(2,paste,collapse="&")
  }) %>% unlist
  data <- lapply(data[,compnames],as.logical)
  attach(data)
  setintersections <- lapply(newvarnames,function(x){
    setnames <- c()
    not_this_set <- lapply(compnames, function(y){!(y %in% as.vector(strsplit(x, "&") %>% unlist))}) %>% unlist
    if(length(as.vector(strsplit(x, "&")) %>% unlist) == length(compnames)){this_set_name <- x}else{ 
    this_set_name <- paste0(x, "&!", paste(compnames[not_this_set], collapse = "&!"))}
  eval(parse(text = this_set_name))})

  detach(data)
  setintersections<-as.data.frame(setintersections)
  names(setintersections)<-newvarnames
  # for msna tool, you might want to use a non-special-character placeholder for "&":
  # names(setintersections)<-gsub("&","._.a.n.d._.",newvarnames)
  return(setintersections)
}


set_intersection_plot<-function(set_percentages, nsets, nintersects){
 # set_percentages<-set_percentages*100 %>% round
  upset(fromExpression(set_percentages), order.by = "freq",mainbar.y.max = 10, nsets = nsets, nintersects = nintersects)
  
}



