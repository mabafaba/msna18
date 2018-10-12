##########
# FUNCTIONS (ignore this part and see below 'example use')
##########
install.packages("upset")
library("upset")
compnames <- c("WASH_PIN", "NFI_PIN", "education_PIN", "health_PIN", "protection_PIN", "livelihood_PIN", "food_PIN") %>% to_alphanumeric_lowercase

comp_ind <- data[,compnames]
write.csv(comp_ind, "./output/PIN.csv")
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


set_intersection_plot<-function(set_percentages){
  set_percentages<-set_percentages*100 %>% round
  upset(fromExpression(set_percentages), order.by = "freq",mainbar.y.max = 100)
  
}

# 
# ##### EXAMPLE USE:
# 
# 
# # dependencies:
# install.packages("UpSetR")
# require("dplyr")
# require("UpSetR")
# 
# # assuming we have data with some (non aggregated) composite_indicators (making an example here):
testdata<-data.frame(test=sample(letters[1:5],100,T),pin1=sample(c(0,1),100,T),pin2=sample(c(0,1),100,T),pin3=sample(c(0,1),100,T), pin4=sample(c(0,1),100,T))
# 
# # which ones should be intersected?
composite_indicator_names<-c("pin1","pin2","pin3", "pin4")
# 
# # assuming they are coercible to logical (e.g. 0's and  1's)
# # you can create TRUE/FALSE columns for call combinations with this:
intersected_composites<- expand_composite_indicators_to_set_intersections_unique(data,compnames)
# 
# #....
# #....
# 
# ### get aggregated results...:
# ### this should be done with the msna tool in the real example to make sure everything is weighted correctly etc.:
# ### If you have used load_questionnaire() you could also do this with the "reachR" package (Eliora knows)
# ### now doing this quick and cheap for the example:
aggregated.results<-sapply(intersected_composites,mean)
# 
# # aggregated.results must be a vector with the names given as set1, set2, set1&set2 ... etc (as given by expand_composite_indicators_to_set_intersections()):
# # then we can do:
set_intersection_plot(aggregated.results)



