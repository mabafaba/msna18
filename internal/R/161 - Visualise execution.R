# # dependencies:
# install.packages("UpSetR")
require("dplyr")
require("UpSetR")
require("magrittr")

# # which ones should be intersected?
composite_indicator_names<-c("education_PIN", "health_PIN", "protection_PIN", "food_PIN", "livelihood_PIN", "NFI_PIN", "WASH_PIN") %>% to_alphanumeric_lowercase
compinds <- data[,composite_indicator_names] 

##########
# FUNCTIONS (ignore this part and see below 'example use')
##########

expand_composite_indicators_to_set_intersections<-function(data,compnames){
  newvarnames<-lapply(1:length(compnames),function(x){
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

set_intersection_plot<-function(set_percentages){
  set_percentages<-set_percentages*100 %>% round
  upset(fromExpression(set_percentages), order.by = "freq", nsets = 7, nintersects = 12, 
        mainbar.y.label = "% in need per combination of sectors", mainbar.y.max = 30)
  
}

#making design object if you havent 
data <- split.data.frame(data, f = data$hh_type)
original.data <- data

data <- data[[1]]

g <- function(x){
design <- map_to_design(data, weights = "weight_nat")

# assuming they are coercible to logical (e.g. 0's and  1's)
# you can create TRUE/FALSE columns for call combinations with this:
intersected_composites<- expand_composite_indicators_to_set_intersections(data,composite_indicator_names)

#### Take away the single indicators
intersected_composites <- intersected_composites[,-(1:7)]
data <- cbind(data, intersected_composites, stringsAsFactors = F)

### get aggregated results...:
### this should be done with the msna tool in the real example to make sure everything is weighted correctly etc.:
### If you have used load_questionnaire() you could also do this with the "reachR" package (Eliora knows)
### now doing this quick and cheap for the example:

aggregated.results <- svymean(data[,c(682:801)], design, na.rm = T)
aggregated.results.named <- aggregated.results %>% unlist %>% as.data.frame(., stringsAsFactors =F, na.rm = T)

aggregated.results <- aggregated.results.named[,1]
names(aggregated.results) <- rownames(aggregated.results.named)
# aggregated.results %>% str
aggregated.results <- aggregated.results[!is.na(aggregated.results)]

# aggregated.results must be a vector with the names given as set1, set2, set1&set2 ... etc (as given by expand_composite_indicators_to_set_intersections()):
# then we can do:

plot <- set_intersection_plot(aggregated.results)
savePlot(paste0(unique(data$hh_type),"intersections"))}
