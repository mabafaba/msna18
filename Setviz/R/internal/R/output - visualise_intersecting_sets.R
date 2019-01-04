##########
# FUNCTIONS (ignore this part and see below 'example use')
##########
require("UpSetR")
require("magrittr")
rm(list = ls())

data <- read.csv("internal/input_files/UKR_MSNA_pin_dummies.csv")


# subset the data if you need to
data <- data[which(data$fem_hh == "Male"),]

# # which ones should be intersected?
composite_indicator_names<-c("MCNA_FoodSec1", "health_score1", "wash3_score1", 
                             "MCNA_education_score1", "protection_scoreSFHH2", "shelter_score1", 
                             "live_score1") 
composite_indicator_names<-c("edu_pin_bin_population", "health_pin_bin", "food_pin_bin", "shelter_pin_bin.", "wash_pin_bin") %>% to_alphanumeric_lowercase

### check that the names are all good
composite_indicator_names %in% names(data)

## subset
compinds <- data[,composite_indicator_names] 
##### Function that returns the most common unique intersections (i.e. a record that appears in columns 
##### 1,2 and 3 will appear in the 1&2&3 intersection but not in the 1&2 intersection) 
###   The set size on the right is significant 
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
  upset(fromExpression(set_percentages), 
        order.by = "freq", nintersects = 10, nsets = 7,
        mainbar.y.label = "% in need per combination of sectors"  
      , mainbar.y.max = 15
  )
}

#making design object if you havent 
data <- split.data.frame(data, f = data$area_km)
original.data <- data

data <- original.data[[3]]



g <- function(x){
  design <- svydesign(~1,weights = data$weight, data = data )
  
  # assuming they are coercible to logical (e.g. 0's and  1's)
  # you can create TRUE/FALSE columns for call combinations with this:
  intersected_composites<- expand_composite_indicators_to_set_intersections(data,composite_indicator_names)
  
  #### Take away the single indicators
  intersected_composites <- intersected_composites[,-(1:5)]
  data <- cbind(data, intersected_composites, stringsAsFactors = F)
  
  ### get aggregated results...:
  ### this should be done with the msna tool in the real example to make sure everything is weighted correctly etc.:
  ### If you have used load_questionnaire() you could also do this with the "reachR" package (Eliora knows)
  ### now doing this quick and cheap for the example:
  
  aggregated.results <- svymean(data[,c(31:56)], design, na.rm = T)
  aggregated.results.named <- aggregated.results %>% unlist %>% as.data.frame(., stringsAsFactors =F, na.rm = T)
  
  aggregated.results <- aggregated.results.named[,1]
  names(aggregated.results) <- rownames(aggregated.results.named)
  # aggregated.results %>% str
  aggregated.results <- aggregated.results[!is.na(aggregated.results)]
  
  # aggregated.results must be a vector with the names given as set1, set2, set1&set2 ... etc (as given by expand_composite_indicators_to_set_intersections()):
  # then we can do:
  
  plot <- set_intersection_plot(aggregated.results)
  savePlot(paste0(unique(data$hh_type),"intersections"))}
