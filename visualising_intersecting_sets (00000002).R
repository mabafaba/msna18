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
  upset(fromExpression(set_percentages), order.by = "freq", nsets = 8, nintersects = 15, 
        mainbar.y.label = "Percent of HouseHolds", mainbar.y.max = 100)
  
}


##### EXAMPLE USE:

#List for Uganda: 
# food.pin, wash.pin, protection.pin, health.pin, nfi.shelter.pin, education.pin, environment.pin, livelihoods.pin


# dependencies:
install.packages("UpSetR")
require("dplyr")
require("UpSetR")

# assuming we have data with some (non aggregated) composite_indicators (making an example here):
testdata<-data.frame(test=sample(letters[1:5],100,T),pin1=sample(c(0,1, NA),100,T),pin2=sample(c(0,1),100,T),pin3=sample(c(0,1),100,T))

# which ones should be intersected?
composite_indicator_names<-c("food.pin", "wash.pin", "protection.pin", "health.pin", "nfi.shelter.pin", "education.pin", "environment.pin", "livelihoods.pin")

d_num <- data[,which((names(data)) %in% c("food.pin", "wash.pin", "protection.pin", "health.pin", "nfi.shelter.pin", "education.pin", "environment.pin", "livelihoods.pin"))]

# assuming they are coercible to logical (e.g. 0's and  1's)
# you can create TRUE/FALSE columns for call combinations with this:
intersected_composites<- expand_composite_indicators_to_set_intersections(data,composite_indicator_names)
#### Take away the single indicators
intersected_composites <- intersected_composites[,9:255]
#....
intersected_composites<- expand_composite_indicators_to_set_intersections(testdata,c("pin1","pin2","pin3"))
#....

### get aggregated results...:
### this should be done with the msna tool in the real example to make sure everything is weighted correctly etc.:
### If you have used load_questionnaire() you could also do this with the "reachR" package (Eliora knows)
### now doing this quick and cheap for the example:

data <- cbind(data, intersected_composites, StringsAsFactors = F)

aggregated.results <- svymean(data[,759:1006], design, na.rm = T)
bibou <- aggregated.results %>% unlist %>% as.data.frame(., stringsAsFactors =F, na.rm = T)

aggregated.results <- bibou[,1]
names(aggregated.results) <- rownames(bibou)
aggregated.results <- aggregated.results[!is.na(aggregated.results)]

# aggregated.results must be a vector with the names given as set1, set2, set1&set2 ... etc (as given by expand_composite_indicators_to_set_intersections()):
# then we can do:

plot <- set_intersection_plot(aggregated.results)

aggregated.results


