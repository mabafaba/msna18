##########
# FUNCTIONS (ignore this part and see below 'example use')
##########
# 
# repeat.var <- "state"
# independent.var <- "group_stratum"
# independent.var.2 <- "vulnerability_index"

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
  newvarnames<-lapply(1:length(compnames),function(x){
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

count_sector_pins <- function(data,compnames,design){
  table.intersect <- expand_composite_indicators_to_set_intersections_unique(data = data, compnames = compnames)
  count <- str_count(names(table.intersect), "&") + 1
  numbered_groups <- unique(count) #head(unique(count), -1)
  multiples <- lapply(numbered_groups, function(x){sum <- rowSums(table.intersect[, count == x,drop=F])
  return(sum)}) 
  names(multiples) <- numbered_groups
  #name(sum) <- paste("sum_of", x)
  return(multiples)}

# 
# ##### EXAMPLE USE:
# 
# 
# # dependencies:
# install.packages("UpSetR")
# require("dplyr")
# require("UpSetR")
# 

# which ones should be intersected?

composite_indicator_names<-c("foodsec_index", "wash_index", "protection_index",  
                             "education_index", "health_index", "livelihood_index", "shelter_index", "nutrition_index")

# assuming they are coercible to logical (e.g. 0's and  1's)
# you can create TRUE/FALSE columns for call combinations with this:
intersected_composites <- expand_composite_indicators_to_set_intersections_unique(data,composite_indicator_names)
name.comp <- names(intersected_composites)
intersected_composites <- intersected_composites %>% as.data.frame

#### Take away the single indicators
data <- cbind(data, intersected_composites, stringsAsFactors = F)
intersected_composites <- data.frame(matrix(unlist(intersected_composites), 
                                     ncol = length(intersected_composites), byrow=F), 
                                     stringsAsFactors = F)

names(intersected_composites) <- name.comp
design <- map_to_design(data)


### get aggregated results...:
### this should be done with the msna tool in the real example to make sure everything is weighted correctly etc.:
### If you have used load_questionnaire() you could also do this with the "reachR" package (Eliora knows)
### now doing this quick and cheap for the example:

# if(is.null(independent.var)){
# aggregated.results <- svymean(intersected_composites, design, na.rm = T)
# aggregated.results.named <- aggregated.results %>% unlist %>% as.data.frame(., stringsAsFactors =F, na.rm = T)}
# 


 dependent.var<-names(intersected_composites)[1]
 percent_with_confints_select_one_groups_two_independent<-function(design,dependent.var,independent.var,independent.var.2){
 formula_string<-  paste0("~", dependent.var)
 by <- paste0("~", independent.var, "+", independent.var.2)
 aggregated.results <- svyby(formula(formula_string), formula(by), design=design,svymean, na.rm = T)}

 
 # lapplying over the whole damn thing
 # lapply(names(intersected_composites),percent_with_confints_select_one_groups_two_independent)

#
# 
# aggregated.results <- aggregated.results.named[,1]
# names(aggregated.results) <- rownames(aggregated.results.named)
# aggregated.results <- aggregated.results[!is.na(aggregated.results)]
# aggregated.results %>% head

# aggregated.results must be a vector with the names given as set1, set2, set1&set2 ... etc (as given by expand_composite_indicators_to_set_intersections()):
# then we can do:
s
