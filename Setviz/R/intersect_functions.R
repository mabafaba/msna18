#'Expand the binary indicators to set intersections
#'
#'@param data a dataframe containing all the 1,0 indicators in varnames
#'@param varnames a vector containing the names of variables to be used in the intersection
#'@return A dataframe containing the intersections of all 
#'@examples
#'
expand_composite_indicators_to_set_intersections<-function(data,varnames){
  # creates newvarnames, a vector for the new combinations of variables using all combinations of varnames with '&'
  newvarnames<-lapply(1:length(varnames),function(x){
    combn(varnames,x) %>% apply(2,paste,collapse="&")
  }) %>% unlist
  # coverts the columns in the data corresponding to varnames to T/F columns
  data <- lapply(data[,varnames],as.logical)
  attach(data)
  # creates setintersections, a dataframe of newvarnames with T/F in each column
  setintersections <- lapply(newvarnames,function(x){
    eval(parse(text = x))
  })
  detach(data)
  setintersections<-as.data.frame(setintersections)
  names(setintersections)<- newvarnames
  # for msna tool, you might want to use a non-special-character placeholder for "&":
  # names(setintersections)<-gsub("&","._.a.n.d._.",newvarnames)
  return(setintersections)

}

#'Create a plot from the percentages in each set
#'
#'@param set_percentages a names vector with the percentages for each combination
#'@param nsets number of sets to look at 
#'@param nsets number of intersections to look at, the default being 12 
#'@param label the label to be added to the plot
#'@return A plot object
#'@examples
#'
set_intersection_plot<-function(set_percentages, nsets, nintersects = 12, label = NULL){
  set_percentages <- set_percentages*100 %>% round
  label <- as.character(label)
  upset(fromExpression(set_percentages),
        order.by = "freq", nintersects = nintersects, nsets = nsets,
        mainbar.y.label = label
        #, mainbar.y.max = 50 
  )
}

set_percentages <- function(data, varnames, exclude_unique = T){
  
  # create the design object with the weights if applicable 
  design <- map_to_design(~1,weights = data$weight, data = data)
  
  # assuming they are coercible to logical (e.g. 0's and  1's)
  # you can create TRUE/FALSE columns for call combinations with this:
  intersected_composites<- expand_composite_indicators_to_set_intersections(data,varnames)
  
  #### Take away the single indicators
  if(exclude_unique = T){
    intersected_composites <- intersected_composites[,-(1:length(varnames))]
    data <- cbind(data, intersected_composites, stringsAsFactors = F)
  }

  ### get aggregated results...:
  ### this should be done with the msna tool in the real example to make sure everything is weighted correctly etc.:
  
  aggregated.results <- svymean(data[,varnames], design, na.rm = T)
  aggregated.results.named <- aggregated.results %>% unlist %>% as.data.frame(., stringsAsFactors =F, na.rm = T)
  
  aggregated.results <- aggregated.results.named[,1]
  names(aggregated.results) <- rownames(aggregated.results.named)
  # aggregated.results %>% str
  aggregated.results <- aggregated.results[!is.na(aggregated.results)]
  
  # aggregated.results must be a vector with the names given as set1, set2, set1&set2 ... etc (as given by expand_composite_indicators_to_set_intersections()):
  # then we can do:
  save_the_plot <- function(aggregated.results){
  plot <- set_intersection_plot(aggregated.results)
  savePlot(paste0(unique(data$hh_type),"intersections"))}