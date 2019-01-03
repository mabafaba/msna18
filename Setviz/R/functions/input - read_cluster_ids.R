load_cluster_sampling_units <- function(cluster.variable=NULL){
  cluster.variable<-cluster.variable[cluster.variable!="" & !is.na(cluster.variable)]
  if(!is.null(cluster.variable)) {
    sampling_units <- as.character(cluster.variable[!is.na(cluster.variable)])
  }else {
    sampling_units <- NULL
  }
  
  cluster_formula <- function() {
    
    if(is.null(sampling_units)) {
      cluster_formula <- "~1"
    }
    
    else {
      cluster_formula <- paste0("~", reduce(sampling_units, function(x, y) {paste(x, y, sep = "+")}))
    }
  }
  return(cluster_formula)
}



