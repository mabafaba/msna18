load_cluster_sampling_units <- function(file = "./internal/input_files/cluster_sample.csv"){
  
  sampling_units <- read.csv(file, skip = 1, header=F, stringsAsFactors = F)
  
  if(sampling_units[1] == "yes") {
    sampling_units <- as.character(sampling_units[!is.na(sampling_units)])
  }
  
  else {
    sampling_units <- NULL
  }
  
  cluster_formula <- function() {
    
    if(is.null(sampling_units)) {
      cluster_formula <- "~1"
    }
    
    else {
      cluster_formula <- paste0("~", reduce(sampling_units[-1], function(x, y) {paste(x, y, sep = "+")}))
    }
    
    return(cluster_formula)
  }
}