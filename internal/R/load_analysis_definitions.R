

load_composite_indicator_definition_weighted_count<-function(file="./internal/input_files/composite_indicators.csv"){
  table <- read.csv.part(file = file,2,2,6) %>% remove.empty.rows 
  table[,c(1,2,4)] <- table[,c(1,2,4)] %>% lapply(to_alphanumeric_lowercase) %>% as.data.frame(stringsAsFactors = F)
  return(table)}

read.csv.part<-function(file,first.row,first.col=1,last.col=NULL){
  headers = read.csv(file, skip = first.row, header = F, nrows = 1, as.is = T)
  df = read.csv(file, skip = first.row+1, header = F,stringsAsFactors = F)
  colnames(df)= headers
  if(is.null(last.col)){last.col<-ncol(df)}
  
  df[,first.col:last.col]
}

remove.empty.rows<-function(df){
  rowempty<-apply(df,1,function(x){
    all(x %in% c(NA,"", "N/A","#N/A","NA"))
  })
  df[!rowempty,]  
}



load_ki_aggregation_definition<-function(file="./internal/KI_aggregation_parameters.csv"){
    def<-read.csv.auto.sep(file)
    def<-lapply(to_alphanumeric_lowercase)
    def    
}

load_cluster_sampling_units <- function(file = "./internal/input_files/cluster_sample.csv"){
  
   sampling_units <- read.csv(file, skip = 1, header=F, stringsAsFactors = F)
   
   if(sampling_units[1] == "yes") {
      sampling_units <- as.character(sampling_units[!is.na(sampling_units)])
      cluster_formula <- paste0("~", reduce(sampling_units[-1], function(x, y) {paste(x, y, sep = "+")}))
   }
   
   else {
     sampling_units <- NULL
   }
   
   return(sampling_units)
   
}