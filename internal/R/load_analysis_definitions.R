

load_composite_indicator_definition_weighted_count<-function(file="./internal/input_files/composite_indicators.csv"){
  read.csv.part(file = file,2,2,6) %>% remove.empty.rows %>% lapply(to_alphanumeric_lowercase) %>% do.call(cbind,.) %>% as.data.frame(stringsAsFactors = F)
}

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
