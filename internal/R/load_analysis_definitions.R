# 
# load_composite_indicator_definition_weighted_count<-function(file="./internal/input_files/composite_indicators.csv"){
#   table <- read.csv.part(file = file,first.row = 3,first.col = 2,last.col = 6) %>% remove.empty.rows 
#   table[,c("var","new.var.name","condition")] <- table[,c("var","new.var.name","condition")] %>% lapply(to_alphanumeric_lowercase) %>% as.data.frame(stringsAsFactors = F)
#   
#   return(table)}

load_composite_indicator_definition_weighted_count<-function(file="./internal/input_files/composite_indicators.csv"){
  
  table_raw<-read.csv.auto.sep(file,header=F)
  header_row <-  apply(table_raw[1:10,],1,function(x){
    !any(is.na(match(c("value","condition","weight"),x)))
  }) %>% which
  
  headers<-table_raw[header_row,]
  conditions<-table_raw[(header_row+1):nrow(table_raw),]
  colnames(conditions)<-headers
  conditions<-conditions[,c("new.var.name","var","value","condition","weight")]
  conditions<- remove.empty.rows(conditions)
  # table <- read.csv.part(file = file,first.row = header_row-1,first.col = 2,last.col = 6) %>% 
  conditions[,c("var","new.var.name","condition")] <- conditions[,c("var","new.var.name","condition")] %>% lapply(to_alphanumeric_lowercase) %>% as.data.frame(stringsAsFactors = F)
  
  
  return(conditions)
  
}



read.csv.part<-function(file,first.row,first.col=1,last.col=NULL){
  headers = read.csv.auto.sep(file, skip = first.row, header = F, nrows = 1)
  df = read.csv.auto.sep(file, skip = first.row+1, header = F,stringsAsFactors = F)
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
