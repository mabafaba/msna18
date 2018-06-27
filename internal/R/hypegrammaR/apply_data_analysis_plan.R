apply_data_analysis_plan<-function(data,analysisplan){
  
  results<- apply(analysisplan,1,function(x){
    this_valid_data<-data[
      which(
        !(is.na(data[,x["dependent.var"]])) &
          (!(is.na(data[,x["independent.var"]])))
      ),
      ]
    
    analyse_indicator(this_valid_data,
                      dependent.var = x["dependent.var"],
                      independent.var = x["independent.var"] ,
                      # hypothesis.type =  x["hypothesis.type"],
                      sampling.strategy.cluster = FALSE,
                      sampling.strategy.stratified = TRUE,
                      case=x["case"])
  }
  )
  
  names(results)<-analysisplan$dependent.var
  return(results)
  
}