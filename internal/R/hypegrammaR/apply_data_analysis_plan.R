




apply_data_analysis_plan<-function(data,analysisplan){
  analysisplan$percentcomplete<-paste0(floor(1:nrow(analysisplan)/nrow(analysisplan)*100),"%\n\n")
  results<- apply(analysisplan,1,function(x){
    this_valid_data<-data[
      which(
        !(is.na(data[,x["dependent.var"]])) &
          (!(is.na(data[,x["independent.var"]])))
      ),
      ]
    printparamlist(x,"1/2: calculating summary statistics and hypothesis tests")
    
    result<-analyse_indicator(this_valid_data,
                              dependent.var = x["dependent.var"],
                              independent.var = x["independent.var"] ,
                              hypothesis.type =  x["hypothesis.type"],
                              sampling.strategy.cluster = FALSE,
                              sampling.strategy.stratified = TRUE,
                              case=x["case"])
    
    return(result)
  }
  )
  
  # names(results)<-analysisplan$dependent.var
  return(results)
  
}




printparamlist<-function(x,title=""){
  
  cat("\014")
  cat(title)
  cat("\n")
  cbind(names(x[-length(x)]),as.matrix(x)[-length(x)]) %>% apply(1,paste,collapse=" = '") %>% paste(collapse="'\n") %>% cat
  cat("\n\n")
  cat(blue(paste("----  ",x["percentcomplete"])))
}




     