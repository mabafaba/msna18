apply_data_analysis_plan<-function(data,analysisplan){
  if(!is.null(analysisplan[,"repeat.var"])){
    repeat.var <- as.character(unique(analysisplan$repeat.var)[1])
      repeat.var.value <- unique(data[[repeat.var]])
      repeat.var.value <- repeat.var.value[!is.na(repeat.var.value )]
      analysisplan <- analysisplan %>% slice(rep(1:n(), each = length(repeat.var.value))) %>% cbind(.,repeat.var.value, stringsAsFactors = F)}
  analysisplan$percentcomplete<-paste0(floor(1:nrow(analysisplan)/nrow(analysisplan)*100),"%\n\n")
  x<-analysis_plan_all_vars_no_disag[10,] %>% unlist
  results<- apply(analysisplan,1,function(x){
    if(!is.null(x["repeat.var"])&(!is.na(x["repeat.var"][1]))){
      this_valid_data <- data[data[,x["repeat.var"]] == as.character(x["repeat.var.value"]),]}else{
        this_valid_data<-data
      }
    
    this_valid_data<-this_valid_data[
      which(
        !(is.na(data[,x["dependent.var"]]))),]
    if(!is.na(x["independent.var"])){
      this_valid_data <- this_valid_data[
        which(
          !(is.na(data[,x["independent.var"]]))),]
    }

    printparamlist(x,"1/2: calculating summary statistics and hypothesis tests")
    .write_to_log(printparamlist(x,"1/2: calculating summary statistics and hypothesis tests"))
    if(is.na(x["independent.var"])|is.null(x["independent.var"])){
      indep.var <- NULL}else{
        indep.var <- x["independent.var"]
      }
    result<-analyse_indicator(this_valid_data,
                              dependent.var = x["dependent.var"],
                              independent.var = indep.var ,
                              hypothesis.type =  x["hypothesis.type"],
                              sampling.strategy.cluster = FALSE,
                              sampling.strategy.stratified = TRUE,
                              case=x["case"])
    
    if(!is.null(x["repeat.var"])&(!is.na(x["repeat.var"]))){
      result$input.parameters$repeat.var<-x["repeat.var"]
      result$input.parameters$repeat.var.value<-x["repeat.var.value"]
    }
    
    if(!is.null(result$summary.statistic)){
      if(nrow(result$summary.statistic)>0){
        result$summary.statistic$repeat.var<-x["repeat.var"]
        result$summary.statistic$repeat.var.value<-x["repeat.var.value"]
      }else{
        result$summary.statistic$repeat.var<-character(0)
        result$summary.statistic$repeat.var.value<-character(0)
        }
    }
    return(result)})
  
  # names(results)<-analysisplan$dependent.var
  return(results)
  
}




printparamlist<-function(x,title=""){
  # if(!exists("debugging_mode")){cat("\014")}else{if(!debugging_mode){cat("\014")}}
  cat(title)
  cat("\n")
  cbind(names(x[-length(x)]),as.matrix(x)[-length(x)]) %>% apply(1,paste,collapse=" = '") %>% paste(collapse="'\n") %>% cat
  cat("\n\n")
  cat(blue(paste("----  ",x["percentcomplete"])))
}




     