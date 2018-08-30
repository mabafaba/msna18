apply_data_analysis_plan<-function(data,analysisplan){
  analysisplan<-analysisplan[!is.na(analysisplan$dependent.var),]
  if(!is.null(analysisplan[,"repeat.var"])){
      # repeat.var.value <- unique(data[[repeat.var]])
      # repeat.var.value <- repeat.var.value[!is.na(repeat.var.value )]
      analysisplan.no.repeat <- analysisplan[analysisplan$repeat.var %in% c(NA,""," "),]
      if(!nrow(analysisplan.no.repeat)<1){
      analysisplan.no.repeat$repeat.var<-NA
      analysisplan.no.repeat$repeat.var.value<-NA
      }else{
        analysisplan.no.repeat$repeat.var<-character(0)
        analysisplan.no.repeat$repeat.var.value<-character(0)
      }
      analysisplan.repeat<-analysisplan[!(analysisplan$repeat.var %in% c(NA,""," ")),]
      analysisplan.repeat<-(1:nrow(analysisplan.repeat)) %>% lapply(function(ap_row_index){
        ap_row<-analysisplan.repeat[ap_row_index,] %>% unlist
        ap_row_expanded<-matrix(ap_row,nrow=length(unique(data[[ap_row["repeat.var"]]])),
                                ncol=length(ap_row),byrow=TRUE) %>% as.data.frame(stringsAsFactors=F)
        colnames(ap_row_expanded)<-names(ap_row)
        ap_row_expanded$repeat.var.value<-unique(as.character(data[[ap_row["repeat.var"]]]))
        ap_row_expanded
      }) %>% do.call(rbind,.)
      
      # analysisplan.repeat <- lapply(repeat.var, function(x){
      #   if(!x %in% c(NA, "", " ")){
      #   repeat.var.value <- unique(data[[x]])
      #   repeat.var.value <- repeat.var.value[!is.na(repeat.var.value )]
      #   analysisplan %>% filter(repeat.var %in% x) %>% slice(rep(1:n(), each = length(repeat.var.value))) %>% cbind(.,repeat.var.value, stringsAsFactors = F)}}) %>% do.call(rbind,.)
    
      analysisplan <- rbind(analysisplan.no.repeat, analysisplan.repeat, stringsAsFactors = F)
    }
      analysisplan$percentcomplete<-paste0(floor(1:nrow(analysisplan)/nrow(analysisplan)*100),"%\n\n")
  
   results<- apply(analysisplan,1,function(x){
    if(!(x["repeat.var"]) %in% c(NULL, "", " ", NA)){
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
    # .write_to_log(printparamlist(x,"1/2: calculating summary statistics and hypothesis tests"))
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
    }else{
      result$input.parameters$repeat.var<-NA
      result$input.parameters$repeat.var.value<-NA
      
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
  
  return(list(results=results,analysisplan=analysisplan))
  
}




printparamlist<-function(x,title=""){
  # if(!exists("debugging_mode")){cat("\014")}else{if(!debugging_mode){cat("\014")}}
  cat("\014")
  cat(title)
  cat("\n")
  cbind(names(x[-length(x)]),as.matrix(x)[-length(x)]) %>% apply(1,paste,collapse=" = '") %>% paste(collapse="'\n") %>% cat
  cat("\n\n")
  cat(blue(paste("----  ",x["percentcomplete"])))
}




     