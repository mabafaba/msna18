

rmdrs_decide_showtitle<-function(results,i,input.parameter){
  # by default, don't show
  showtitle<-F
  # if first item, always show:
  if(i==1){showtitle<-T}else{
    # otherwise show new title whenever the relevant input.parameter has changed:
  if(results$results[[i]]$input.parameters[[input.parameter]]!=results$results[[i-1]]$input.parameters[[input.parameter]]){
    showtitle<-T}
  }
  return(showtitle)
}

rmdrs_title<-function(results,i,parameter,prefix="##", always.show=F){
  parameter_available<-function(x){
    if(is.null(x)){return(F)}
    if(is.na(x)|x==""){return(F)}
    return(T)
  }
  if(rmdrs_decide_showtitle(results,i,parameter)|always.show){
    
    if(!parameter_available(results$results[[i]]$input.parameters[[parameter]])){
        if(parameter=="independent.var"){param_formated<-"no comparison"
        }else if(parameter=="dependent.var"){param_formated<-"error: no dependent variable"
        }else if(parameter=="repeat.var"){param_formated<-"all data"
        }else if(parameter=="repeat.var.value"){param_formated<-"no subset"
        }else{param_formated<-paste(parameter,": N/A")}
    }else{
      param_formated<-question_get_question_label(results$results[[i]]$input.parameters[[parameter]])
    }

    thistitle<-paste0("\n\n\n\n\n",prefix," ",param_formated,"\n\n\n")
    cat(thistitle)
  }
}
rmdrs_independent_title<-function(results,i){
  
}



rmdrs_title_concatenated<-function(results,i){
  mycount<-i
  if(is.na(results$results[[mycount]]$input.parameters$repeat.var)){
    titl<-paste0(" by ",
                 question_get_question_label(results$results[[mycount]]$input.parameters$independent.var),
                 ": ",
                 question_get_question_label(results$results[[mycount]]$input.parameters$dependent.var))
    
  }else{
    titl<-paste0(" by ",
                 question_get_question_label(results$results[[mycount]]$input.parameters$independent.var),
                 ": ",
                 question_get_question_label(results$results[[mycount]]$input.parameters$dependent.var),
                 " / ",
                 question_get_question_label(results$results[[mycount]]$input.parameters$repeat.var),
                 "=",
                 results$results[[mycount]]$input.parameters$repeat.var.value)
  }
  
  title<-paste("\n\n\n\n# ",titl,"\n\n\n\n")
  
  return(titl)
}




rmdrs_pretty_summary_table<-function(results,i){
  mycount<-i
  df<-results$results[[mycount]]$summary.statistic[,c(
    "repeat.var.value",
    "dependent.var.value",
    "independent.var.value",
    "numbers",
    "se",
    "min",
    "max")]
  
  kdf<-df
  
  # format for export
  names(kdf)<-c(
    results$results[[mycount]]$input.parameters$repeat.var,
    results$results[[mycount]]$input.parameters$dependent.var,
    results$results[[mycount]]$input.parameters$independent.var,
    "numbers",
    "se",
    "lover conf. limit",
    "upper conf. limit"
  )
  
  kdf<-kdf[,which(lapply(kdf,function(x){!all(is.na(x))}) %>% unlist)]
  if(is.null(results$results[[mycount]]$hypothesis.test$results$p.value)){
    results$results[[mycount]]$hypothesis.test$results$p.value<-NA
  }
return(kdf)
}









rmdrs_pretty_hypothesis_test_result<-function(results,i){
  mycount<-i
if(case=="CASE_group_difference_numerical_categorical" & !is.null(results$results[[mycount]]$hypothesis.test$name)){
  cat_stat<-paste0(results$results[[mycount]]$hypothesis.test$name,': p=',tryround(results$result[[mycount]]$hypothesis.test$result$p.value[1],3),
                   '; ',paste0('df=',tryround(results$results[[mycount]]$hypothesis.test$parameters$df,1)))
  } else if(case=="CASE_group_difference_categorical_categorical" & !is.null(results$results[[mycount]]$hypothesis.test$name)){
    cat_stat<-paste0(results$results[[mycount]]$hypothesis.test$name,': p=',tryround(results$results[[mycount]]$hypothesis.test$results$p.value[1],3),
                     '; ',paste0('ddf=',tryround(results$results[[mycount]]$hypothesis.test$parameters$ddf,1)))
  }else {
    cat_stat<-"no hypothesis test performed"
  }
  
}
