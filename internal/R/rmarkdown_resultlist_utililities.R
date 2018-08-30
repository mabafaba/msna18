rmdrs_decide_showtitle<-function(results,i,input.parameter){
  # by default, don't show
  showtitle<-F
  if(is.null(results$results[[i]])){return(F)}
  # if(is.null(results$results[[i]]$input.parameters[[input.parameter]])){return(T)}
  # if first item, always show:
  title_changed<-function(input.parameter){
  if(i==1){showtitle<-T}else{
    # otherwise show new title whenever the relevant input.parameter has changed:
    this_param<-results$results[[i]]$input.parameters[[input.parameter]]
    previous_param<-results$results[[i-1]]$input.parameters[[input.parameter]]
    # if param available that wasn't before: show
    if(!parameter_available(previous_param) & parameter_available(this_param)){return(TRUE)}
    # if param not available and not before: don't show
    if(!parameter_available(this_param) & !parameter_available(previous_param)){return(FALSE)}
    # if param not available but was before: show
    if(!parameter_available(this_param) & parameter_available(previous_param)){return(TRUE)}
    # if both available and the same: don't show:
    if(this_param==previous_param){return(FALSE)}
    showtitle<-T
  }
  return(showtitle)
  }
  
  input.parameter.changed<-sapply(c("independent.var","dependent.var","repeat.var","repeat.var.value"),title_changed)
  showtitle_final<-min(which(input.parameter.changed))<=which(names(input.parameter.changed)==input.parameter)
  return(showtitle_final)
  }

parameter_available<-function(x){
  if(is.null(x)){return(F)}
  if(is.na(x)|x==""){return(F)}
  return(T)
}
rmdrs_title<-function(results,i,parameter,prefix="##", always.show=F){
 
  if(rmdrs_decide_showtitle(results,i,parameter)|always.show){
    
    if(!parameter_available(results$results[[i]]$input.parameters[[parameter]])){
        if(parameter=="independent.var"){param_formated<-"no comparison"
        }else if(parameter=="dependent.var"){param_formated<-"error: no dependent variable"
        }else if(parameter=="repeat.var"){param_formated<-"all data"
        }else if(parameter=="repeat.var.value"){param_formated<-"all data"
        }else{param_formated<-paste(parameter,": N/A")}
    }else{
      param_formated<-question_get_question_label(results$results[[i]]$input.parameters[[parameter]])
    }
thistitle<-paste0("\n\n\n",prefix," ",param_formated,"\n\n\n")
    cat(thistitle)
  }else{thistitle<-""}
return(thistitle)
  }

rmdrs_independent_title<-function(results,i){
  
}



# rmdrs_title_concatenated<-function(results,i){
#   mycount<-i
#   if(is.na(results$results[[mycount]]$input.parameters$repeat.var)){
#     titl<-paste0(" by ",
#                  question_get_question_label(results$results[[mycount]]$input.parameters$independent.var),
#                  ": ",
#                  question_get_question_label(results$results[[mycount]]$input.parameters$dependent.var))
# 
#   }else{
#     titl<-paste0(" by ",
#                  question_get_question_label(results$results[[mycount]]$input.parameters$independent.var),
#                  ": ",
#                  question_get_question_label(results$results[[mycount]]$input.parameters$dependent.var),
#                  " / ",
#                  question_get_question_label(results$results[[mycount]]$input.parameters$repeat.var),
#                  "=",
#                  results$results[[mycount]]$input.parameters$repeat.var.value)
#   }
# 
#   title<-paste("\n\n\n\n# ",titl,"\n\n\n\n")
# 
#   return(titl)
# }
rmdrs_pretty_summary_table<-function(results,i){
  mycount<-i
  df<-labels_summary_statistic(results$results[[mycount]]$summary.statistic,T,T,T,T,T,T)
  
  kdf<-df
  
  # format for export
    # <-c(
  #   results$results[[mycount]]$input.parameters$repeat.var,
  #   results$results[[mycount]]$input.parameters$dependent.var,
  #   results$results[[mycount]]$input.parameters$independent.var,
  #   "numbers",
  #   "se",
  #   "lover conf. limit",
  #   "upper conf. limit"
  # )
  df_rename<-function(df,from,to,order=NULL){
    
    for(i in 1:length(from)){
      df[[to[i]]]<-df[[from[i]]]
      df[[from[i]]]<-NULL
    }
    inorder<-to[order];inorder<-inorder[inorder%in%colnames(df)]
    notinorder<-names(df)[!(names(df) %in% inorder)]
    df[,c(inorder,notinorder)]
  }
  kdf<-df_rename(kdf,from=c("min",
                            "max",
                            "numbers",
                            "independent.var",
                            "independent.var.value",
                            "dependent.var",
                            "repeat.var",
                            "repeat.var.value",
                            "dependent.var.value"),
                      to= c("Lower confidence limit",
                            "Upper confidence limit",
                            "Summary statistic",
                            "Independent variable",
                            "Value of independent variable",
                            "Variable",
                            "Subset variable",
                            "Subset",
                            "Response"),
                            order=c(8,
                                    9,
                                    7,
                                    5,
                                    6,
                                    3,
                                    1,
                                    2,
                                    4)
                 )
  kdf<-kdf[,which(lapply(kdf,function(x){!all(is.na(x))}) %>% unlist)]
  names(kdf)<-colnames(kdf)
  
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
