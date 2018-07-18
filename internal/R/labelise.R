# for the functions that actually get the labels, see load_questionniare.R

map_to_labelisation<-function(type){
  if(type=="summary.statistic"){
    return(labels_summary_statistic)
  }
}

labels_summary_statistic<-function(x){
  summary.statistic<-x
  if(length(unique(summary.statistic[,"dependent.var"]))>1){stop("labels_summary_statistic only works for a single combination of dependent and independent variable.")}
  if(length(unique(summary.statistic[,"independent.var"]))>1){stop("labels_summary_statistic only works for a single combination of dependent and independent variable.")}
  summary.statistic[,"dependent.var.value"]<-question_get_choice_labels(summary.statistic[,"dependent.var.value"],
                                                                        summary.statistic[,"dependent.var"][1])
  
  summary.statistic[,"independent.var.value"]<-question_get_choice_labels(summary.statistic[,"independent.var.value"],
                                                                          summary.statistic[,"independent.var"][1])
  
  summary.statistic[,"dependent.var"]<-question_get_question_label(summary.statistic[,"dependent.var"])
  summary.statistic[,"independent.var"]<-question_get_question_label(summary.statistic[,"independent.var"])
  
  return(summary.statistic)
}

