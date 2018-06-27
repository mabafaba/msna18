
# map to analysis plan

map_to_analysis_plan_all_vars_as_dependent <- function(each.x.as.independent.in.var,data){
  x<-unique(data[[each.x.as.independent.in.var]])
  lapply(x, function(indep.var){
    analysisplan <-data.frame(independent.var= indep.var,
                              dependent.var=names(data),
                              hypothesis.type="group_difference",
                              case=paste0("CASE_group_difference_",ifelse(data %>% sapply(is.numeric),"numerical","categorical"),"_categorical")
                              ,stringsAsFactors = F)
    analysisplan <- analysisplan[analysisplan[,"dependent.var"]!= analysisplan[,"independent.var"],]
    return(analysisplan)
  }) %>% do.call(rbind,.)
}





produce_analysis_plan_direct_report_all_variables <- function(independent.vars,data,data_types){
  x<-unique(data[[independent.vars]])
  lapply(x, function(indep.var){
    analysisplan <-data.frame(independent.var= indep.var,
                              dependent.var=names(data),
                              hypothesis.type="direct_reporting",
                              case=paste0("CASE_direct_reporting_",data_types,"_categorical")
                              ,stringsAsFactors = F)
    analysisplan <- analysisplan[analysisplan[,"dependent.var"]!= analysisplan[,"independent.var"],]
    return(analysisplan)
  }) %>% do.call(rbind,.)
}




