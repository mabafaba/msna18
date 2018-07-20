
# map to analysis plan

map_to_analysis_plan_all_vars_as_dependent <- function(independent.vars,data,hypothesis.type="direct_reporting"){
  x<-independent.vars %>% unlist %>% as.character %>% unique
  lapply(x, function(indep.var){
    analysisplan <-data.frame(independent.var= indep.var,
                              dependent.var=names(data),
                              hypothesis.type=hypothesis.type,
                              case=paste0("CASE_",hypothesis.type,"_",ifelse(data %>% sapply(is.numeric),"numerical","categorical"),"_categorical")
                              ,stringsAsFactors = F)
    analysisplan <- analysisplan[analysisplan[,"dependent.var"]!= analysisplan[,"independent.var"],]
    return(analysisplan)
  }) %>% do.call(rbind,.)
}
