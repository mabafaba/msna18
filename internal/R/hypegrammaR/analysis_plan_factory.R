
# 
# ### make many different analysis plans 
# many_plans <- function(repeat.var, independent.vars, data, hypothesis.type="direct_reporting"){
#   x<-repeat.var %>% unlist %>% as.character %>% unique
#   lapply(x, function(rep.var){
#     analysis_plan <- map_to_analysis_plan_all_vars_as_dependent(independent.vars, data, hypothesis.type = "direct_reporting")
#     analysisplan_x <- rbind(analysis_plan, )
#     analysisplan <- analysisplan[analysisplan[,"dependent.var"]!= analysisplan[,"independent.var"],]
#     return(analysisplan_x)
#   })
# }

# map to analysis plan
map_to_analysis_plan_all_vars_as_dependent <- function(repeat.var = NULL ,independent.vars, data,hypothesis.type="direct_reporting"){
  # rep.var <- repeat.var %>% unlist %>% as.character %>% unique
  # lapply(rep.var, function(value){
    x <- independent.vars %>% unlist %>% as.character %>% unique
    analysisplan_list <- lapply(x, function(indep.var){
      analysisplan <- data.frame(independent.var= indep.var,
                                dependent.var=names(data),
                                hypothesis.type=hypothesis.type,
                                case=paste0("CASE_",hypothesis.type,"_",ifelse(data %>% sapply(is.numeric),"numerical","categorical"),"_categorical")
                                ,stringsAsFactors = F)
      analysisplan <- analysisplan[analysisplan[,"dependent.var"]!= analysisplan[,"independent.var"],]
      return(analysisplan)})
      if(!is.null(repeat.var)){analysis_plan_data_table <- analysisplan_list %>% do.call(rbind, .) %>% cbind(.,repeat.var, stringsAsFactors = F)}
      if(is.null(repeat.var)){analysis_plan_data_table <- analysisplan_list %>% do.call(rbind, .)}
      return(analysis_plan_data_table)}

# map to analysis plan
map_to_analysis_plan_all_vars_no_disag <- function(repeat.var = NULL ,independent.vars = NULL,data,hypothesis.type="direct_reporting"){
  # rep.var <- repeat.var %>% unlist %>% as.character %>% unique
  # lapply(rep.var, function(value){
    analysisplan <- data.frame(independent.var = rep(NA, ncol(data)),
                               dependent.var=names(data),
                               hypothesis.type=hypothesis.type,
                               case=paste0("CASE_",hypothesis.type,"_",ifelse(data %>% sapply(is.numeric),"numerical_","categorical_"))
                               ,stringsAsFactors = F)
    if(!is.null(repeat.var)){analysisplan %<>% cbind(.,repeat.var, stringsAsFactors = F)}
    return(analysisplan)}
