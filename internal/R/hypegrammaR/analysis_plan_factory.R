
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
map_to_analysis_plan_all_vars_as_dependent <- function(repeat.var = NULL ,independent.vars, data,hypothesis.type="group_difference"){
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
    
    
    analysis_plan_data_table <- analysis_plan_data_table %>% analysisplan_remove_where_select_multiple_choice %>% analysisplan_remove_dependent_same_as_independent
    
    
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
    
    analysisplan <- analysisplan %>% analysisplan_remove_where_select_multiple_choice %>% analysisplan_remove_dependent_same_as_independent
    
    return(analysisplan)}


















analysisplan_remove_where_select_multiple_choice<-function(analysisplan){
  dep_is_sm_choice<-analysisplan$dependent.var %>% sapply(question_is_sm_choice) 
  indep_is_sm_choice<-analysisplan$independent.var %>% sapply(question_is_sm_choice)
  return(analysisplan[!dep_is_sm_choice & !indep_is_sm_choice,])
}

analysisplan_remove_dependent_same_as_independent<-function(analysisplan){
  analysisplan[as.character(analysisplan$dependent.var)!=as.character(analysisplan$independent.var)|is.na(analysisplan$independent.var),]
}




map_to_analysisplan_custom_user_plan<-function(data,analysis_plan_user){
  analysis_plan_user_ALL<-analysis_plan_user[analysis_plan_user$variable=="..all..",]
  analysis_plan_user<-analysis_plan_user[analysis_plan_user$variable!="..all..",]
  
  analysis_plan_user<-analysis_plan_user[!apply(analysis_plan_user,1,function(x){all(is.na(x))}),]

  repeat.var=analysis_plan_user$repeat.for %>% to_alphanumeric_lowercase %>% unname
  independent.var = analysis_plan_user$disaggregate.by %>% to_alphanumeric_lowercase %>% unname
  independent.var<-independent.var %>% (function(x){y<-x;y[is.na(x)|x==""|is.null(x)]<-NA;y})
  dependent.var = analysis_plan_user$variable %>% to_alphanumeric_lowercase %>% unname
  illegal_independent<-independent.var[which(
                                              (
                                                !((is.na(independent.var)) | independent.var=="") # not empty or na (which is allowed)
                                              & !(independent.var %in% names(data)) # and not in data colnames
                                              ))]
  illegal_dependent<-dependent.var[which( !(  dependent.var %in% names(data)))] # not in data
  illegal_repeat<-repeat.var[which(
                                                  (
                                                    !((is.na(repeat.var)) | repeat.var=="") # not empty or na (which is allowed)
                                                    & !(repeat.var %in% names(data)) # and not in data colnames
                                                  ))]

                                                data
  if(length(c(illegal_repeat,illegal_dependent,illegal_independent))>0){
    stop(paste("analysis plan variable name inputs not found in data:\n",
               "repeat for: ",paste(illegal_repeat %>% unique,collapse=", "),"\n",
               "dissagregate by: ", paste(illegal_independent %>% unique,collapse=", "),"\n",
               "variable: ", paste(illegal_dependent %>% unique,collapse=", ")
               ))
  }

  hypothesis.type = rep("group_difference",nrow(analysis_plan_user))
  hypothesis.type[is.na(independent.var)]<-"direct_reporting"
  case<- paste0("CASE_",hypothesis.type,"_",ifelse(data[,dependent.var] %>% sapply(is.numeric),"numerical","categorical"),"_categorical")
  analysisplan<-data.frame(
             independent.var,
             dependent.var,
             hypothesis.type,
             case,
             repeat.var,
             analysis_plan_user[,grep("output",names(analysis_plan_user))],
             stringsAsFactors=F)
  
    var.exists.in.data <- (c(analysisplan[,"dependent.var"],analysisplan[,"independent.var"]) %>% unique) %in% names(data)
  if(any(!var.exists.in.data)){stop(paste0("analysis plan input contains variable names that could not be found in the data or composite indicators:\n",
                                    paste0((c(analysisplan[,"dependent.var"],analysisplan[,"independent.var"]) %>% unique)[var.exists.in.data] %>% names,collapse="\n")))}
  
    analysisplan$output.minimal.chart...width.of.quarter.A4.landscape..FS.[analysisplan$output.minimal.chart...width.of.quarter.A4.landscape..FS.!="yes"]<-"no"
    analysisplan$output.raw.numbers[!(analysisplan$output.raw.numbers)=="yes"]<-"no"
    analysisplan$output.regular.chart..report.[analysisplan$output.regular.chart..report.!="yes"]<-"no"
    analysisplan$output.Map..Mode[analysisplan$output.Map..Mode!="yes"]<-"no"
    analysisplan$output.Map..Average.Percent..of.mode.[analysisplan$output.Map..Average.Percent..of.mode.!="yes"]<-"no"
    return(analysisplan)
}







