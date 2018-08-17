# composite_indicator_data_loss_diagnostic<-function(data,composite_indicators_definitions_weighted_counts,write_log=T, warning=T){
#   links<-unique(composite_indicators_definitions_weighted_counts[,c("var","new.var.name")])
#   sourceNA <- apply(data[,unique(links[,"var"])],2,is.na)
#   sourceSkipped <- lapply(unique(links[,"var"]),question_is_skipped,data=data) 
#   sourceSkipped %>% glimpse
#   sourceSkipped[[1]] %>% table
#   
#   targetNA <- apply(data[,composite_indicators_definitions_weighted_counts$new.var.name],2,is.na)
#   targetSkipped <- lapply(composite_indicators_definitions_weighted_counts$new.var.name,question_is_skipped,data=data)
#   targetSkipped %>% glimpse
#   undebug(selected_condition_fulfilled)
# }
# 
# undebug(question_is_skipped)
# question_is_skipped(data,"type_shelter")
# 
# 


