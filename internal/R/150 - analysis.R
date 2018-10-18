analysisplan<-map_to_analysisplan_custom_user_plan(data,analysis_plan_user)
analysisplan <- analysisplan[order(as.numeric(row.names(analysisplan))), ]   # the analysis plan in the order of the input file

# analysisplan$case <- c("CASE_group_difference_categorical_categorical", "CASE_group_difference_categorical_categorical")
logmessage(silver("applying analysis plan.."))
data$calc.household<-NA
results<-apply_data_analysis_plan(data,analysisplan)

results$results %>% lapply(function(x){x[["message"]]})

results$analysisplan_log<-results$analysisplan