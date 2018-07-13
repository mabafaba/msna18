









names_to_labels<-function(results){
  # change independent variable values to labels:
  results <- results %>% split.data.frame(results["independent.var"]) %>% lapply(function(x){
    x[["independent.var.value"]]<-question_get_choice_labels(x[,"independent.var.value"],x[1,"independent.var"])
    return(x)
  }) %>% do.call(rbind,.) 
  # change independent variable values to labels:
  
  results <- results %>% split.data.frame(results["dependent.var"]) %>% lapply(function(x){
    x[["dependent.var.value"]]<-question_get_choice_labels(x[["dependent.var.value"]],x[1,"dependent.var"])
    return(x)
  })
  
  apply(results,function(x){
  question_get_choice_labels()
})
  
}
