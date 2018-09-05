hypothesis_test_chisquared<-function(dependent.var,independent.var,design){
  
  if(tryCatch({question_is_select_multiple(dependent.var)},error={FALSE})){
    return(hypothesis_test_chisquared_select_multiple(dependent.var,independent.var,design))
  }
  hypothesis_test_chisquared_select_one(dependent.var,independent.var,design)
  
}
  
hypothesis_test_chisquared_select_one <- function(dependent.var,
                                  independent.var,
                                  design){


  # .write_to_log(paste0("chisquared testing:\n",
  #                      "dependent.var: ", dependent.var,"\n",
  #                      "independent.var:",independent.var,"\n\n"
  #               ))
  formula_string<-paste0("~",independent.var, "+", dependent.var)
  # drop empty choices from levels (to avoid many empty cells, potentially breaking the chisquared test)
  if(is.factor(design$variables[[independent.var]])){design$variables[[independent.var]]<-droplevels(design$variables[[independent.var]])}
  if(is.factor(design$variables[[dependent.var]])){design$variables[[dependent.var]]<-droplevels(design$variables[[dependent.var]])}
  chisq<-tryCatch(
  {svychisq (formula(formula_string), design, na.rm = TRUE)
  },
  error=function(e){
    .write_to_log(paste0("FAILED: Chi squared test.  Error:\n",e,"\n"))
    .write_to_log(paste0("independent.var:",independent.var,"- dependent.var:",dependent.var,"\n raw frequency table:"))
    .write_to_log(kable(table(design$variables[,c(dependent.var,independent.var)])))
    return(NULL)}
  
  )
  results<-list()
  results$result <- list(F=chisq$statistic, p.value=chisq$p.value %>% unname)
  results$parameters <- chisq$parameter %>% as.list
  results$name<-chisq$method
  return(results)
}

######## ONE SAMPLE Z tEST
# hypothesis_test_one_sample_z_num <- function(data.dependent.var, crit, design, data = data) {
# doesn't seem right.. parameter 'crit' not used.
#   svyttest(data[[dependentvar]]~data[[independent.var]], design = design, family = quasibinomial())
# }


hypothesis_test_empty <- function(dependent.var = NULL,
                                       independent.var = NULL,
                                       design = NULL, ...){
  results<-list()
  results$result <- c()
  results$parameters <- c()
  results$name<-"No Hypothesis test"
  return(results)
}


hypothesis_test_t_two_sample <- function(dependent.var,
                                       independent.var,
                                       design){


  as.numeric_factors_from_names<-function(x){
    if(is.factor((x))){x<-as.character(x)}  
    as.numeric(x)  
  }
  design$variables[[dependent.var]] <- as.numeric_factors_from_names(design$variables[[dependent.var]])
  if(is.factor(design$variables[[independent.var]])){
    design$variables[[independent.var]]<-droplevels(design$variables[[independent.var]])
  }
    
  

  independent_more_than_1 <- length(unique(design$variables[[independent.var]])) > 1
      if(!independent_more_than_1){
        results <- list()}else{
  formula_string<-paste0(dependent.var, "~", independent.var)
  ttest <- svyttest(formula(formula_string), design, na.rm = TRUE)
  results<-list()
  results$result <- list(t=unname(ttest$statistic), p.value = ttest$p.value %>% unname)
  results$parameters <- as.list(ttest$parameter)
  results$name<-"two sample ttest on difference in means (two sided)"}
  return(results)

  ttest$statistic

}


hypothesis_test_logistic_regression <- function(dependent.var, 
                                                independent.var, 
                                                design){
  dependent_more_than_1 <- length(unique(design$variables[[dependent.var]])) > 1
  if(!dependent_more_than_1){
    sanitised <-sanitise_data(data,dependent.var,independent.var,case = case)
    results <- list()}else{
      formula_string <- paste0(dependent.var,"~", independent.var, sep = "")
      test <- svyglm(as.formula(formula_string), design, family=quasibinomial)
      summary <- summary(test)
      results <- list()
      results$result <- list(coefficients = unname(summary$coefficients), decision = summary$effects)
    }
}

hypothesis_test_z <- function(dependent.var,
                               independent.var,
                               design){
  # .....


  results<-list()
  results$result <- c()
  results$parameters <- c()
  results$name<-"Z test (not implemented)"
  return(results)
}


hypothesis_test_linear_regression <- function(independent.var = independent.var,
                                              dependent.var = data.dependent.var,
                                              design){
  
  
  # .....
  
  
  results<-list()
  results$result <- c()
  results$parameters <- c()
  results$name<-"linear regression (not implemented)"
  return(results)
  return(results)
}




hypothesis_test_chisquared_select_multiple <- function(dependent.var,
                                                       independent.var,
                                                       design){
  
  # .write_to_log(paste0("chisquared testing:\n",
  #                      "dependent.var: ", dependent.var,"\n",
  #                      "independent.var:",independent.var,"\n\n"
  #               ))
  multiple_dependents<-names(data)[choices_for_select_multiple(dependent.var,design$variables)]
  multiple_results<-lapply(multiple_dependents,function(dependent.var){
    formula_string<-paste0("~",independent.var, "+", dependent.var)
    chisq<-tryCatch(
      {svychisq (formula(formula_string), design, na.rm = TRUE)
      },
      error=function(e){
        .write_to_log(paste0("FAILED: Chi squared test.  Error:\n",e,"\n"))
        .write_to_log(paste0("independent.var:",independent.var,"- dependent.var:",dependent.var,"\n raw frequency table:"))
        .write_to_log(kable(table(design$variables[,c(dependent.var,independent.var)])))
        return(NULL)}
      
    )
  })
  `%pull%`<-function(list,item){lapply(list,function(x){x[[item]]})}
  successful<-sapply(multiple_results,function(x){!is.null(x)})
  p.values<-(multiple_results %pull% "p.value") %>% unlist %>% .[successful]
  choicesnames<-multiple_dependents %>% gsub(paste0(dependent.var,"."),"",.) %>% .[successful]
  names(p.values)<-choicesnames %>% unlist 
  fstats<-multiple_results %pull% "statistic" %>% unlist %>% .[successful]
  names(fstats)<-choicesnames
  method<-multiple_results %pull% "method" %>% unlist %>% .[successful]
  names(method)<-choicesnames %>% unlist 
  results<-list()
  results$result <- list(F=fstats, p.value=p.values)
  results$parameters <- multiple_results %pull% "parameter" %>% .[successful] %>% do.call(rbind,.)
  
  results$name<-method
  results <- results %>% data.frame %>% (function(x){names(x)<-gsub("^result\\.","",names(x));x})
  return(results)
}
