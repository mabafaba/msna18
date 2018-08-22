
hypothesis_test_chisquared <- function(dependent.var,
                                  independent.var,
                                  design){


  # .write_to_log(paste0("chisquared testing:\n",
  #                      "dependent.var: ", dependent.var,"\n",
  #                      "independent.var:",independent.var,"\n\n"
  #               ))
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
  results<-list()
  results$results <- list(F=chisq$statistic, p.value=chisq$p.value %>% unname)
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
  formula_string<-paste0(dependent.var, "~", independent.var)
  ttest <- svyttest(formula(formula_string), design, na.rm = TRUE)
  results<-list()
  results$result <- list(t=unname(ttest$statistic), p.value = ttest$p.value %>% unname)
  results$parameters <- as.list(ttest$parameter)
  results$name<-"two sample ttest on difference in means (two sided)"
  return(results)

  ttest$statistic

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
