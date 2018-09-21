
#' Complete  analysis for one hypothesis
#'
#' Produce summary statistics, hypothesis tests and plot objects for a hypothesis
#'
#' @param data dataframe with all the data; created from `130 - load_input.R`
#' @param dependent.var string with the column name in `data` of the dependent variable
#' @param independent.var string with the column name in `data` of the independent variable
#' @param hypothesis.type the type of hypothesis as a string. Allowed values are "direct_reporting", "group_difference",
#'  "limit", "correlation" or "change"
#' @param case a string to support the mapping functions. To list valid case strings use \link{\code{list_all_cases}}
#' @details this function takes the data, information about your variables of interest, hypothesis type. It selects 
#' the appropriate summary statistics, hypothesis test and visualisation and applies them.
#' it uses \code{\link{map_to_case}}, \code{\link{map_to_design}}, \code{\link{map_to_summary_statistic}},
#' \code{\link{map_to_hypothesis_test}} and \code{\link{map_to_visualisation}}.
#' The function does not take any `design` as parameter, it will create the `design` object itself.
#' @return A list of 5 items with:
#' 1. `input.parameters`, a list with the input parameters, 
#' 2. `summary.statistic`, a dataframe with the summary.statistic, 
#' 3. `hypothesis.test`, a list with the hypothesis.test.result,
#' 4. `visualisation`, a ggplot object with the visualisation and
#' 5. `message`, a string to say whether the analysis was successful or not
#' @examples
#' #### USING SOM_MSNA_input
#' analyse_indicator(data, "health_pay", "x7states", "group_difference")
#' analyse_indicator(data, "health_pay", "x7states", "group_difference", "CASE_group_difference_categorical_categorical")
#' @export

analyse_indicator <- function(data,
                            dependent.var,
                            independent.var = NULL,
                            hypothesis.type,
                            case=NULL){
  
  options(survey.lonely.psu = "average")
  
  
  input.parameters= list(
    dependent.var=dependent.var,
    independent.var=ifelse(is.null(independent.var),NA,independent.var),
    hypothesis.type=hypothesis.type,
    case=case
  )
  # sanitise input
  # if(!is.null(do.for.each.unique.value.in.var)){stop("do.for.each.unique.value.in.var must be NULL (not yet implemented)")}
  # data <- data[!is.na(data[,dependent.var]),]
  # if(nrow(data)==0){stop('provided data has no rows where dependent.var is not NA')}
  # if(all(is.na(data[,dependent.var]))){stop(paste('variable', dependent.var, 'can\'t be all NA'))}
  
  # map from input to analysis case:
  if(is.null(case)){
    case <- map_to_case(hypothesis.type = hypothesis.type,
                        data = data,
                        dependent.var = dependent.var,
                        independent.var = independent.var,
                        paired = NULL)
  }else{
    if(!is_valid_case_string(case)){
      stop(paste(case,"is not a valid case string. List of valid cases:\n",paste(list_all_cases(T),collapse = "\n"),
                 "value for argument 'case' is not a valid case string. It must be of the form 
                 CASE_[hypothesis_type]_[dependent.variable.type]_[independent.variable.type]\n
                 for example 'CASE_group_difference_categorical_categorical'\nAlternatively, 
                 you can leave out that parameter, and we will try to identify the case automagically 
                 from the questionnaire"))
    }
  }
  
  
  data_sanitised <- apply_data_sanitations(data,dependent.var,independent.var)
  
  
  if(data_sanitised$success) {
    data <- data_sanitised$data
  } else {
    return(
      empty_result(input.parameters,data_sanitised$message)
      
    )
  }
  

  # map from case to appropriate summary statistic, hypothesis test and visualisation:
  design <- map_to_design(data = data, cluster.var = NULL, weights = NULL) 
  
  summarise.result<- map_to_summary_statistic(case)

  test.hypothesis <- map_to_hypothesis_test(case)
  visualisation <- map_to_visualisation(case)
  
  # apply the summary statistic, hypothesis test to the given data and survey design:
  summary.result  <- summarise.result(dependent.var = dependent.var,independent.var, design = design, data = data)
  # do hypothesis test:
  
  hypothesis.test.result<- test.hypothesis(dependent.var,independent.var, design)

  # add results to the visualisation:
  # visualisation<-visualisation+ggplot()...
  return(list(
    input.parameters= list(
                      dependent.var=dependent.var,
                      independent.var=independent.var,
                      hypothesis.type=hypothesis.type,
                      case=case
                      
                    ),
                    summary.statistic = summary.result,
                    hypothesis.test = hypothesis.test.result,
                    visualisation = visualisation,
                    message = "success (or unidentified issue)"
              ))

}

empty_result<-function(input.parameters,message){
  
  empty_summary_stat<-matrix(0, ncol = 8, nrow = 0) %>% as.data.frame
  colnames(empty_summary_stat)<-c("dependent.var","independent.var","dependent.var.value", "independent.var.value","numbers","se","min", "max")
  .write_to_log("data sanitation decided not to produce summary statistics. Message:")
  .write_to_log(message)
  return(
    list(
      input.parameters=input.parameters,
      summary.statistic=empty_summary_stat,
      hypothesis.test.result=NULL,
      visualisation=NULL,
      message=message
    )
    
  )
}
