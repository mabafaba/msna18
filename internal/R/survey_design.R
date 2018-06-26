#' Map to Design
#'
#' creates a `survey` design object from the data
#'
#' @param data
#' @param cluster.var if cluster sampling was used, what's the name of the column in `data` that identifies the cluster?
#' @param strata.var if cluster sampling was used, what's the name of the column in `data` that identifies the cluster?

#' @details create a `survey` package design object from the data and information on the sampling strategy
#' @return a `survey` package design object
#' @examples map_to_design(data,cluster.var="cluster_id")
#' @export
map_to_design <- function(data,
                          cluster.var = NULL,strata.var) {
  if(is.null(cluster.var)){
    cluster.ids <- as.formula(c("~1"))}else{
      cluster.ids <- cluster.var}
  strata.weights <- reachR:::weights_of(data)
  survey.design <- svydesign(data = data,
                             ids = formula(cluster.ids),
                             strata = names(strata.weights),
                             weights = as.vector(strata.weights))
  return(survey.design)
  }



#' Loads the sampling frame from a csv file, performing inital tests on the data.
#'
#' @param sampling.frame.file data frame containing the sampling frame. should contain columns "stratum" and "population", otherwise column names must be specified.
#' @param sampling.frame.population.column sampling frame name of column holding population counts. defaults to "population"
#' @param sampling.frame.stratum.column sampling frame name of column holding stratum names. defaults to "stratum". Stratum names must match exactly values in:
#' @param data.stratum.column data column name that holds the record's strata names
#' @param return.stratum.populations  by default this function returns `NULL`, but must be called to make automatic weighting in ..._weighted() functions possible. you can however retreive the stratum population counts by setting this to `TRUE`.
#' @seealso \code{\link{load_data} (not implemented), \link{aggregate_count_weighted}}
#' @return - makes function `weights_of()` usable, and with it .._weighted() functions with automatic weighting
#' `NULL` if `return.stratum.populations` is `FALSE` (default)
#' A named vector with the population counts per stratum, if `return.stratum.populations` is set to `TRUE`
#' @export
#' @examples
#' load_data("mydata.csv",uuid.column="UUID")
load_samplingframe <- function(sampling.frame.file,
                               sampling.frame.population.column="population",
                               sampling.frame.stratum.column="stratum",
                               data.stratum.column="overview.camp_name",
                               return.stratum.populations=FALSE){
  
  # check input
  # insure.is.single.value(sampling.frame.file)
  # insure.is.single.value(sampling.frame.population.column)
  # insure.is.single.value(sampling.frame.stratum.column)
  # insure.is.single.value(data.stratum.column)
  
  # load file:
  sf_raw<-read.csv(sampling.frame.file,stringsAsFactors = F, header = T)

  # sf_raw<-sf
  # get unique strata names from sampling frame
  unique_strata <- sf_raw[, sampling.frame.stratum.column]
  # make sure strata are unique
  if(any((unique_strata %>% hasdata %>% table)>1)){stop("duplicate stratum names in the sampling frame")}
  
  # standardise internal sampling frame format
  # - data.stratum.column: the name of the data column holding strata names (is function argument)
  # - population.counts: named vector with counts as values and strata names as names
  # use: population.counts[stratum_name_string]
  population.counts<-sf_raw[[sampling.frame.population.column]]
  names(population.counts)<-as.character(unique_strata)
  
  # error if any stratum has zero population
  if(any(population.counts==0)){stop("strata in sampling frame can not have population 0, please remove the stratum from your sampling frame and data. (how did you even sample from that)")}
  # make sure all strata have data:
  population.counts <- population.counts[(
    !is.na(population.counts) &
      !is.na(population.counts) &
      population.counts > 0)]
  
  # closure function that calculates weights on the fly
  # uses immutable data provided to load_samplingframe()
  weights_of<<- function(df) {
    # # insure stratum column exists in df:
    # insure.string.is.column.header(df,data.stratum.column)
    # # insure only one data.stratum.column is given:
    # insure.is.single.value(data.stratum.column)
    # make sure df is handled as characters, not factors. otherwise we match factor id's instead of names
    df[[data.stratum.column]]<-as.character(df[[data.stratum.column]])
    df <- df[!is.na(data.stratum.column),]
    df <- df[!(df[[data.stratum.column]] %in% c("NA", "N/A", "#N/A")),]
    
    # count number of records in each stratum
    sample.counts<-stratify.count.sample(data.strata = df[[data.stratum.column]],sf.strata = population.counts)
    
    # make sure all record's strata can be found in the sampling frame:
    if("weights" %in% names(df)){stop("'weights' is not allowed as a column name (will be calculated from the sampling frame)")}
    if(!all(names(sample.counts) %in% names(population.counts))){stop("all strata names in column '",
                                                                      data.stratum.column,"' must also appear in the loaded sampling frame.")}
    # population counts taken from weights_of() enclosing environment, created in load_samplingframe()
    weights <- stratify.weights(pop_strata = population.counts,
                                sample_strata = sample.counts)
    
    # final test that mean of weights == 1
    # insure(that.all=mean(weights[df[[data.stratum.column]]]) %almost.equals% 1,
    #        err="Weighting calculation failed internally, this is our fault. Sorry! Contact the Reach Initiatives data unit to get this fixed!")
    return(weights[df[[data.stratum.column]]])
    
    
  }
  message(
    "Sampling frame loaded. you can now use .._weighted() functions with automatic weighting"
  )
  if(return.stratum.populations){return(population.counts)}
}


weights_of <- function(df) {
  stop("Before weights_of() can be used, load_samplingframe() must be run.")
}


auto.weight<-function(df,weight.by=NULL){
  # A) if no weight.by provided, I'll try calculating myself:
  if(is.null(weight.by)){temp.weights<-weights_of(df)}
  # B) character string provided
  else if(is.character(weight.by)){
    warning("using the weight.by argument is not recommended. You should call load_samplingframe(), and then weighted functions without weight.by argument.")
    insure.string.is.column.header(df,weight.by)
    insure(length(weight.by)==1,err="weight by should be NULL for auto weighting, or a single character string naming the data column that stores the weights")
    df[[weight.by]] <- gsub(",", ".", df[[weight.by]]) %>% as.numeric
    temp.weights<-df[[weight.by]]
  }else{
    insure.has.data(temp.weights)
  }
  return(temp.weights)
}


hasdata<-function (x, return.index = F) {
  index <- which(!is.null(x) & !is.na(x) & x != "" & !is.infinite(x))
  value <- x[which(!is.null(x) & !is.na(x) & x != "" & !is.infinite(x))]
  if (return.index) {
    return(index)
  }
  return(value)
}
