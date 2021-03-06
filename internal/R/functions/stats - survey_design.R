######## BLIND CODE

combine_weighting_functions<-function(weights_function_1,weights_function_2){
  normweights<-function(w){w*length(w)/sum(w)}
  normW2byW1<-function(w1,w2){
   
    
    w1_proportions_is<-(w2 %>% split(names(w1)) %>% sapply(sum)) %>% normweights() 
    w1_proportions_should<-w1 %>% split(names(w1)) %>% sapply(sum) %>% normweights
    w1_proportions_should<-w1_proportions_should[match(names(w1_proportions_is),names(w1_proportions_should))]
    w1_proportion_factor<-w1_proportions_should/w1_proportions_is
    
    w_combined<-w2*w1_proportion_factor[names(w1)]
    w_combined<-normweights(w_combined)
    
    w_combined
  }
  
  combined_fun<-function(df){
    
    w1 <- weights_function_1(df)
    w2 <- weights_function_2(df)
    w_combined<-normW2byW1(w1,w2) 
    
    return(w_combined)
  }
  
  return(combined_fun)
}






#' creates a weighting function from a sampling frame
#'
#' @param sampling.frame.file data frame containing the sampling frame. should contain columns "stratum" and "population", otherwise column names must be specified.
#' @param sampling.frame.population.column sampling frame name of column holding population counts. defaults to "population"
#' @param sampling.frame.stratum.column sampling frame name of column holding stratum names. defaults to "stratum". Stratum names must match exactly values in:
#' @param data.stratum.column data column name that holds the record's strata names
#' @param return.stratum.populations  by default this function returns `NULL`, but must be called to make automatic weighting in ..._weighted() functions possible. you can however retreive the stratum population counts by setting this to `TRUE`.
#' @seealso \code{\link{load_data} (not implemented), \link{aggregate_count_weighted}}
#' @return returns a function that calculates weights based on here provided sampling frame
#' @export
#' @examples
#' load_data("mydata.csv",uuid.column="UUID")
weighting_fun_from_samplingframe <- function(sampling.frame,
                               data.stratum.column,
                               sampling.frame.population.column="population",
                               sampling.frame.stratum.column="stratum",data=NULL){
  
  # this function was originally designed for stratification weights only, hence the variable names.
  # just imagine you're doing a stratification, and BAAAMM you will feel normal again
  
  
  
  # functions from previous external dependencies. In here to not pollute global space
        hasdata<-function (x, return.index = F) {
          index <- which(!is.null(x) & !is.na(x) & x != "" & !is.infinite(x))
          value <- x[which(!is.null(x) & !is.na(x) & x != "" & !is.infinite(x))]
          if (return.index) {
            return(index)
          }
          return(value)
        }
        
        
        
        
        
        
        stratify.count.sample<-function(data.strata,sf.strata){
          # count samples per stratum
          samplecounts<-table(data.strata)
          # check which ones can be found in sampling frame
          strataexists<-(names((samplecounts)) %in% names(sf.strata))
          data.strata.not.in.sampleframe<-samplecounts[!strataexists]
          # throw error if data strata not found in sampling frame
          
          if(length(data.strata.not.in.sampleframe)!=0){
            stop(paste0("problem with ", cat(bgYellow(red(" sampling frame "))),
                        "in the data, we find the following strata names that don't exist in the sampling frame, or have no valid population numbers:\n",
                        cyan(paste0(names(data.strata.not.in.sampleframe),collapse="\n")))
            )
                 }
          # return sample counts
          return(samplecounts[strataexists])
        }
        
        x<-c(a="1",b="2")
        stratify.weights<-function(pop_strata,sample_strata){
          
          
          # remove sample_strata names with no data (can happen when only a subset of the data is used)
          sample_strata %>% hasdata -> sample_strata
          # only use populations that appear in current sample:
          pop_strata<-pop_strata[names(sample_strata)]
          
          # insure that all names of sample strata are found in names of population strata
          if(!all(names(sample_strata)%in%names(pop_strata))){stop(paste(
            "all data strata must exist in sampling frame. The following could not be found:\n",
            paste(names(sample_strata)[names(sample_strata)%in%names(pop_strata)],collapse="\n")))}
          sample_global<-sum(sample_strata)
          pop_global<-sum(pop_strata)
          weights = (pop_strata/pop_global) / (sample_strata/sample_global)
          return(weights)
          
        }
  
  
  
  
  
  
  
  
  
  
  
  # check input

  # load file:
  sf_raw<-sampling.frame
  if(any(duplicated(sf_raw[, sampling.frame.stratum.column]))){
    sf_raw<-sf_raw[!duplicated(sf_raw[sampling.frame.stratum.column]),]
    .write_to_log(paste("SERIOUS ISSUE: duplicate stratum names in the sampling frame:\n",
                        cyan(paste0(names(sf_raw[duplicated(sf_raw[sampling.frame.stratum.column]),sampling.frame.stratum.column] %>% table),collapse="\n"))))
    warning("DUPLICATE STRATUM NAMES IN SAMPLING FRAME! results potentially invalid.")
  }
  # sf_raw<-sf
  # get unique strata names from sampling frame
  unique_strata <- sf_raw[, sampling.frame.stratum.column]
  # make sure strata are unique
  
  # standardise internal sampling frame format
  # - data.stratum.column: the name of the data column holding strata names (is function argument)
  # - population.counts: named vector with counts as values and strata names as names
  # use: population.counts[stratum_name_string]
  population.counts<-sf_raw[[sampling.frame.population.column]]
  names(population.counts)<-as.character(unique_strata)
  
  # error if any stratum has zero population
  if(any(population.counts==0,na.rm = T)){.write_to_log("CRITICAL: strata in sampling frame can not have population 0, please remove the stratum from your sampling frame and data. (how did you even sample from that)")}
  # make sure all strata have data:
  population.counts <- population.counts[(
    !is.na(population.counts) &
      !is.na(population.counts) &
      population.counts > 0)]

  
  # make sure all data has strata in samplingframe:
  if(!is.null(data)){
  is_data_in_sf<-unique(data[,data.stratum.column]) %in% sf_raw[,sampling.frame.stratum.column]
  if(any(!(is_data_in_sf))){
    warning(paste0("there are records that can not be found in the sampling frame:\n",
                   cyan(paste0(data[is_data_in_sf,data.stratum.column] %>% unique,collapse="\n"))))
  }
  }
  
    
  # closure function that calculates weights on the fly
  # uses immutable data provided to load_samplingframe()
  weights_of<- function(df) {
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
    if("weights" %in% names(df)){warning("'weights' is used as a column name (will not be calculated from the sampling frame)")}
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

  return(weights_of)
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




