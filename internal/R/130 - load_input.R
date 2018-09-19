# This part of script will load all the inputs from the Excel file in the input folder.
# The objects of interests are:
# - data, the data tab from the excel file
# - data_parameters, the parameters tab from the excel file
# - stratification_sf, a list to calculate the weight of the stratification strategy, 
#   including the stratification_samplingframe tab from the excel file
# - cluster_sf, a list to calculate the weight of the clusterting strategy, 
#   including the cluster_samplingframe tab from the excel file
# - questionnaire, a BIG list that contains kobo questions tab, as well as the kobo choices from the excel file
# - analysis_plan, the analysis plan from the excel file


# check all files are there:
verify_excel_input()

# Load data from the input file
logmessage(silver("loading and preparing data.."))
data<-read.csv("./internal/input_files/data.csv",stringsAsFactors = F) %>% to_alphanumeric_lowercase_colnames_df

# Function to change missing data to be NA
missing_data_to_NA<-function(data){
  lapply(data,function(x){
    replace(x,which(x %in% c("","N/A","#N/A","NA", " ")),NA)    
  }) %>% as.data.frame(stringsAsFactors=F)# survey needs with factors.
}

# Function to remove empty rows
remove.empty.rows<-function(df){
  rowempty<-apply(df,1,function(x){
    all(x %in% c(NA,"", "N/A","#N/A","NA"))
  })
  df[!rowempty,]  
}

# Corrects the NA and remove the empty rows in data
data <- data %>% missing_data_to_NA %>% remove.empty.rows

#function that recodes categorical variables using the levels provided in the choices file
#also converts missing data to NA without messing up the factors 
levels_for_cat <- function(data, questionnaire){
  #questionnaire must be loaded
  data_level <-  lapply(names(data), function(x){
    replace(data[[x]],which(data[[x]] %in% c("","N/A","#N/A","NA", " ")),NA)
    if(question_is_categorical(x)){
      levels_questionnaire<-questionnaire$choices_per_variable[x] %>% as.data.frame %>% extract2(2) %>% unique
      levels_data<-unique(data[[x]])
      levels_all<-c(levels_questionnaire,levels_data) %>% unique
      data[[x]] %<>% factor(., levels = levels_all)
    }
    return(data[[x]])}) 
  names(data_level) <- names(data)
  return(data_level %>% as.data.frame)}
  
# Loads cluster sampling units
## data parameters
logmessage(silver("loading and preparing parameters.."))

## Reads data parameters from the input files
data_parameters<-read.csv("./internal/input_files/parameters.csv",stringsAsFactors = F) 

## remove any empty values from the specified cluster variables
cluster_deff<-any(!(data_parameters$cluster.variables %in% c(NA,"NA",""," ")))

## Creates the cluster formula
### removes any records with NA in cluster id
cluster_formula <- if(cluster_deff){
                            cluster_formula<-load_cluster_sampling_units(cluster.variable = data_parameters$cluster.variables %>% to_alphanumeric_lowercase)
                            cluster.id.formula<-cluster_formula()
                            rows_with_valid_clusterids<-data[,all.vars(formula(cluster.id.formula)),drop=F] %>% apply(2,is.na) %>% apply(1,function(x){!any(x)}) 
                            data<-data[rows_with_valid_clusterids,]
                            cluster_formula
                   }else{
                            cluster_formula<-suppressWarnings(load_cluster_sampling_units(cluster.variable = NULL))
                            cluster.id.formula<-cluster_formula()
                            cluster_formula
                        }




## Prepares the sampling frames

logmessage(silver("loading and preparing sampling frames.."))

# make the weighting functions based on strata sampling frame and cluster sampling frame
is.stratified<-function(){any(grep("stratified", data_parameters$sampling.strategy[1])>0)}
is.clustered<-function(){any(grep("cluster", data_parameters$sampling.strategy[1])>0)}

# load strata weighting function
## Loads strata weighting function if strata sampling exists
### Creates stratification_sf, a list of 4:
### - function: weights_of,
### - dataframe: sampling frame, that is the stratification_samplingframe from the input file
### - function: add_stratum_names_to_data,
### - a character vector: stratum variable
if(is.stratified()){
  stratification_sf<-excel_csv_inputs_sampling_frame_stratification_to_weighting_function()
  stratfication_weighting<-stratification_sf$weights_of
  # Verifies that all the data are in the sampling frame, will stop the tool with any found 
  data<-data_sanitation_remove_not_in_samplingframe(data,stratification_sf,"for_stratification")
}

# load cluster weighting function
### Creates cluster_sf, a list of 4:
### - function: weights_of,
### - dataframe: sampling frame, that is the cluster_samplingframe from the input file
### - function: add_stratum_names_to_data,
### - a character vector: stratum variable
if(is.clustered()){
  cluster_sf<-excel_csv_inputs_sampling_frame_cluster_to_weighting_function()
  cluster_weighting<-cluster_sf$weights_of
  data<-data_sanitation_remove_not_in_samplingframe(data,cluster_sf,"for_strata")
}

### Selects one of stratification or cluster methodology, or combines them if both exist, or makes 'unweighted' weighting function if none of them:
if(is.stratified() & !is.clustered()){weights_of<-stratfication_weighting


}
if(!is.stratified() & is.clustered()){weights_of<-cluster_weighting}

if(is.stratified() & is.clustered()){
  weights_of<-combine_weighting_functions(stratfication_weighting,cluster_weighting)
}

if(!is.stratified() & !is.clustered()){
  weights_of<-function(df){return(rep(1,nrow(df)))}
}




logmessage(silver("loading and preparing questionnaire.."))
# load questionnaire and create associated functions.
## Creates questionaire, a list that contains:
## - questions, the questions from kobo
## - choices, the choices from kobo
## - choices_per_variables
## - and all the columns of the dataset
questionnaire<-load_questionnaire(data,questions.file = "./internal/input_files/kobo questions.csv",
                                  choices.file = "./internal/input_files/kobo choices.csv",
                                  choices.label.column.to.use = data_parameters$questionnaire.choices.label.column[1])


# load cluster ids and create associated functions:

# add kobo tool choices to the factor levels (so we can include choices that were available but don't appear in the data)
data <- levels_for_cat(data, questionnaire)
logmessage(silver("loading and preparing analysis plan.."))

# load analysis definitions
## Loads the analysis plan from the input file
analysis_plan_user<-read.csv("./internal/input_files/analysis plan.csv",stringsAsFactors = F)
## changes variable names provided in the analysisplan to the standard format (only characters allowed are lowercase alphanumerics and underscores)
analysis_plan_user[,c("repeat.for","disaggregate.by","variable")]<-analysis_plan_user[,c("repeat.for", "disaggregate.by", "variable")] %>% lapply(to_alphanumeric_lowercase) %>% as.data.frame(stringsAsFactors=F)


# #####LIBYA
# analysis_plan_user$repeat.for %<>% gsub("_", ".", .) %>% to_alphanumeric_lowercase
# analysis_plan_user$variable %<>% gsub("_", ".", .) %>% to_alphanumeric_lowercase
# analysis_plan_user$disaggregate.by %<>% gsub("_", ".", .) %>% to_alphanumeric_lowercase

# just give weighting a shot to see if the sampling frame is complete:
test_weights<-weights_of(data);rm(test_weights)

# Specific to uganda data; must be in conjunction with changes in data formatting, questionnaire, analysisplan and composite indicator weighted counts (only Eliora knows..) 
# data <- apply(data, 2, function(x) gsub("_", ".", x)) %>% rbind.data.frame(list)

