# check all files are there:
verify_excel_input()

# data 
message(silver("loading and preparing data.."))
data<-read.csv("./internal/input_files/data.csv",stringsAsFactors = F) %>% to_alphanumeric_lowercase_colnames_df

missing_data_to_NA<-function(data){
  lapply(data,function(x){
    replace(x,which(x %in% c("","N/A","#N/A","NA", " ")),NA)    
  }) %>% as.data.frame(stringsAsFactors=F)# survey needs with factors.
}


remove.empty.rows<-function(df){
  rowempty<-apply(df,1,function(x){
    all(x %in% c(NA,"", "N/A","#N/A","NA"))
  })
  df[!rowempty,]  
}

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
  return(data_level %>% as.data.frame)
}


## Loading cluster sampling units

# data parameters
message(silver("loading and preparing parameters.."))

data_parameters<-read.csv("./internal/input_files/parameters.csv",stringsAsFactors = F) 

cluster_deff<-any(!(data_parameters$cluster.variables %in% c(NA,"NA",""," ")))
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



# remove records with NA in cluster id



message(silver("loading and preparing sampling frames.."))

# make the weighting functions based on strata sampling frame and cluster sampling frame
is.stratified<-function(){any(grep("stratified", data_parameters$sampling.strategy[1])>0)}
is.clustered<-function(){any(grep("cluster", data_parameters$sampling.strategy[1])>0)}
# load strata weighting function
if(is.stratified()){
  stratification_sf<-excel_csv_inputs_sampling_frame_stratification_to_weighting_function()
  stratfication_weighting<-stratification_sf$weights_of
  data<-data_sanitation_remove_not_in_samplingframe(data,stratification_sf,"for_stratification")
}
# load cluster weighting function
if(is.clustered()){
  cluster_sf<-excel_csv_inputs_sampling_frame_cluster_to_weighting_function()
  cluster_weighting<-cluster_sf$weights_of
  data<-data_sanitation_remove_not_in_samplingframe(data,cluster_sf,"for_strata")
  cluster_weighting(data)  
}

# select one of them, or combine them if both exist:
if(is.stratified() & !is.clustered()){weights_of<-stratfication_weighting


}
if(!is.stratified() & is.clustered()){weights_of<-cluster_weighting}

if(is.stratified() & is.clustered()){
  # BLIND CODE
  # KNOWN ISSUE
  # must be nested / 
  weights_of<-combine_weighting_functions(stratfication_weighting,cluster_weighting)
}

if(!is.stratified() & !is.clustered()){
  weights_of<-function(df){return(rep(1,nrow(df)))}
}


###################  


message(silver("loading and preparing questionnaire.."))

# load questionnaire and create associated functions:
questionnaire<-load_questionnaire(data,questions.file = "./internal/input_files/kobo questions.csv",
                                  choices.file = "./internal/input_files/kobo choices.csv",
                                  choices.label.column.to.use = data_parameters$questionnaire.choices.label.column[1])
# load cluster ids and create associated functions:

# cleaning and getting the factors out 
data <- levels_for_cat(data, questionnaire)
message(silver("loading and preparing analysis plan.."))

# load analysis definitions
analysis_plan_user<-read.csv("./internal/input_files/analysis plan.csv",stringsAsFactors = F)
analysis_plan_user[,c("repeat.for","disaggregate.by","variable")]<-analysis_plan_user[,c("repeat.for", "disaggregate.by", "variable")] %>% lapply(to_alphanumeric_lowercase) %>% as.data.frame(stringsAsFactors=F)

