# data 
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
      data[[x]] %<>% factor(., levels = questionnaire$choices_per_variable[x] %>% as.data.frame %>% extract2(2) %>% unique)}
    return(data[[x]])}) 
  names(data_level) <- names(data)
  return(data_level %>% as.data.frame)
}

## Loading cluster sampling units

# data parameters
data_parameters<-read.csv("./internal/input_files/parameters.csv",stringsAsFactors = F) 
data_parameters$stratum.name.variable <- data_parameters$strata.name.variable[1] %>% to_alphanumeric_lowercase
cluster_formula <- if(any(grep("cluster", data_parameters$sampling.strategy)>0)){
                            load_cluster_sampling_units(cluster.variable = data_parameters$cluster.variables %>% to_alphanumeric_lowercase)
                   }else{
                            load_cluster_sampling_units(cluster.variable = NULL)
                        }

cluster.id.formula<-cluster_formula()

# load samplingframe (only if data_parameters says it's a stratified sample)

if(any(grep("stratified", data_parameters$sampling.strategy[1])>0)){
  rawsf<-read.csv("./internal/input_files/stratification_samplingframe.csv",stringsAsFactors = F) %>% remove.empty.rows
  data_parameters$stratification.variable<-rawsf[1,"first.strata.name.variable"] %>% as.character
  rawsf_readable<-rawsf[-1,c("first.strata.name.variable","population")]
  colnames(rawsf_readable)<-c("stratum","population")
  # data[["unique_stratum_group_name"]]<-paste()
  if(is.na(data_parameters$stratum.name.variable[1])){stop("if the input sheets \"sampling strategy\" contains \"stratified\"", "then you also must fill a value for \"strata name variable\" (see input sheet xlsm)")}
  
  write.csv(rawsf_readable,"./internal/input_files/stratification_samplingframe_generated.csv")
  sf<-load_samplingframe("./internal/input_files/stratification_samplingframe_generated.csv",
                         data.stratum.column = data_parameters$stratum.name.variable[1],
                         return.stratum.populations = T)
  }
# load questionnaire and create associated functions:
questionnaire<-load_questionnaire(data,questions.file = "./internal/input_files/kobo questions.csv",
                                  choices.file = "./internal/input_files/kobo choices.csv",
                                  choices.label.column.to.use = data_parameters$questionnaire.choices.label.column[1])
# load cluster ids and create associated functions:

# cleaning and getting the factors out 
data <- levels_for_cat(data, questionnaire)

# load analysis definitions
analysis_plan_user<-read.csv("./internal/input_files/analysis plan.csv",stringsAsFactors = F)
analysis_plan_user[,c("repeat.for","disaggregate.by","variable")]<-analysis_plan_user[,c("repeat.for","disaggregate.by","variable")] %>% lapply(to_alphanumeric_lowercase) %>% as.data.frame(stringsAsFactors=F)
  
