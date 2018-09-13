# ### function that returns the names of select multiple questions and their indices in the data
# ### needs the questionnaire to be loaded
# multiples_in_questionnaire <- function(data){
#   select_mul <- lapply(questionnaire$questions$name, question_is_select_multiple) %>% unlist %>% which
#   select_mul_names <- questionnaire$questions$name[select_mul]
#   return(list(select_mul, select_mul_names))
# }


###function that returns the indices in the data of the choices for each select multiple question
### needs the questionnaire to be loaded
choices_for_select_multiple <- function(question_name, data){
  question_name<-as.character(question_name)
  choices<-questionnaire$choices_per_variable[[question_name]]
  select_mult_colnames<-paste(question_name,choices$name,sep=".") %>% to_alphanumeric_lowercase
  indices<-match(as.character(select_mult_colnames),names(data))
  if(all(is.na(indices))){stop(paste("can not find TRUE/FALSE columns for variable",question_name,". Please double check that they exist in the data and that their names are in the standard kobo format of \"[question name].[choice name]\""))}
  if(any(is.na(indices))){warning(paste("could not find TRUE/FALSE columns for variable", question_name, ". Some choices will be discarded. 
                                       Please double check that they exist in the data and that their names are in the standard 
                                       kobo format of \"[question name].[choice name]\""))}
  indices <- na.rm(indices)
return(indices)
  }
