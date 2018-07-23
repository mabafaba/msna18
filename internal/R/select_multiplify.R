### function that returns the names of select multiple questions and their indices in the data
### needs the questionnaire to be loaded
multiples_in_questionnaire <- function(data){
  select_mul <- lapply(questionnaire$questions$name, question_is_select_multiple) %>% unlist %>% which
  select_mul_names <- questionnaire$questions$name[select_mul]
  return(list(select_mul, select_mul_names))
}


###function that returns the indices in the data of the choices for each select multiple question
### needs the questionnaire to be loaded
choices_for_select_multiple <- function(question_name, data){
  choices<-questionnaire$choices_per_variable[[question_name]]
  select_mult_colnames<-paste(question_name,choices$name,sep=".") %>% to_alphanumeric_lowercase
  indices<-match(select_mult_colnames,names(data))
  if(any(is.na(indices))){stop(paste("can not find TRUE/FALSE columns for variable",question_name,". Please double check that they exist in the data and that their names are in the standard kobo format of \"[question name].[choice name]\""))}
}

# qs_dirt$type
# begin_gr <- grep(paste(c("begin_group","begin group"), collapse = "|"), qs_dirt$type, ignore.case = T)
# end_gr <- grep(paste(c("end_group","end group"), collapse = "|"), qs_dirt$type, ignore.case = T)
# reachR:::insure.same.length(begin_gr, end_gr)
# groups <- cbind(begin_gr, end_gr)

# #####
# multiples <- data.frame(s_mul, s_mul_names)
# multiples_indices_in_data <- which(reachR:::to_alphanumeric_lowercase(names(data)) %in% s_mul_names)

