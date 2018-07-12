### function that returns the names of select multiple questions and their indices in the data
### needs the questionnaire to be loaded
multiples_in_questionnaire <- function(data){
  select_mul <- lapply(questionnaire$questions$name, question_is_select_multiple) %>% unlist %>% which
  select_mul_names <- questionnaire$questions$name[select_mul]
  return(list(select_mul, select_mul_names))
}

questionnaire$choices_per_variable
questionnaire$choices_per_variable[["other.water.sources"]]
question_name <- "other.water.sources"

###function that returns the indices in the data of the choices for each select multiple question
### needs the questionnaire to be loaded
choices_for_select_multiple <- function(question_name, data){
  ### cleaning the questionnaire questions of group names
  # groups <- grep("begin_group", questionnaire[["questions"]]$type, ignore.case = T)
  # group_names <- questionnaire[["questions"]]$name[groups]
  # 
  ### cleaning the questionnaire choices names and data column names
  questionnaire[["choices"]]$list.name <- reachR:::to_alphanumeric_lowercase(questionnaire[["choices"]]$list.name)
  questionnaire[["choices"]]$list.name <- gsub(".list", "", questionnaire[["choices"]]$list.name)
  
  ### calculating the indices
  index_multiple_answers_data <- c()
  if(question_name %in% names(data)){
  # mult.q <- filter(questionnaire[["questions"]], name %in% question_name) %>% select(type)
    number <- nrow(filter(questionnaire[["choices"]], list.name %in% question_name))
    
    q.num <- c(as.numeric(which(names(data) == question_name)+1):as.numeric(which(names(data) == question_name)+number))
    index_multiple_answers_data <- q.num 
  }
  return(index_multiple_answers_data)
}

# qs_dirt$type
# begin_gr <- grep(paste(c("begin_group","begin group"), collapse = "|"), qs_dirt$type, ignore.case = T)
# end_gr <- grep(paste(c("end_group","end group"), collapse = "|"), qs_dirt$type, ignore.case = T)
# reachR:::insure.same.length(begin_gr, end_gr)
# groups <- cbind(begin_gr, end_gr)

# #####
# multiples <- data.frame(s_mul, s_mul_names)
# multiples_indices_in_data <- which(reachR:::to_alphanumeric_lowercase(names(data)) %in% s_mul_names)

