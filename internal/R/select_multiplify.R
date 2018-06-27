foo1 <- function(data, questionnaire){
  s_mul <- lapply(questionnaire$questions$name, question_is_select_multiple) %>% unlist %>% which
  s_mul_names <- questionnaire$questions$name[s_mul]
  return(s_mul, s_mul_names)
}

groups <- grep("begin_group", questionnaire[["questions"]]$type, ignore.case = T)
group_names <- questionnaire[["questions"]]$name[groups]
questions.names <- qdap::mgsub(group_names, "", questions.names, trim = T, order.pattern = T)

table(names(data) %in% qs_dirt$name)
length(qs_dirt$name)

qs_dirt$type
begin_gr <- grep(paste(c("begin_group","begin group"), collapse = "|"), qs_dirt$type, ignore.case = T)
end_gr <- grep(paste(c("end_group","end group"), collapse = "|"), qs_dirt$type, ignore.case = T)
reachR:::insure.same.length(begin_gr, end_gr)
groups <- cbind(begin_gr, end_gr)


#####
questions.names

multiples <- data.frame(s_mul, s_mul_names)
multiples_indices_in_data <- which(reachR:::to_alphanumeric_lowercase(names(data)) %in% s_mul_names)
questions.names <- reachR:::to_alphanumeric_lowercase(names(data))

#Getting associated choices columns in the data
dependents <- function(s_mul, s_mul_names, data){
  index_multiple_answers_data <- list()
  for(x in s_mul){
    y <- s_mul_names[(s_mul == x)]
    if(y %in% questions.names){
      number <- sum(questionnaire[["choices"]]$list.name == y)
      index_multiple_answers_data[[x]] <- c(as.numeric(which(questions.names == y)+1):as.numeric(which(questions.names == y)+number))
    }
  }
  return(index_multiple_answers_data)
}

debug(dependents)
dependents(s_mul, s_mul_names, data = data)

which(questions.names == y)