source("./load_questionnaire.R")
source("./questionnaire_skiplogic.R")
source("./skiplogic_dependendencies.R")

require("dplyr")
require("data.table")
require("rlist")
require("crayon")
data<-read.csv.auto.sep(file = "./internal/input_files/data.csv")


# must load questionnaire first:
questionnaire<-load_questionnaire(data,questions.file = "./internal/input_files/kobo_questions.csv",
                                  choices.file = "./internal/input_files/kobo_choices.csv",
                                  choices.label.column.to.use = "label")




# check a variable:
a_variable_name<-sample(names(data),1)
question_is_skipped(data,a_variable_name) %>% table

# to do it for all variables:

skippeddf2<-lapply(names(data)[!is.na(question_variable_type(names(data)))],function(dataname){
  isk<-question_is_skipped(data,dataname)
  print(dataname)
  print(is.null(isk))
  print(table(isk))
}) %>% as.data.frame

ncol(skippeddf)
skippeddf[]
names(skippeddf)<-names(data)
