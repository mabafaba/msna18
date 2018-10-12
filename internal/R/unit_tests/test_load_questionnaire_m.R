is_in_test_dir<-(getwd() %>% gsub("\\/$","",.) %>% strsplit("/") %>% .[[1]] %>% last)=="unit_tests"
if(!is_in_test_dir){setwd("./internal/R/unit_tests/")}


test_that("load_questionnaire: errors",{
  example1<-load.example("example1")
  
  expect_error(load_questionnaire())
  
  good_parameters<-list(
                        example.data.path("example1"),
                        questions.file = paste0(example1$path,"kobo questions.csv"),
                        choices.file = paste0(example1$path,"kobo choices.csv"),
                        choices.label.column.to.use = example1$choice.label.column.to.use)
  
  
  bad_parameters<-rep(list(good_parameters),10)
  bad_parameters[[1]]$questions.file<-"not_a_file"
  bad_parameters[[2]]$questions.file<-NA
  bad_parameters[[3]]$choices.file<-"not_a_file"
  bad_parameters[[4]]$choices.file<-NA
  bad_parameters[[5]]$choices.label.column.to.use<-NA
  
  expect_error(do.call(load_questionnaire,c(bad_paramters[[1]],data=list(example1$data))))
  expect_error(do.call(load_questionnaire,c(bad_paramters[[1]],data=list(example1$data))))
  expect_error(do.call(load_questionnaire,c(bad_paramters[[1]],data=list(example1$data))))
  expect_error(do.call(load_questionnaire,c(bad_paramters[[1]],data=list(example1$data))))
  expect_error(do.call(load_questionnaire,c(bad_paramters[[5]],data=list(example1$data))))
  expect_error(do.call(load_questionnaire,c(good_paramters[[5]],data=NA)))
  expect_error(do.call(load_questionnaire,c(good_paramters[[5]],data=NULL)))
  expect_error(do.call(load_questionnaire,c(good_paramters[[5]],data=c(1:100))))
  
    })

# question_is_skipped
# question_is_categorical
# question_is_numeric
# question_is_select_one

# question_is_select_multiple
# question_get_choice_labels
# question_get_question_label
# question_is_sm_choice
# is_questionnaire_loaded
# question_in_questionnaire
# question_is_skipped

test_that("question_variable_type: errors",{
  example1<-load.example("example1")
  good_parameters<-list(
    example.data.path("example1"),
    questions.file = paste0(example1$path,"kobo questions.csv"),
    choices.file = paste0(example1$path,"kobo choices.csv"),
    choices.label.column.to.use = example1$choice.label.column.to.use)
  
  question_
  
  })

