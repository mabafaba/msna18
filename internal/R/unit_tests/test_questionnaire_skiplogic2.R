is_in_test_dir<-(getwd() %>% gsub("\\/$","",.) %>% strsplit("/") %>% .[[1]] %>% last)=="unit_tests"
if(!is_in_test_dir){setwd("./internal/R/unit_tests/")}

context("Skip Logic: basics")

test_that("question_is_skipped_apply_condition_to_data works",{
  # setwd("./internal/R/unit_tests/")
  example<-load.example("example1",F)
  expect_is(question_is_skipped,"function")                   
  
  attach(example$data)
  skip_examples<-c("settlement_other","selected(${settlement},\"other\")",'settlement=="other"',
                   "settlement_other","${settlement}='other'",'settlement=="other"',
                   "plw","${females_13_15}>0 or ${females_16_17}>0 or ${females_18_40}>0",'females_13_15>0 | females_16_17>0 | females_18_40>0',
                   "note_vuln_gender","${disabled_chronic}>0 or ${sick_children}>0 or ${mental}>0 or ${uasc}>0","disabled_chronic>0 | sick_children>0 | mental>0 | uasc>0",
                   "note_vuln_gender","(${disabled_chronic}>0 or ${sick_children}>0) and (${mental}>0 or (${uasc}>0))","(disabled_chronic>0 | sick_children>0) & (mental>0 | uasc>0)",
                   "uasc","${total_children}>0", "total_children>0",
                   "uasc","${total_children}+1>0+1*5*${total_children}", "(total_children+1)>0+1*5*total_children",
                   "uasc","(${total_children}+1)*2>2*(0+1*5*${total_children})+3 and (${disabled_chronic}>0)", "((total_children+1)*2>2*(0+1*5*total_children)+3)&disabled_chronic>0",
                   "uasc","(2)*(0+(1+${total_children}+${total_children})-1)/4>3 and (${disabled_chronic}>0)", "(2)*(0+(1+total_children+total_children)-1)/4>3 & (disabled_chronic>0)"
                   
  ) %>% 
    matrix(3,byrow=F) %>% t %>% as.data.frame(stringsAsFactors=F) %>% set_colnames(c("var","condition","manual_calculation"))
  
  skip_example_solutions<-apply(skip_examples,1,function(x){
    !eval(parse(text=x["manual_calculation"]))
  })
  # debug(question_is_skipped_apply_condition_to_data)
  expect_identical(question_is_skipped_apply_condition_to_data(example$data,skip_examples[1,"condition"]),skip_example_solutions[,1])
  expect_identical(question_is_skipped_apply_condition_to_data(example$data,skip_examples[2,"condition"]),skip_example_solutions[,2])
  expect_identical(question_is_skipped_apply_condition_to_data(example$data,skip_examples[3,"condition"]),skip_example_solutions[,3])
  expect_identical(question_is_skipped_apply_condition_to_data(example$data,skip_examples[4,"condition"]),skip_example_solutions[,4])
  expect_identical(question_is_skipped_apply_condition_to_data(example$data,skip_examples[5,"condition"]),skip_example_solutions[,5])
  expect_identical(question_is_skipped_apply_condition_to_data(example$data,skip_examples[6,"condition"]),skip_example_solutions[,6])
  expect_identical(question_is_skipped_apply_condition_to_data(example$data,skip_examples[7,"condition"]),skip_example_solutions[,7])
  expect_identical(question_is_skipped_apply_condition_to_data(example$data,skip_examples[8,"condition"]),skip_example_solutions[,8])
})




test_that("question_is_skipped_apply_condition_to_data: empty input returns false",{
  example<-load.example("example1",F)
  expect_is(question_is_skipped,"function")                   
  expect_equal(question_is_skipped_apply_condition_to_data(example$data, ""),rep(F,nrow(example$data)))
  expect_equal(question_is_skipped_apply_condition_to_data(example$data, NA),rep(F,nrow(example$data)))
  expect_equal(question_is_skipped_apply_condition_to_data(example$data, NULL),rep(F,nrow(example$data)))
  expect_equal(question_is_skipped_apply_condition_to_data(example$data, "   "),rep(F,nrow(example$data)))
  
})

test_that("question_is_skipped_apply_condition_to_data: errors",{
  example<-load.example("example1",F)
  expect_is(question_is_skipped,"function")  
  expect_error(question_is_skipped_apply_condition_to_data(example$data, c(1,2)))
  expect_error(question_is_skipped_apply_condition_to_data(example$data, c(1)))
  expect_error(question_is_skipped_apply_condition_to_data(example$data, c(1128)))
  expect_error(question_is_skipped_apply_condition_to_data(example$data, c("a")))
  expect_error(question_is_skipped_apply_condition_to_data(example$data,"${nonexistent_variable}>0"))
  expect_error(question_is_skipped_apply_condition_to_data(example$data,c("${total_children}>0","${total_children}>0")))
  
})
