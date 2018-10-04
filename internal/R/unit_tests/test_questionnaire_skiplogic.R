
context("Skip Logic: basics")

test_that("reduce_single_item_lists identities",{
  expect_equal(reduce_single_item_lists(NULL),NULL)
  expect_equal(reduce_single_item_lists(NA),NA)
  expect_equal(reduce_single_item_lists(1:10),1:10)
  expect_equal(reduce_single_item_lists(1),1)
  expect_equal(reduce_single_item_lists("A"),"A")
  expect_equal(reduce_single_item_lists(list()),list())
  expect_equal(reduce_single_item_lists(list(1,2,list(1,2))),list(1,2,list(1,2)))
  })

test_that("reduce_single_item_lists works",{
  # empty lists reduced:
  expect_equal(reduce_single_item_lists(list(list())),list())
  # root list reduced:
  expect_equal(reduce_single_item_lists(list(list(1,2))),list(1,2))
  # NULL kept, single list reduced:
  expect_equal(reduce_single_item_lists(list("A", list("A","B"),list(1,2),list(1),NULL)),
               list("A", list("A","B"),list(1,2),1,NULL))
  # null kept, parent list reduced, "list" (as string) treated normally
  expect_equal(reduce_single_item_lists(list("list",list(NULL))),list("list",NULL))
  })



# expect_error_string_w_brackets_to_hierarchical_list


test_that("string_w_brackets_to_hierarchical_list errors on non-character input",{
  expect_error_string_w_brackets_to_hierarchical_list<-function(x){expect_error(string_w_brackets_to_hierarchical_list(x))}
  expect_error_string_w_brackets_to_hierarchical_list(NULL)
  expect_error_string_w_brackets_to_hierarchical_list(NA)
  expect_error_string_w_brackets_to_hierarchical_list(list())
  expect_error_string_w_brackets_to_hierarchical_list(c(1,2,3))
})





test_that("string_w_brackets_to_hierarchical_list identities",{
  expect_returns_list_of_input<-function(x){expect_identical(string_w_brackets_to_hierarchical_list(x),list(x))}
  expect_returns_list_of_input("")
  expect_returns_list_of_input("A")
  expect_returns_list_of_input("asdf")
  expect_returns_list_of_input("[][][]]")
  expect_returns_list_of_input("1,2,3")
  expect_returns_list_of_input(")(")
  expect_returns_list_of_input("()")
})




test_that("string_w_brackets_to_hierarchical_list works",{
  expect_equal_string_w_brackets_to_hierarchical_list<-function(input,expectation){
    expect_equal(string_w_brackets_to_hierarchical_list(input),expectation)
    }

  expect_equal_string_w_brackets_to_hierarchical_list("a(b)",list("a",list("b")))
  expect_equal_string_w_brackets_to_hierarchical_list("a(b(c))",list("a",list("b",list("c"))))
  expect_equal_string_w_brackets_to_hierarchical_list("(1)2(3)",list(list("1"),"2",list("3")))
  expect_equal_string_w_brackets_to_hierarchical_list("a(b)",list("a",list("(b)")))
  expect_equal_string_w_brackets_to_hierarchical_list("a()",list("a",list("")))
})

test_that("string_w_brackets_to_hierarchical_list errors non-string input",{
  expect_error_string_w_brackets_to_hierarchical_list<-function(input){
    expect_error(string_w_brackets_to_hierarchical_list(input))
  }

  expect_error_string_w_brackets_to_hierarchical_list(NULL)
  expect_error(string_w_brackets_to_hierarchical_list(NA))
  expect_error(string_w_brackets_to_hierarchical_list(list()))
  expect_error(string_w_brackets_to_hierarchical_list(list("A")))
  expect_error(string_w_brackets_to_hierarchical_list(c("A","B")))
  expect_error(string_w_brackets_to_hierarchical_list(c(1)))
  expect_error(string_w_brackets_to_hierarchical_list(factor("A")))
})



# split_on_highest_brackets

test_that("split_on_highest_brackets identities (as list)",{
  expect_equal(split_on_highest_brackets("a"),list("a"))
  expect_equal(split_on_highest_brackets("asdf"),list("asdf"))
  expect_equal(split_on_highest_brackets("1"),list("1"))
  expect_equal(split_on_highest_brackets(""),list(""))
  expect_equal(split_on_highest_brackets("NA"),list("NA"))
  expect_equal(split_on_highest_brackets(")("),list(")("))
})

test_that("split_on_highest_brackets errors: non-character input",{
  expect_error(split_on_highest_brackets(NA))
  expect_error(split_on_highest_brackets(NULL))
  expect_error(split_on_highest_brackets(list()))
  expect_error(split_on_highest_brackets(factor(1)))
  expect_error(split_on_highest_brackets(c(1,2,3)))
  expect_error(split_on_highest_brackets(c(1,2,3)))
  expect_error(split_on_highest_brackets(c("A","B")))
})

test_that("split_on_highest_brackets works",{
  expect_equal(length(split_on_highest_brackets("A (B)")),2)
  expect_equal(length(split_on_highest_brackets("A (B(C))")),2)
  expect_is(split_on_highest_brackets("A (B(C))")[[1]],"character")
  expect_is(split_on_highest_brackets("A (B(C))")[[2]],"character")
  expect_equal(split_on_highest_brackets("abc de )( fgh (ijk)  lmn"),
            list("abc de )( fgh ","(ijk)","  lmn"))
  expect_equal(split_on_highest_brackets("(abc(de)fg)"),
               list("(abc","(de)","fg)"))


  expect_equal(split_on_highest_brackets("(abc(de)fg(hi)jk)"),
               list("(abc","(de)","fg","(hi)","jk)"))
}
)





test_that("is_numeric_condition identity: FALSE if not a condition",{
  expect_false(is_numeric_condition(""))
  expect_false(is_numeric_condition("A"))
  expect_false(is_numeric_condition("()()()"))
  expect_false(is_numeric_condition("a{}"))
  expect_false(is_numeric_condition("a{1=2}"))
  expect_false(is_numeric_condition("<2}"))
  expect_false(is_numeric_condition("<2>}"))
  expect_false(is_numeric_condition("=<"))
  expect_false(is_numeric_condition("   "))
  })

test_that("is_numeric_condition errors: not single item character input",{
  expect_error(is_numeric_condition(NA))
  expect_error(is_numeric_condition(NULL))
  expect_error(is_numeric_condition(list("a")))
  expect_error(is_numeric_condition(list()))
  expect_error(is_numeric_condition(factor("a")))
  expect_error(is_numeric_condition(TRUE))
  expect_error(is_numeric_condition(c("a","b")))
  expect_error(is_numeric_condition(c("a",NA)))
})




context("Skip Logic: applied skip test")


test_that("question_is_skipped_apply_condition_to_data errors: non-character condition",{

  question_is_skipped_apply_condition_to_data_fixed_data<-function(condition){
    question_is_skipped_apply_condition_to_data(data.frame(a=c(1,2,3)),condition)
  }

  #
  expect_error(question_is_skipped_apply_condition_to_data_fixed_data(NULL))
  expect_error(question_is_skipped_apply_condition_to_data_fixed_data(c(1,2,3)))
  expect_error(question_is_skipped_apply_condition_to_data_fixed_data(c("a","b")))
  expect_error(question_is_skipped_apply_condition_to_data_fixed_data(c("a",NA)))
  expect_error(question_is_skipped_apply_condition_to_data_fixed_data(c("a",NA)))
})

test_that("question_is_skipped_apply_condition_to_data errors: invalid condition string",{
  question_is_skipped_apply_condition_to_data_fixed_data<-function(condition){
    question_is_skipped_apply_condition_to_data(data.frame(a=c(1,2,3)),condition)
  }

  expect_error(question_is_skipped_apply_condition_to_data_fixed_data("bad condition"))
  expect_error(question_is_skipped_apply_condition_to_data_fixed_data("a<=b"))
  expect_error(question_is_skipped_apply_condition_to_data_fixed_data("123"))
  expect_error(question_is_skipped_apply_condition_to_data_fixed_data("select(${a},'1'"))
  expect_error(question_is_skipped_apply_condition_to_data_fixed_data("()a=b()()+x and {!}"))
})


test_that("question_is_skipped_apply_condition_to_data FALSE on empty condition",{
  question_is_skipped_apply_condition_to_data_fixed_data<-function(condition){
    question_is_skipped_apply_condition_to_data(data.frame(a=c(1,2,3)),condition)
  }
  all_false<-c(F,F,F)
  expect_equal(question_is_skipped_apply_condition_to_data_fixed_data(NA),all_false)
  expect_equal(question_is_skipped_apply_condition_to_data_fixed_data(""),all_false)
  })



test_that("question_is_skipped_apply_condition_to_data works",{
  # setwd("./internal/R/unit_tests/")
  example<-load.example("example1")
  
  expect_is(question_is_skipped,"function")                   

  attach(example$data)
  skip_examples<-c("settlement_other","selected(${settlement},\"other\")",'settlement=="other"',
                      "plw","${females_13_15}>0 or ${females_16_17}>0 or ${females_18_40}>0",'females_13_15>0 | females_16_17>0 | females_18_40>0',
                     "uasc","${total_children}>0", "total_children>0",
                     "note_vuln_gender","${disabled_chronic}>0 or ${sick_children}>0 or ${mental}>0 or ${uasc}>0","disabled_chronic>0 | sick_children>0 | mental>0 | uasc>0"
                     
                     
                     ) %>% 
                      matrix(3,4,byrow=F) %>% t %>% as.data.frame(stringsAsFactors=F) %>% set_colnames(c("var","condition","manual_calculation"))
  
    skip_example_solutions<-apply(skip_examples,1,function(x){
      !eval(parse(text=x["manual_calculation"]))
    })

  detach(example$data)
  
  expect_identical(question_is_skipped_apply_condition_to_data(example$data,skip_examples[1,"condition"]),skip_example_solutions[,1])
  expect_identical(question_is_skipped_apply_condition_to_data(example$data,skip_examples[2,"condition"]),skip_example_solutions[,2])
  expect_identical(question_is_skipped_apply_condition_to_data(example$data,skip_examples[3,"condition"]),skip_example_solutions[,3])
  expect_identical(question_is_skipped_apply_condition_to_data(example$data,skip_examples[4,"condition"]),skip_example_solutions[,4])
                   
  })
