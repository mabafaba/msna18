context("Test mapping to case works properly")


test_that("Map_to_case inputs correct",{
  # setwd("./internal/R/unit_tests/")
  example<-load.example("example1",F)
  expect_is(map_to_case,"function")                   
  
  attach(example$data) 
  tf <- example$tf
  
  })


test_that("case_vartype works",{
  expect_error(case_vartype(tf$select_one[1], NULL)) ### should this work if the data isnt loaded yet 
  expect_error(case_vartype(tf$select_one[1]), data)
})


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