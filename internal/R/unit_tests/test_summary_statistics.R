context("Summary stats test")


test_that("var_more_than_1 returns FALSE unless the dependent variable has two categories",{
  expect_equal(var_more_than_1(c(NULL,7,7,7)),FALSE)
  expect_equal(var_more_than_1(c(NA, 7,7,7)),FALSE)
  expect_equal(var_more_than_1(c("d", 7,7,7)),TRUE)
  expect_equal(var_more_than_1(c("d", "D")),TRUE) #do we want it to be case sensitive? ideally throw a warning
  expect_equal(var_more_than_1(c("d", "d")),FALSE)
  expect_equal(var_more_than_1(c(NA,NULL)),FALSE)
  expect_equal(var_more_than_1(c(7," ")),FALSE)
  expect_equal(var_more_than_1(c("  "," ")),FALSE)
  expect_equal(var_more_than_1(c(list(), "d")),FALSE)
  # expect_equal(1,2)
  # expect_equal(reduce_single_item_lists(1),1)
  # expect_equal(reduce_single_item_lists("A"),"A")  
  # expect_equal(reduce_single_item_lists(list()),list()) 
  # expect_equal(reduce_single_item_lists(list(1,2,list(1,2))),list(1,2,list(1,2)))
  # expect_equal(1,2)
}) 



test_that("percent_with_confints_select_one outputs correct",{
  ###This needs to be tested with a dependent var thats select one, one that's select multiple, one that's numeric etc
  expect_is(percent_with_confints_select_one(("today"), design), "data.frame")
  expect_error(percent_with_confints_select_one(("nb.youth"), design))
  expect_error(percent_with_confints_select_one(("bla"), design))
  expect_match(names(percent_with_confints_select_one(("today"), design)), "min",all = FALSE)
  expect_match(names(percent_with_confints_select_one(("today"), design)), "max",all = FALSE)
})

test_that("percent_with_confints_select_mult inputs correct",{
  expect_is(percent_with_confints_select_one(("type.chronic.disease"), design), "data.frame")
  expect_error(percent_with_confints_select_mult(("today"), design))
  expect_error(percent_with_confints_select_mult(("bla"), design))
})

test_that("percent_with_confints",{
  expect_is(percent_with_confints(("today"), design), "data.frame")
  expect_is(percent_with_confints(("type.chronic.disease"), design), "data.frame")
  expect_error(percent_with_confints(("nb.youth"), design))
})

 test_that("percent_with_confints_groups",{
   expect_error(percent_with_confints_groups(("today"), ("sheep"), design))
 })
 
 test_that("percent_with_confints_select_one_groups"){
   ###converts a numeric dependent var to a factor
   ### the names of the levels in the svytstat object worl
   
 }

#expect_warning
#expect_error
