### check that the names are all good
composite_indicator_names %in% names(data)


context("tests for expand_composite_indicators_to_set_intersections")

example<-load.example("example1",F)
data <- example$data
tf <- example$tf
questionnaire <- example$questionnaire
design <- svydesign(~0, data = data)


test_that("confidence_intervals_mean inputs correct",{
  ###This needs to be tested with a dependent var thats select one, one that's select multiple, one that's numeric etc
  expect_is(expand_composite_indicators_to_set_intersections(tf$numeric[1], design = design), "data.frame") #numerical var
  expect_is(expand_composite_indicators_to_set_intersections(tf$numeric_NA_heavy[1], design = design), "data.frame") #numerical var
  expect_is(expand_composite_indicators_to_set_intersections(tf$logical[1], design = design), "data.frame")
  expect_warning(expand_composite_indicators_to_set_intersections(tf$numeric[1], tf$select_one[2] , design = design))
  expect_error(expand_composite_indicators_to_set_intersections(tf$select_one[1], design = design))
  expect_error(expand_composite_indicators_to_set_intersections(tf$select_one[1], design = design))
  expect_error(expand_composite_indicators_to_set_intersections(tf$NAs[1], design = design))
  expect_error(expand_composite_indicators_to_set_intersections(tf$fake[1], design = design)) #nonexistent.var
  expect_error(expand_composite_indicators_to_set_intersections(tf$select_multiple[1], design = design)) # select multiple
})
#
#
# test_that("confidence_intervals_mean outputs correct",{
#   ###This needs to be tested with a dependent var thats select one, one that's select multiple, one that's numeric etc
#   expect_named(confidence_intervals_mean(tf$numeric[1], design = design), c("dependent.var","independent.var",
#                                                                             "dependent.var.value","independent.var.value",
#                                                                             "numbers","se","min","max"))
#   expect_true(all(is.na(confidence_intervals_mean(tf$numeric[1], design = design)[["independent.var.value"]])))
#   expect_true(all(is.na(confidence_intervals_mean(tf$numeric[1], design = design)[["dependent.var.value"]])))
#   expect_true(is.numeric(expect_true(all(is.na(confidence_intervals_mean(tf$numeric[1], design = design)[["numbers"]])))))
# })
#
# test_that("confidence_intervals_mean_groups inputs correct",{
#   ###This needs to be tested with a dependent var thats select one, one that's select multiple, one that's numeric etc
#   expect_is(confidence_intervals_mean_groups(tf$numeric[1], tf$select_one[2], design), "data.frame") #numerical var
#   expect_is(confidence_intervals_mean(tf$logical[2], tf$select_one[1], design = design), "data.frame")
#   expect_is(confidence_intervals_mean_groups(tf$numeric_NA_heavy[1],tf$select_one[2],  design), "data.frame") ####THIS FAILS, should it ?
#   expect_error(confidence_intervals_mean_groups(tf$select_one[1], tf$select_one[2], design))
#   expect_error(confidence_intervals_mean_groups(tf$NAs[1], tf$select_one[2], design))
#   expect_error(confidence_intervals_mean_groups(tf$fake[1],tf$select_one[2], design)) #nonexistent.var
#   expect_error(confidence_intervals_mean_groups(tf$select_multiple[1],tf$select_one[2], design)) # select multiple
# })
#
# test_that("confidence_intervals_mean_groups outputs correct",{
#   ###This needs to be tested with a dependent var thats select one, one that's select multiple, one that's numeric etc
#   expect_named(confidence_intervals_mean_groups(tf$numeric[1], tf$select_on[2], design), c("dependent.var","independent.var",
#                                                                                            "dependent.var.value","independent.var.value",
#                                                                                            "numbers","se","min","max"))
#   expect_true(all(is.na(confidence_intervals_mean(tf$numeric[1],  tf$select_on[2], design)[["dependent.var.value"]])))
#   expect_true(is.numeric(expect_true(all(is.na(confidence_intervals_mean(tf$numeric[1], design)[["numbers"]])))))
# })
