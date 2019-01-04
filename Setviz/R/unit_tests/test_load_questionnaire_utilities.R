
test_that("question_in_questionnaire returns FALSE unless question is in the qaire",{
  expect_true(question_in_questionnaire(tf$select_one[1]))
  expect_true(question_in_questionnaire(tf$select_one_NA_heavy[1]))
  expect_error(question_in_questionnaire(tf$select_one, data, questionnaire)) #list input instead of string + too many inputs
  expect_error(question_in_questionnaire(tf$NAs[1], questionnaire))
  expect_false(question_in_questionnaire(tf$fake[1]))
  rm(questionnaire); is_questionnaire_loaded <- function(){return(FALSE)}
  expect_false(question_in_questionnaire(tf$select_one[1]))
  expect_false(question_in_questionnaire(tf$select_one[1], data))
}) 


test_that("question_type_generic works with or without the questionnaire",{
  expect_error(question_type_generic(tf$select_one)) #data input missing with no default
  expect_equal(question_type_generic(tf$select_one[1], data), "select_one")
  expect_equal(question_type_generic(tf$select_multiple[1], data), "select_one")
  expect_equal(question_type_generic(tf$numeric_NA_heavy[1], data), "numeric")
  rm(questionnaire); is_questionnaire_loaded <- function(){return(FALSE)}
  expect_warning(question_type_generic(tf$select_multiple[1], data))
  expect_warning(question_type_generic(tf$numeric_NA_heavy[1], data))
  expect_error(question_type_generic(tf$fake[1]), data)
})
