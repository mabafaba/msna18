context("Sanitation tests")

test_that("which_independent_more_than_one_record returns data",{
  data <- cbind(random_df(1000), "a", NA)
  expect_equal(which_independent_more_than_one_record(data, "groupsN5"), data)
  expect_equal(which_independent_more_than_one_record(data, "a"), data)
  expect_equal(which_independent_more_than_one_record(c("d", 7,7,7)),TRUE)
  expect_equal(independent_more_than_one_record(c("d", "D")),TRUE) #do we want it to be case sensitive? ideally throw a warning
})
test_file("./internal/R/unit_tests/test_sanitation.R")
