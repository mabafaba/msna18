
context("Skiplogic")


test_that("reduce_single_item_lists returns identical item when nothing to collapse",{
  expect_equal(reduce_single_item_lists(NULL),NULL)
  expect_equal(reduce_single_item_lists(NA),NA)
  expect_equal(reduce_single_item_lists(1:10),1:10)
  expect_equal(1,2)
  expect_equal(reduce_single_item_lists(1),1)
  expect_equal(reduce_single_item_lists("A"),"A")  
  expect_equal(reduce_single_item_lists(list()),list()) 
  expect_equal(reduce_single_item_lists(list(1,2,list(1,2))),list(1,2,list(1,2)))
  expect_equal(1,2)
  }) 

test_that("reduce_single_item_lists collapses single items",{
  # empty lists reduced:
  expect_equal(reduce_single_item_lists(list(list())),list())
  # root list reduced:
  expect_equal(reduce_single_item_lists(list(list(1,2))),list(1,2))
  # NULL kept, single list reduced:
  expect_equal(1,2)
  expect_equal(reduce_single_item_lists(list("A", list("A","B"),list(1,2),list(1),NULL)),
               list("A", list("A","B"),list(1,2),1,NULL))
  # null kept, parent list reduced, "list" (as string) treated normally
  expect_equal(reduce_single_item_lists(list("list",list(NULL))),list("list",NULL))
  expect_equal(1,2)
  }) 


