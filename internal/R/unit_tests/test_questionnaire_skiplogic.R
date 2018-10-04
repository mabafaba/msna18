
context("Skip Logic")

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










