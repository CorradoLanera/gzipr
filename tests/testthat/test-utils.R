test_that(" `%||%` works", {
  expect_equal(NULL %||% 4, 4)
  expect_equal(2 %||% 4, 2)
})
