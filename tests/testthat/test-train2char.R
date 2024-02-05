test_that("train2char works for data.frames", {
  # setup
  first <- mtcars[1, ]
  second <- mtcars[2, ]
  double <- rbind(first, second)
  same <- rbind(first, first)

  expected_first <- paste0(
    "<FLD>mpg<VAL>21<FLD>cyl<VAL>6<FLD>disp<VAL>160<FLD>hp<VAL>110",
    "<FLD>drat<VAL>3.9<FLD>wt<VAL>2.62<FLD>qsec<VAL>16.46",
    "<FLD>vs<VAL>0<FLD>am<VAL>1<FLD>gear<VAL>4<FLD>carb<VAL>4"
  )
  expected_second <- paste0(
    "<FLD>mpg<VAL>21<FLD>cyl<VAL>6<FLD>disp<VAL>160<FLD>hp<VAL>110",
    "<FLD>drat<VAL>3.9<FLD>wt<VAL>2.875<FLD>qsec<VAL>17.02",
    "<FLD>vs<VAL>0<FLD>am<VAL>1<FLD>gear<VAL>4<FLD>carb<VAL>4"
  )
  expected_double <- paste0(
    "<FLD>mpg<VAL>c(21, 21)<FLD>cyl<VAL>c(6, 6)<FLD>disp<VAL>c(160, 160)",
    "<FLD>hp<VAL>c(110, 110)<FLD>drat<VAL>c(3.9, 3.9)",
    "<FLD>wt<VAL>c(2.62, 2.875)<FLD>qsec<VAL>c(16.46, 17.02)",
    "<FLD>vs<VAL>c(0, 0)<FLD>am<VAL>c(1, 1)<FLD>gear<VAL>c(4, 4)",
    "<FLD>carb<VAL>c(4, 4)"
  )

  # eval
  result_first <- train2char(first)
  result_second <- train2char(second)
  result_double <- train2char(double)
  result_same <- train2char(same)

  # test
  expect_string(result_first)
  expect_string(result_second)
  expect_string(result_double)
  expect_string(result_same)

  expect_equal(result_first, expected_first)
  expect_equal(result_second, expected_second)
  expect_equal(result_double, expected_double)
  expect_equal(result_same, expected_first)
})
