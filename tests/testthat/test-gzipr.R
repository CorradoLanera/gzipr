test_that("gzip works", {
  # setup
  smpl <- letters |> sample(10, replace = TRUE)

  # eval
  char_self <- gzipr(smpl)
  char_label <- gzipr(smpl, toupper(smpl))
  char_relabelles <- gzipr(char_label, smpl)
  df <- gzipr(mtcars[, -2], mtcars[[2]])


  # test
  expect_s3_class(char_self, "gzipr")
  expect_s3_class(char_label, "gzipr")
  expect_s3_class(char_relabelles, "gzipr")
  expect_s3_class(df, "gzipr")

  expect_length(char_self, length(smpl))
  expect_length(char_label, length(smpl))
  expect_length(char_relabelles, length(smpl))
  expect_length(unclass(df), nrow(mtcars))
  expect_list(df)

  expect_named(char_self, smpl)
  expect_named(char_label, toupper(smpl))
  expect_named(char_relabelles, smpl)
  expect_named(df, as.character(mtcars[[2]]))

  expect_error(
    gzipr(matrix(1:10, 2, 5)),
    "not yet implemented for objects of class matrix"
  )

  expect_s3_class(
    data.frame(
      a = 1:2, b = 3:4, c = 5:6,
      row.names = c("d", "e")
    ) |>
      gzipr(),
    "gzipr"
  )
  expect_error(
    data.frame(a = 1:2, b = 3:4, c = 5:6) |>
      gzipr(),
    "y must be provided if x has no or standard rownames"
  )

})


test_that("predict.gzipr works", {
  # setup
  smpl <- letters |> sample(10, replace = TRUE)
  model_self <- gzipr(smpl)
  model_label <- gzipr(smpl, toupper(smpl))

  # eval
  result_self <- predict(model_self, smpl)
  result_label <- predict(model_label, smpl)

  # test
  expect_subset(result_self, smpl)
  expect_subset(result_label, toupper(smpl))
})
