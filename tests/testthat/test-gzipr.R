test_that("gzip works for characters", {
  # setup
  smpl <- letters |> sample(10, replace = TRUE)

  # eval
  char_label <- gzipr(smpl, toupper(smpl))
  char_relabelles <- gzipr(char_label, smpl)

  # test
  expect_s3_class(char_label, "gzipr")
  expect_s3_class(char_relabelles, "gzipr")

  expect_length(char_label, length(smpl))
  expect_length(char_relabelles, length(smpl))

  expect_named(char_label, toupper(smpl))
  expect_named(char_relabelles, smpl)
  expect_error(
    gzipr(smpl),
    "y must be provided if x has no names"
  )
})


test_that("gzip works for data frames", {
  df <- gzipr(mtcars[, -2], mtcars[[2]])

  # test
  expect_s3_class(df, "gzipr")

  expect_length(unclass(df), nrow(mtcars))
  expect_list(df)

  expect_named(df, as.character(mtcars[[2]]))

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

  expect_error(
    gzipr(matrix(1:10, 2, 5)),
    "not yet implemented for objects of class matrix"
  )
})


test_that("gzip works for lists", {
  # setup
  lst_named <- list(a = 1:2, b = 3:4, a = 5:6)
  lst_unnamed <- list(1:2, 3:4, 5:6)
  labels <- c("a", "b", "a")

  # eval
  result <- gzipr(lst_named)
  result_unnamed <- gzipr(lst_unnamed, labels)

  # test
  expect_s3_class(result, "gzipr")
  expect_length(result, length(lst_named))
  expect_subset(names(result), names(lst_named))

  expect_s3_class(result_unnamed, "gzipr")
  expect_length(result_unnamed, length(lst_unnamed))
  expect_named(result_unnamed, labels)
})



test_that("predict.gzipr works on characters", {
  # setup
  smpl <- letters |>
    sample(10, replace = TRUE) |>
    purrr::set_names()
  model_self <- gzipr(smpl)
  model_label <- gzipr(smpl, toupper(smpl))

  # eval
  result_self <- predict(model_self, smpl)
  result_label <- predict(model_label, smpl)

  # test
  expect_subset(result_self, smpl)
  expect_subset(result_label, toupper(smpl))
})


test_that("predict.gzipr works on data frames", {
  # setup
  df <- mtcars
  model <- gzipr(df[, -2], df[[2]])

  # eval
  result <- predict(model, df[, -2])

  # test
  expect_subset(result, as.character(df[[2]]))
})
