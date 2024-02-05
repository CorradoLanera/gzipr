test_that("gzip_knn works", {
  # setup
  smpl <- letters |>
    sample(10, replace = TRUE) |>
    purrr::set_names()
  model <- gzipr(smpl)
  x <- sample(smpl, 1)

  # eval
  result <- gzip_knn(x, model)
  result_5 <- gzip_knn(x, model, 5)
  result_all <- gzip_knn(x, model, length(smpl))

  # test
  expect_subset(result, smpl)
  expect_subset(result_5, smpl)
  expect_subset(result_all, smpl)
  expect_length(unique(result_all), 1)
})


test_that("gzip_dist works", {
  # eval
  result_0 <- gzip_dist("aa", "aa")
  result_1 <- gzip_dist("aa", "ab")
  result_1_bis <- gzip_dist("aa", "ba")
  result_1_rev <- gzip_dist("ab", "aa")
  result_2 <- gzip_dist("aa", "bb")

  res_xy <- gzip_dist("xx", "xy")
  res_xz <- gzip_dist("xx", "xz")
  res_zy <- gzip_dist("xz", "xy")

  # test
  # d \in \R
  expect_number(result_0)

  # 1. d > 0
  min(result_0, result_1, result_1_bis, result_1_rev, result_2) |>
    expect_gte(0)

  # 2. d(x, y) = 0 <=> x == y
  expect_equal(result_0, 0)
  expect_gt(result_1, 0)
  expect_gt(result_1_bis, 0)
  expect_gt(result_2, 0)

  # 3. d(x, y) = dist(y, x)
  expect_equal(result_1, result_1_rev)

  # 4. d(x, y) <= d(x, z) + d(z, y)
  expect_lt(res_xy, res_xz + res_zy)

  # common sense for labels
  expect_gt(result_2, result_1)
  expect_equal(result_2, result_1_bis)
})


test_that("get_gzip_complexity works", {
  # eval
  result_x <- get_gzip_complexity("x")
  result_xx <- get_gzip_complexity("xx")

  # test
  expect_number(result_x)
  expect_gt(result_xx, result_x)
})
