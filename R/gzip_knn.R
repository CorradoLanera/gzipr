#' Gzip KNN
#'
#' For each of the `k` nearest neighbors, the distance is calculated as
#' the difference between the complexity of the reference and the
#' complexity of the test point, divided by the maximum complexity
#' between the two.
#'
#' @param x single data instance to classify
#' @param model gzipr model
#' @param k number of neighbors to consider
#'
#' @return the class of the majority of the `k` nearest neighbors
gzip_knn <- function(x, model, k = 3) {
  idx <- purrr::map_dbl(model, gzip_dist, x) |>
    order()
  kidx <- idx[seq_len(k)]

  names(model)[kidx] |>
    table() |>
    which.max() |>
    names()
}


#' Gzip Distance
#'
#' The distance is calculated as the difference between the complexity of
#' the reference and the complexity of the test point, divided by the
#' maximum complexity between the two.
#'
#' @param ref reference data instance
#' @param x single data instance to classify
#'
#' @return the distance between the two data instances
gzip_dist <- function(ref, x) {
  c_ref <- get_gzip_complexity(ref)
  c_x <- get_gzip_complexity(x)
  c_xref <- get_gzip_complexity(list(ref, x))

  (c_xref - min(c_x, c_ref)) / max(c_x, c_ref)
}

#' Gzip Complexity
#'
#' The complexity of a data instance is calculated as the length of the
#' compressed representation of the serialized object.
#'
#' @param x single data instance
#'
#' @return the complexity of the data instance
get_gzip_complexity <- function(x) {
  serialize(x, NULL) |>
    memCompress() |>
    length()
}
