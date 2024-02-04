# gzipr ---------------------------------------------------------

#' gzipr
#'
#' Citing the original paper: Deep neural networks (DNNs) are often used
#' for text classification tasks as they usually achieve high levels of
#' accuracy. However, DNNs can be computationally intensive with
#' billions of parameters and large amounts of labeled data, which can
#' make them expensive to use, to optimize and to transfer to
#' out-of-distribution (OOD) cases in practice. In this paper, we
#' propose a non-parametric alternative to DNNs that's easy,
#' light-weight and universal in text classification: a combination of a
#' simple compressor like gzip with a k-nearest-neighbor classifier.
#' Without any training, pre-training or fine-tuning, our method
#' achieves results that are competitive with non-pretrained deep
#' learning methods on six in-distributed datasets. It even outperforms
#' BERT on all five OOD datasets, including four low-resource languages.
#' Our method also performs particularly well in few-shot settings where
#' labeled data are too scarce for DNNs to achieve a satisfying
#' accuracy.
#'
#' @param x training data
#' @param y labels for training data, must be the same length of `x`
#'
#' @return a `gzipr` model object
#' @export
#'
#' @references Jiang, Zhiying, Matthew Y. R. Yang, Mikhail Tsirlin,
#' Raphael Tang, and Jimmy Lin. 2022. “Less Is More: Parameter-Free Text
#' Classification with Gzip.” - http://arxiv.org/abs/2212.09410
#'
#' @examples
#' library(gzipr)
#'
#' train <- mtcars[1:20, ]
#' test <- mtcars[21:32, ]
#'
#' train_x <- train[, -2]
#' train_y <- train[[2]]
#'
#' test_x <- test[, -2]
#' test_y <- test[[2]]
#'
#' model <- gzipr(train_x, train_y)
#' result <- predict(model, test_x)
#'
#' # accuracy
#' mean(result == test_y)
gzipr <- function(x, y = NULL) {
  UseMethod("gzipr")
}

#' @describeIn gzipr method to train on `character` vectors as input
#'   data
#' @export
gzipr.character <- function(x, y = NULL) {
  as.list(x) |>
    purrr::set_names(y) |>
    structure(class = "gzipr")
}

#' @describeIn gzipr method to train on `data.frame`s as input data
#' @export
gzipr.data.frame <- function(x, y = NULL) {
  seq_len(nrow(x)) |>
    purrr::map(\(id_row) x[id_row, , drop = FALSE]) |>
    purrr::set_names(y) |>
    structure(class = "gzipr")
}

#' @describeIn gzipr method to train
#'   on `list`s as input data
#' @export
gzipr.list <- function(x, y = NULL) {
  stopifnot(length(unique(purrr::map(x, class))) == 1L)
  purrr::set_names(x, y) |>
    structure(class = "gzipr")
}

#' @describeIn gzipr method to
#'   retrain a `gzipr` model mantaing the same data and possibly
#'   changing the labels
#' @export
gzipr.gzipr <- function(x, y = NULL) {
  if (!is.null(names(x))) {
    y <- y %||% names(x)
  }
  purrr::set_names(unname(x), y) |>
    structure(class = "gzipr")
}

#'
#' @export
gzipr.default <- function(x, y = NULL) {
  usethis::ui_stop("gzipr not implemented for {class(x)} objects yet")
}

#' @export
predict.gzipr <- function(object, newdata = NULL, k = 3) {
  stopifnot(is_gzipr(object))

  newdata <- gzipr(newdata %||% unname(object))
  purrr::map_chr(newdata, \(x1) gzip_knn(x1, object, k = k))
}
