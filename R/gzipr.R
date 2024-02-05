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
#' @param y (default = `NULL`) labels for training data, must be the
#'   same length of `x`, i.e., one label per observation. If provided,
#'   `y` is used as labels for `x`, otherwise, if `x` is named
#'   (`rownames` for `data.frame`s, `names` for `character`s) `y` is set
#'   to the names of `x`. If `x` is not named, `y` must be provided.
#' @param test (default = `FALSE`) whether the input data is for test
#'   (`TRUE`, i.e., without labels) or for train (`FALSE`)
#'
#' @return a `gzipr` model object
#' @export
#'
#' @references Jiang, Zhiying, Matthew Y. R. Yang, Mikhail Tsirlin,
#'   Raphael Tang, and Jimmy Lin. 2022. "Less Is More: Parameter-Free
#'   Text Classification with Gzip." - http://arxiv.org/abs/2212.09410
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
gzipr <- function(x, y = NULL, test = FALSE) {
  UseMethod("gzipr")
}

#' @describeIn gzipr method to train on `character` vectors as input
#'   data
#' @export
gzipr.character <- function(x, y = NULL, test = FALSE) {
  x <- as.list(x)

  y <- y %||% names(x)

  if (is.null(y)) {
    usethis::ui_stop("y must be provided if x has no names")
  }

  if (!test) {
    x <- purrr::set_names(x, y)
  }

  structure(x, class = "gzipr")
}

#' @describeIn gzipr method to train on `data.frame`s as input data
#' @export
gzipr.data.frame <- function(x, y = NULL, test = FALSE) {
  nrow_x <- nrow(x)
  y <- y %||% rownames(x)

  x <- seq_len(nrow_x) |>
    purrr::map(\(id_row) x[id_row, , drop = FALSE])

  if (!test) {
    if (
      is.null(y) ||
      isTRUE(all.equal(y, as.character(seq_len(nrow_x))))
    ) {
      usethis::ui_stop(paste(
        "y must be provided if x has no or standard rownames",
        "(i.e., seq_len(nrow(x)))."
      ))
    }
    x <- purrr::set_names(x, y)
  }

  structure(x, class = "gzipr")
}

#' @describeIn gzipr `r lifecycle::badge("experimental")`
#'   method to train on `list`s as input data
#' @export
gzipr.list <- function(x, y = NULL, test = FALSE) {
  stopifnot(length(unique(purrr::map(x, class))) == 1L)

  if (!test) {
    x <- purrr::set_names(x, y)
  }
  structure(x, class = "gzipr")
}

#' @describeIn gzipr method to retrain a `gzipr` model maintaining the
#'   same data and possibly changing the labels
#' @export
gzipr.gzipr <- function(x, y = NULL, test = FALSE) {
  if (!is.null(names(x))) {
    y <- y %||% names(x)
  }

  if (!test) {
    x <- purrr::set_names(unname(x), y)
  }
  structure(x, class = "gzipr")
}

#' @describeIn gzipr catch-all method for not yet implemented training
#'   objects
#' @export
gzipr.default <- function(x, y = NULL, test = FALSE) {
  usethis::ui_stop(paste(
    "gzipr not yet implemented for objects of class",
    "{paste(class(x), collapse = ', ')}"
  ))
}

#' @describeIn gzipr method to predict on a `gzipr` model
#'
#' @param gzipr trained model
#' @param newdata optional new test data to classify
#' @param k (int, default k = 3) number of neighbors to consider
#' @param .progress (logical, default .progress = TRUE) whether to
#'   display a progress bar.
#'
#' @export
predict.gzipr <- function(
    gzipr,
    newdata = NULL,
     k = 3,
    .progress = TRUE
) {
  stopifnot(is_gzipr(gzipr))

  newdata <- gzipr(newdata %||% unname(gzipr), test = TRUE)
  purrr::map_chr(
    newdata,
    \(x1) gzip_knn(x1, gzipr, k = k),
    .progress = .progress
  )
}
