#' @export
train2char <- function(x) {
  UseMethod("train2char")
}

#' @export
train2char.data.frame <- function(x) {
  paste0(
    "<FLD>",
    stringr::str_c(
      names(x),
      as.character(unique(x)),
      sep = "<VAL>",
      collapse = "<FLD>"
    )
  )
}

#' @export
train2char.default <- function(x) {
  row.names(x) <- NULL
  as.character(unique(x))
}
