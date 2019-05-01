
#' The Proportion of NA's in a vector
#'
#' @param x a vector.
#' @importFrom crayon red yellow
#' @export
prop_na <- function(x) {
  UseMethod("prop_na", x)
}

prop_na.default <- function(x) {
  stop(red("Don't know how to get the proportion NA for a object of class ",
           class(x), ".", sep = ""))
}

prop_na_vector <- function(x) {
  if (length(x) == 0) {
    warning(yellow("Vector of length 0 passed to prop_na."))
  }
  sum(is.na(x)) / length(x)
}

prop_na.character <- prop_na.numeric <- prop_na.logical <- 
  prop_na.factor <- prop_na.list <- prop_na_vector

#' @export
prop_na.data.frame <- function(x) {
  ret <- rep(NA_real_, ncol(x))
  for (j in seq_along(x)) {
    ret[j] <- prop_na(x[[j]])
  }
  ret
}
