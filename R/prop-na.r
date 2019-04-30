
#' The Proportion of NA's in a vector
#'
#' @param x a vector.
#' @importFrom crayon red yellow
#' @export
prop_na <- function(x) {
  if (!is.vector(x)) {
    stop(red("Don't know how to get the proportion NA for a object of class ",
             class(x), ".", sep = ""))
  }
  if (length(x) == 0) {
    warning(yellow("Vector of length 0 passed to prop_na."))
  }
  sum(is.na(x)) / length(x)
}
