
#' Apply the Gram-Schmidt Process to a Matrix
#'
#' @description Apply the Gram-Schmidt process to a matrix. This orthogonalizes
#' the columns of a matrix iteratively starting with the left-most column 
#' and moving to the right after a pivoting step to ensure numerical stability.
#' @param x a matrix
#' @param drop_colinear should colinear columns be dropped? (default TRUE)
#' @param order_pivot should the columns of the return be orderd by the 
#' pivot? (default TRUE)
#' @return the return matrix is an orthogonalized version of the input.
#' @examples
#' # Create a matrix from iris.
#' x <- model.matrix(~ Sepal.Length+Petal.Width+Petal.Length + Petal.Width - 1,
#'                   data = iris)
#'
#' # Colinear columns removed and columns are orthogonal.
#' crossprod(gram_schmidt(x))
#'
#' # Same linear subspace information as before:
#' summary(lm(Sepal.Width ~ Sepal.Length+Petal.Width+Petal.Length - 1,
#'            data = iris))
#'
#' summary(lm(iris$Sepal.Width ~ Sepal.Length+Petal.Width+Petal.Length - 1,
#'            data = as.data.frame(gram_schmidt(x))))
#' 
#' @importFrom dplyr bind_cols
#' @export
gram_schmidt <- function(x, drop_colinear = TRUE, order_pivot = TRUE) {
  UseMethod("gram_schmidt", x)
}

gram_schmidt.default <- function(x, drop_colinear, order_pivot) {
  stop(paste("Don't know how to perform Gram-Schmidt process on an object of",
             "type", class(x)))
}

gram_schmidt.data.frame <- function(x, drop_colinear = TRUE, 
                                    order_pivot = TRUE) {
  non_numeric_cols <- !unlist(lapply(x, is.numeric))
  nnc <- x[,non_numeric_cols, drop = FALSE]
  bind_cols(nnc, 
    as.data.frame(gram_schmidt(as.matrix(x[,!non_numeric_cols]), drop_colinear,
                               order_pivot)))
}

#' @export
gram_schmidt.matrix <- function(x, drop_colinear = TRUE, order_pivot = TRUE) {
  r <- qr(x)
  ret <- qr.Q(r)
  pivot <- r$pivot
  if (!is.null(colnames(x))) {
    colnames(ret) <- colnames(x)[pivot]
  }
  if (drop_colinear) {
    pivot <- pivot[seq_len(r$rank)]
    ret <- ret[, pivot, drop = FALSE]
  }
  if (order_pivot) {
    ret <- ret[, order(pivot)]
  } 
  attr(ret, "pivot") <- pivot
  attr(ret, "rank") <- r$rank
  ret
}
