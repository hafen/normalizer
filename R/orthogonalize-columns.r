
#' @title Orthogonalize Columns of a data.frame
#'
#' @description Orthogonalize the independent variables, specified by
#' a formula so that they will be orthogonal to one another. Orthogonalization
#' is done via an iterative Gram-Schmidt process beginning with the
#' left-most independent variable after pivoting to ensure numerical
#' stability.
#' @param x a data.frame
#' @param form the model description.
#' @param drop_colinear should colinear columns be dropped? (default TRUE)
#' @param order_pivot should the columns of the return be orderd by the
#' pivot? (default TRUE)
#' @return The return is the data frame with colums specified by the
#' formula and an orthogonalized independent variables.
#' @importFrom crayon red
#' @importFrom dplyr bind_cols
#' @importFrom stats terms
#' @export
orthogonalize_columns <- function(x, form, drop_colinear = TRUE, 
                                  order_pivot = TRUE) {
  if (!inherits(x, "data.frame")) {
    stop(red("Don't know how to orthogonalize an object with class ", class(x),
             ".", sep = ""))
  }
  rh_terms <- gsub("`", "", attributes(terms(form))$term.labels)
  lh_terms <- setdiff(all.vars(form), rh_terms)
  bind_cols(x[,lh_terms], as.data.frame(gram_schmidt(x[,rh_terms])))
}
