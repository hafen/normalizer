
#' @title Orthogonalize Columns of a data.frame
#'
#' @description Orthogonalize the independent variables, specified by
#' a formula so that they will be orthogonal to one another. Orthogonalization
#' is done via an iterative Gram-Schmidt process beginning with the
#' left-most independent variable after pivoting to ensure numerical
#' stability.
#' @param x a data.frame.
#' @param form the model description. If missing, then a formula is generated
#' based on numeric groupings (see group_numeric_vars). If no grouping 
#' exists, then orthogonalization is performed for the entire data frame.
#' @param imp_ordering column names of x ordered by their importance. 
#' (default NULL - #' no importance ordering)
#' @param drop_colinear should colinear columns be dropped? (default TRUE)
#' @param order_pivot should the columns of the return be orderd by the
#' pivot? (default TRUE)
#' @return The return is the data frame with colums specified by the
#' formula and an orthogonalized independent variables.
#' @importFrom dplyr bind_cols
#' @importFrom crayon red
#' @export
orthogonalize_columns <- function(x, form = NULL, imp_ordering = NULL,
                                  drop_colinear = TRUE, 
                                  order_pivot = TRUE) {
  if (!inherits(x, "data.frame")) {
    stop(red("Don't know how to orthogonalize an object with class ", class(x),
             ".", sep = ""))
  }
  orth_groups <- list(colnames(x))
  extra_keep_vars <- character()
  if (!is.null(form)) {
    fd <- form_desc(x, form)
    orth_groups <- list(fd$indep)
    extra_keep_vars <- unlist(c(fd$lh_terms, fd$cond))
  } else if (!is.null(attributes(x)$colinear_groups)) {
    orth_groups <- attributes(x)$colinear_groups
    extra_keep_vars <- setdiff(colnames(x), unlist(orth_groups))
  }
  ret <- x[,extra_keep_vars, drop = FALSE]
  for (i in seq_along(orth_groups)) {
    group <- orth_groups[[i]]
    imp_ordered_group <- imp_order(group, imp_ordering)
    ret <- bind_cols(ret, 
      gram_schmidt(x[ ,imp_ordered_group], drop_colinear, order_pivot))
  }
  ret
}
