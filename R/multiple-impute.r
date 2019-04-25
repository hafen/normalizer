
#' @importFrom foreach foreach %do%
#' @export
make_formulas <- function(dep_vars, indep_vars) {
  ret <- foreach(dv = dep_vars, .combine = c) %do% {
    foreach(iv = indep_vars, .combine = c) %do% {
      as.formula(paste(df, "~", paste(indep_vars, sep = " + ")))
    }
  }
  class(ret) <- "formulas"
}

#' @export
multiple_impute <- function(x, form, impute_model, num_imputes) {
  
  # Dispatch on form, *not* the first argument, which is assumed to be a
  # data.frame/tibble.
  UseMethod("multiple_impute", form)
}

#' @importFrom crayon red
#' @export
multiple_impute.default <- function(x, form, impute_model, num_imputes) {
  stop(red("Don't know how to impute when \"form\" argument is of type ", 
           class(form), ".", sep = ""))
}

impute_n_times <- function(x, form, impute_model, num_imputes, attr = NULL) {
  foreach(seq_len(num_imputes)) %do% {
    ret <- impute_model(x, form)
    if (!is.null(attr)) {
      attributes(ret) <- attr
    }
  }
}

all_rows_imputed <- function(x, imputed_x) {
  isTRUE(
    all.equal(c(nrow(x), unlist(lapply(ret, function(x) nrow(na.omit(x)))))))
}


#' @importFrom equivalent equiv
#' @export
multiple_impute.formula <- function(x, form, 
  impute_model = list(imput_rf = impute_rf, imput_mf = impute_mf,
                      impute_rhd = impute_rhd, impute_knn = impute_knn),
  num_imputes = 5,
  remove_equivalents = TRUE) {
 
  if (is.function(impute_model)) {
    impute_n_times(x, form, impute_model, num_imputes)
  } else if (is.list(impute_model)) {
    idata <- foreach(i = seq_along(impute_model), .combine = c) %do% {
      attr <- NULL
      if (!is.null(names(impute_model)[i])) {
        attr <- names(impute_model[i])
      }
      imd <- impute_n_times(x, form, impute_model[i], num_imputes, attr)
    }
    equiv_mat <- outer(idata, idata, FUN = equiv)
  } else {
    stop(
      red("impute_model must be a function or list of imputation functions."))
  }
}

#' @importFrom crayon yellow
#' @export
multiple_impute.formulas <- function(x, form, 
  impute_model = list(imput_rf = impute_rf, imput_mf = impute_mf, 
                       impute_rhd = impute_rhd, impute_knn = impute_knn),
  num_imputes = 5) {

}
