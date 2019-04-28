
# #' @importFrom stats as.formula
# #' @importFrom foreach foreach %do%
# #' @export
# make_formulas <- function(dep_vars, indep_vars) {
#   dv <- iv <- NULL
#   ret <- foreach(dv = dep_vars, .combine = c) %do% {
#     foreach(iv = indep_vars, .combine = c) %do% {
#       as.formula(paste(dv, "~", paste(indep_vars, sep = " + ")))
#     }
#   }
#   class(ret) <- "formulas"
#   ret
# }

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

#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom foreach foreach %do%
impute_n_times <- function(x, form, impute_model, num_imputes = 5, 
  impute_name = as.character(match.call()$impute_model),
  .key = "data") {

  if (num_imputes < 1) {
    ret <- tibble(model = character(), impute = integer(), .key = list())
    colnames(ret)[3] <- .key
  } else {
    foreach(i = seq_len(num_imputes), .combine = bind_rows) %do% {
      rt <- tibble(model = impute_name, impute = i, 
                   .key = list(impute_model(x, form)))
      colnames(rt)[3] <- .key
      rt
    }
  }
}

#' @importFrom stats na.omit
all_rows_imputed <- function(x, imputed_x) {
  isTRUE(
    all.equal(c(nrow(x), 
      unlist(lapply(imputed_x, function(df) nrow(na.omit(df)))))))
}

#' @importFrom simputation impute_rf impute_mf impute_rhd impute_knn
#' @importFrom equivalent equiv
#' @export
multiple_impute.formula <- function(x, form, 
  impute_model = list(imput_rf = simputation::impute_rf, 
                      imput_mf = simputation::impute_mf,
                      impute_rhd = simputation::impute_rhd, 
                      impute_knn = simputation::impute_knn),
  num_imputes = 5,
  remove_equivalents = TRUE,
  .key = "data") {
 
  if (is.function(impute_model)) {
    impute_n_times(x, form, impute_model, num_imputes)
  } else if (is.list(impute_model)) {
    idata <- foreach(i = seq_along(impute_model), .combine = bind_rows) %do% {
      impute_name <- names(impute_model)[i]
      if (!is.null(impute_name)) {
        imput_name <- ""
      }
      imd <- impute_n_times(x, form, impute_model[[i]], num_imputes, 
                            impute_name, .key = .key)
    }
    dup_vec <- rep(FALSE, length(idata))
    if (remove_equivalents) {
      for (i in seq_along(dup_vec[-1])) {
        for (j in (i+1):length(dup_vec)) {
          if (equiv(idata[[.key]][[i]], idata[[.key]][[j]])) {
            dup_vec[j] <- TRUE
          }
        }
      }
    }
    idata[!dup_vec]
  } else {
    stop(
      red("impute_model must be a function or list of imputation functions."))
  }
}

