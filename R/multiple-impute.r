
#' @title Create Multiple Imputations
#'
#' @param x a data.frame or matrix.
#' @param form a formula describing the model.
#' @param impute_model the imputation models to apply to the data. Default is
#' list(imput_rf = simputation::impute_rf, imput_mf = simputation::impute_mf,
#' impute_rhd = simputation::impute_rhd, impute_knn = simputation::impute_knn)
#' @param num_imputes the number of imputations per model. Default 5.
#' @param remove_equivalents after imputation should redundant imputed data
#' be removed. Default TRUE.
#' @param .key the name of column containing the list of imputed data sets.
#' Default is "data".
#' @export
multiple_impute <- function(x, form, impute_model, num_imputes, 
  remove_equivalents, .key) {
  
  # Dispatch on form, *not* the first argument, which is assumed to be a
  # data.frame/tibble.
  UseMethod("multiple_impute", form)
}

#' @importFrom crayon red
#' @export
multiple_impute.default <- function(x, form, impute_model, num_imputes,
  remove_equivalents, .key) {
  stop(red("Don't know how to impute when \"form\" argument is of type ", 
           class(form), ".", sep = ""))
}

#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom foreach foreach %do%
impute_n_times <- function(x, form, impute_model, num_imputes = 5, 
  impute_name = as.character(match.call()$impute_model),
  .key = "data") {
  
  i <- NULL

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
#' @importFrom foreach %dopar% registerDoSEQ getDoParName
#' @export
multiple_impute.formula <- function(x, form, 
  impute_model = list(imput_rf = simputation::impute_rf, 
                      imput_mf = simputation::impute_mf,
                      impute_rhd = simputation::impute_rhd, 
                      impute_knn = simputation::impute_knn),
  num_imputes = 5,
  remove_equivalents = TRUE,
  .key = "data") {

  if (is.null(getDoParName())) {
    registerDoSEQ()
  }
 
  if (is.function(impute_model)) {
    impute_n_times(x, form, impute_model, num_imputes)
  } else if (is.list(impute_model)) {
    idata <- foreach(i = seq_along(impute_model), 
                     .combine = bind_rows) %dopar% {
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

#' Average data.frames
#'
#' @param x a list of data.frames to average.
#' @importFrom dplyr bind_rows bind_cols
#' @importFrom crayon red
#' @export
tibbles_average <- function(x) {
  ret <- tibble()
  if (length(x) > 1) {
    num_cols <- unlist(lapply(x, ncol))
    if (!isTRUE(all(0 == diff(num_cols)))) {
      stop(red("Not all data sets have the same number of columns."))
    } else {
      num_cols <- num_cols[1]
    }
    num_rows <- unlist(lapply(x, nrow))
    if (!isTRUE(all(0 == diff(num_cols)))) {
      stop(red("Not all data sets have the same number of columns."))
    } else {
      num_rows <- num_rows[1]
    }

    ret <- foreach(j = seq_len(num_cols), .combine = bind_cols) %do% {
      cc <- foreach(di = seq_len(length(x)), .combine = bind_cols) %do% {
        x[[di]][,j, drop = FALSE]  
      }
      if (!isTRUE(all(unlist(lapply(cc, class)) == class(cc[[1]])))) {
        stop(red("Class inconsistency in column ", j, ".", sep = ""))
      }
      if (inherits(cc[[1]], "numeric")) {
        cc[[1]] <- apply(as.matrix(cc), 1, 
          function(x) {
            ret <- NA
            if (!isTRUE(all(is.na(x)))) {
              mean(x, na.rm = TRUE)
            }
          })
      } else if (inherits(cc[[1]], "character") || 
                 inherits(cc[[1]], "factor")) {
        for (i in seq_len(num_rows)) {
          tab <- sort(table(unlist(cc[i,])), decreasing = TRUE)
          if (length(tab) == 0) {
            cc[[1]][i] <- NA
          } else {
            cc[[1]][i] <- names(tab)[1]
          }
        }
      } else {
        stop(red("Don't know how to combine columns with class ", 
                 class(cc[[1]]), ".", sep = ""))
      }
      cc[,1]
    }
  } else if (length(x) == 1) {
    ret <- x[[1]]
  }
  ret
}
