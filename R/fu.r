
rh_terms <- function(form, data = NULL) {
#  ret <- gsub("`", "", attributes(terms(form))$term.labels)
  ret <- attributes(terms(form, data = data))$term.labels
  if (length(ret) == 1 && length(grep("\\|", ret)) > 0) {
    rh_split <- strsplit(ret, "\\|")[[1]]
    ret <- lapply(rh_split,
      function(x) {
        nsp <- gsub(" ", "", x)
        if (nsp == ".") {
          nsp
        } else {
          gsub("`", "", 
            attributes(terms(as.formula(paste("~", x))))$term.labels)
        }
      })
    names(ret) <- c("indep", "cond")
  } else {
    ret <- list(indep = ret, cond = NULL)
  }
  ret
}

indep_terms <- function(form, data = NULL) {
  rh_terms <- rh_terms(form, data)$indep
}

cond_terms <- function(form, data = NULL) {
  rh_terms <- rh_terms(form)$cond
}

lh_terms <- function(form, data = NULL) {
  ret <- NULL
  facs <- attributes(terms(form, data = data))$factors
  if (nrow(facs) > ncol(facs)) {
    lht_all <- rownames(attributes(terms(form, data = data))$factors)[1]
    ret <- all.vars(parse(text = lht_all))
  }
  ret
}

#' Get a Description of a formula Object
#'
#' @param x a data.frame.
#' @param form a formula object.
#' @importFrom crayon red
#' @export
form_desc <- function(x, form) {
  ft <- c(list(lh_terms=lh_terms(form, x)), rh_terms(form, x))
  if (isTRUE(any(duplicated(unlist(ft))))) {
    stop(red("Variables may not be apear more than once"))
  }
  dt_list_elem <- which(unlist(lapply(ft, function(x) "." %in% x)))
  if (length(dt_list_elem) > 0) {
    dt <- which("."  == ft[[dt_list_elem]])
    ft[[dt_list_elem]] <- c(ft[[dt_list_elem]][-dt], 
      setdiff(colnames(x), setdiff(unlist(ft), ".")))
  }
  if (isTRUE(!all(unlist(ft) %in% colnames(x)))) {
    stop(red("The following formula variables do not appear in data set.\n\t", 
             paste(setdiff(unlist(ft), colnames(x)), collapse = "\n\t"),
             sep = ""))
  }
  ft
}


