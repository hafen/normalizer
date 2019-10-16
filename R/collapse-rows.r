

collapsible_vars <- function(x, group_var) {
  s <- NULL
  spl <- split(seq_len(nrow(x)), x[,group_var])
  if (length(spl) == nrow(x)) {
    character()
  } else {
    check_vars <- setdiff(colnames(x), group_var)
    check_vals <- Reduce(`&`,
      Map(function(s) {
            unlist(lapply(x[s, check_vars],
              function(x) {
                isTRUE(all(x == x[1])) | all(is.na(x))
              }))
          }, spl))
    check_vars[check_vals]
  }
}

#' Collapse the rows of a data.frame object
#'
#' @param x and ADaM formatted data.frame
#' @param key which variable should be collpased on? (Default: "USUBJID")
#' @param collapse_name the variable name of the collapsed sub-data.frames.
#' @importFrom tidyr nest
#' @export
collapse_rows <- function(x, key = "key", collapse_name = "data") {
  svs <- NULL
  sv <- c(key, collapsible_vars(x, key))
  nsv <- setdiff(colnames(x), sv)
  if (length(nsv) > 0 && length(unique(x[[key]])) < nrow(x)) {
    eval(parse(text = 
      gsub("collapse_name", collapse_name,
           "nest(x, collapse_name = colnames(x)[match(nsv, colnames(x))])")))
  } else {
    x
  }
}

