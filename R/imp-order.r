
#' @importFrom crayon yellow
imp_order <- function(group, imp_ordering = NULL) {
  ret <- group
  if (!is.null(imp_ordering)) {
    mi <- match(group, imp_ordering)
    names(mi) <- group
    if (isTRUE(any(is.na(mi)))) {
      warning(yellow("The following group variables were not found ",
                     "and will be last:\n\t",
                     paste(names(mi[is.na(mi)]), collapse = "\n\t"),
                     sep = ""))
    }
    ret <- names(sort(mi, na.last = TRUE))
  }
  ret
}
