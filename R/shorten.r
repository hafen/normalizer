
#' @importFrom crayon yellow
cat_string <- function(x, min_len, max_len, max_total, outsep) {
  if (any(is.na(x))) {
    if (sum(is.na(x)) < length(x)) {
      warning(
        yellow("\ncat_string: argument x has NA's. Dropping all occurences"))
      x <- x[!is.na(x)]
    } else {
      stop(red("\ncat_string: argument consists only of NA's."))
    }
  }
  if (max_total < max_len) {
    warning(yellow(paste0( "max_total is less than max_len. ",
                           "Setting max_total to max_len.")))
    max_total <- max_len
  }
  x <- x[nchar(x) >= min_len]
  too_long <- which(nchar(x) > max_len)
  for (tl in too_long) {
    x[tl] <- substr(x[tl], 1, max_len)
  }
  while(sum(nchar(x)) > max_total) {
    if (length(x) == 1) {
      break
    }
    x <- x[-length(x)]
  }
  paste(x, collapse = outsep)
}

#' Shorten Character Strings
#'
#' @param x a vector of characters.
#' @param min_len the minimum length a string can be without being dropped. 
#' Default 3.
#' @param max_len the maximum length a string can be. If it's longer it
#' gets truncated. Default 10.
#' @param max_total the maximum total size of the shortened string.
#' Default 18.
#' @param sep the string separators. Default " ".
#' @param outsep the shortened string separator. Default "_".
#' @param cruft_pattern the regex desribing the characters that will be dropped
#' from the string.
#' @importFrom tibble tibble
#' @export
shorten_string <- function(x, min_len = 3, max_len = 10, max_total = 18, 
  sep = " ", outsep = "_", cruft_pattern = "[aeiou\\-]") {

  string <- x
  x <- gsub(cruft_pattern, "", x)
  ss <- strsplit(x, split = sep)
  short_string <- 
    unlist(lapply(ss, cat_string, min_len, max_len, max_total, outsep))
  tibble(string = string, short_string = short_string)
}

#' Shorten Character Strings to Acronym
#'
#' @param x a vector of characters.
#' @param sep the string separators. Default " ".
#' @param cruft_pattern the regex desribing the characters that will be dropped
#' from the string.
#' @importFrom tibble tibble
#' @export
shorten_acronym <- function(x, sep = " ", cruft_pattern = "[^a-zA-Z0-9 \\.]") {
  string <- x
  x <- gsub(cruft_pattern, "", x)
  ss <- strsplit(x, split = sep)
  short_string <- 
    unlist(lapply(ss,
      function(x) {
        paste(unlist(lapply(x, substr, 1, 1)), collapse = "")
      }))
  tibble(string = string, short_string = short_string)
}

#' Shorten Column Names of data.frame
#'
#' @param x the data.frame whose columnames should be shortened.
#' @param cols the column names to shorten. Can be integer denoting which
#' columns or character indicating the column names.
#' @param sep the shortened name separator.
#' @param method how should the string be shortened. Can be either 
#' shorten_string or shorten_acronym. Default shorten_string.
#' @param ... other parameters to be passed to method.
#' @importFrom crayon red
#' @export
shorten_colnames <- function(x, cols = seq_along(x), sep = " ", 
  method = shorten_string, ...) {

  if (is.character(cols)) {
    cols <- match(cols, colnames(x))
  }
  cn <- colnames(x)[cols]
  if (any(duplicated(cn))) {
    
    stop(red(paste("Non-unique column names: ", 
         paste(cn[duplicated(cn)], collapse = ", "))))
  }

  sc <- method(colnames(x)[cols], sep = sep, ...)

  if (any(duplicated(sc$short_string))) {
    stop(red(paste0("\nNon-unique shortened columns: \n", 
                    paste("\t", cn[duplicated(sc$short_string)], 
                          collapse = "\n"),
                    collapse = "")))
  }
  colnames(x)[cols] <- sc$short_string
  sc$cols <- cols
  if (is.null(attributes(x)$name_map)) {
    attributes(x)$name_map <- sc
  } else {
    attributes(x)$name_map <- rbind(attributes(x)$name_map, sc)
  }
  x
}

#' Get the Map of Shortened Names to Original Names
#'
#' @param x the data.frame to get the name map from.
#' @return if the name map exists, then the return is a tibble where the
#' column "string" is the original column name and "short_string" is the
#' shortened column name.
#' @importFrom tibble tibble
#' @export
name_map <- function(x) {
  nm <- attributes(x)$name_map
  if (is.null(nm)) {
    tibble(string = character(), short_string = character(), cols = integer())
  } else {
    nm
  }
}
