
#' Process, Normalize, and, Consolidate Data Sets
#'
#' @param x a named list of data.frames.
#' @param collapse_on which variable should be collapsed on?
#' @param collapse_name the variable name of the collapsed sub-data.frames.
#' Default is the names of 'x'.
#' @param handle_contra_vals if variables have contradicting values across
#' data.frame's, then can they be handled by using the first instance of
#' the variable in the set? (Default FALSE) See Details for more information.
#' @param remove_equiv_columns should equivalent columns be remove 
#' (Default FALSE).
#' @param keep_cols columns that must be kept in the returned data.frame
#' object.
#' @param verbose should extra information be provided? (Default TRUE)
#' @details Setting 'handle_contra_vals' to TRUE should be used with 
#' extreme caution. If it is used then the contradicting value are stored
#' as an attribute of the consolidated data.frame and can be retrieved using
#' the function 'get_contradictions()'.
#' @importFrom cli cat_rule
#' @importFrom crayon yellow red
#' @importFrom dplyr select 
#' @export
pnc_ds <- function(x, collapse_on, 
                   collapse_name = names(x), 
                   handle_contra_vals = FALSE, 
                   remove_equiv_columns = TRUE, 
                   keep_cols = character(), verbose = TRUE) {

  xs <- lapply(seq_along(x),
  function(i) {
    if (verbose) {
      cat("\n")
      cat_rule(paste("Processing", names(x)[i]))
      cat("\n")
    }

    normalize(x[[i]], 
              collapse_on = collapse_on,
              collapse_name = collapse_name[i],
              remove_equiv_columns = remove_equiv_columns,
              keep_cols = character(),
              verbose = verbose)
  })
  if (verbose) {
    cat("\n")
  }
  names(xs) <- collapse_name

  var_contradictions <- contradicting_vars(xs, collapse_on = collapse_on)
  contradictions <- list()

  if (length(var_contradictions) > 0) {
    if (!handle_contra_vals) {
      contra_string <- paste(vapply(seq_along(var_contradictions),
        function(i) {
          paste0(paste0(names(var_contradictions)[i], ": "),
                 paste(var_contradictions[[i]], collapse = " "),
                 collapse = " ")
        }, ""), collapse = "\n\t")
      err_mesg <- paste0("Contradicting variables:\n\t", contra_string)
      if (nchar(err_mesg) > getOption("warning.length")) {
        warning(
          yellow("Increasing warning length to accomodate error message."))
        options("warning.length" = nchar(err_mesg))
      }
      stop(red(err_mesg))
    } else {
      for (i in seq_along(var_contradictions)) {
        vct <- NULL
        var_contrs <- var_contradictions[[i]]
        var_name <- names(var_contradictions)[i]
        for (vc in var_contrs[-1]) {
          if (is.null(vct)) {
            vct <- xs[[vc]][, c(collapse_on, var_name)]
            names(vct)[2] <- vc
          } else {
            vt <- xs[[vc]][, c(collapse_on, var_name)]
            names(vt)[2] <- vc
            vct <- full_join(vct, vt, by = collapse_on)
          }
          xs[[vc]] <- xs[[vc]] %>% select(-!!var_name)
        }
        contradictions <- c(contradictions, list(vct))
      }
      names(contradictions) <- names(var_contradictions)
    }
    
  }

  xs <- consolidate(xs, collapse_on = collapse_on)
  if (remove_equiv_columns) {
    xs <- remove_equiv_columns(xs, verbose = verbose)
  }
  if (length(contradictions) > 0) {
    attributes(xs)$contra <- contradictions
  }
  xs
}

#' Print a message
#' 
#' @param x the data the function will return
#' @param msg the message to cat.
#' @param verbose should the message be cat'ed? Default TRUE.
#' @param style the crayon style to use when printing. Default reset.
#' @param ... other paramters passed to cat.
#' @importFrom crayon reset
tcat <- function(x, msg, verbose = TRUE, style = reset, ...) {
  if (verbose) {
    cat(style(msg), ...)
  }
  x
}

#' Normalize an dataset.
#' 
#' @param x a data.frame.
#' @param collapse_on which variable should be collapsed on? 
#' @param collapse_name the variable name of the collapsed sub-data.frames.
#' @param remove_equiv_columns should equivalent columns be removed?
#' @param keep_cols column names that should be kept.
#' @param verbose should information about dropped columns be printed? 
#' (default FALSE)
#' @return The collapsed data.frame with numerically encoded columens removed.
#' @importFrom dplyr %>% mutate_if mutate_at
#' @importFrom crayon green
#' @export
normalize <- function(x, collapse_on, collapse_name, 
  remove_equiv_columns = TRUE, keep_cols = character(), verbose = FALSE) {

  . <- NULL
  if (missing(collapse_name)) {
    collapse_name <- as.character(as.list(match.call())$x)
  }
  to_factor <- function(x) {
    col_types <- sapply(x, class)
    colnames(x)[colnames(x) != collapse_on & col_types == "character"]
  }
  if (remove_equiv_columns) {
    x %>%
      tcat("Removing equivalent columns.\n", verbose = verbose,
           style = green) %>%
      remove_equiv_columns(verbose = verbose, keep_cols = keep_cols)
  }
 
  if (verbose) { 
    cat(green("Mutating character columns to factors.\n"))
    ma <- to_factor(x)
    if (length(ma) == 0) {
      cat(italic("No variables to turn into factor.\n"))
    } else {
      cat(
        italic("\tThe following variables will be turned into factors:\n\t\t"))
      cat(italic(paste(ma, collapse = "\n\t\t")))
      cat("\n")
    }
  }

  x %>%
    mutate_at(to_factor(.), as.factor) %>%
    tcat("Collapsing rows.\n", verbose = verbose, style = green) %>%
    collapse_rows(collapse_on, {{collapse_name}}) 
}

#' Find Variables Appearing Multiple Data Sets
#'
#' @param x the list of data.frames.
#' @param collapse_on the collapse variable.
#' @param x_names the names of the data sets. (Default names(x))
#' @importFrom dplyr full_join distinct
#' @importFrom crayon red
#' @importFrom equivalent equiv
#' @export
repeat_vars <- function(x, collapse_on, x_names = names(x)) {
  if (is.null(x_names)) {
    stop(red("The supplied list must have names."))
  }
  dup_violations <- list()
  if (length(x) > 1) {
    dup_names <- setdiff(dup_vars(x), collapse_on)
    for (dn in dup_names) {
      dup_inds <- which(unlist(lapply(x, function(d) dn %in% colnames(d))))
      dup_ret <- as_tibble(x[[dup_inds[1]]][,c(collapse_on, dn)])
      colnames(dup_ret)[2] <- x_names[dup_inds[1]]
      dup_ret <- dup_ret[!duplicated(dup_ret[[collapse_on]]),]
      for (di in dup_inds[-1]) {
        nd <- x[[di]][, c(collapse_on, dn)]
        colnames(nd)[2] <- x_names[di]
        dup_ret <- full_join(dup_ret, nd[!duplicated(nd[[collapse_on]]),], 
                             by = collapse_on)
      }
      dup_ret$var <- dn
      dup_ret <- dup_ret[, c(1, ncol(dup_ret), 
                             setdiff(seq_len(ncol(dup_ret)), 
                                     c(1, ncol(dup_ret))))]
      dup_violations <- c(dup_violations, list(dup_ret))
    }
    names(dup_violations) <- dup_names
  }
  dup_violations
}

#' Variables Duplicated Across Data Sets
#'
#' @param x the list of data.frames.
#' @export
dup_vars <- function(x) {
  all_names <- unlist(lapply(x, colnames))
  unique(all_names[duplicated(all_names)])
}

#' @importFrom equivalent has_equiv_column
#' @importFrom crayon yellow
#' @importFrom stats na.omit
handle_repeated_vars <- function(arg_list, rvs, collapse_on) {
  new_arg <- NULL
  for (rv in rvs) {
    if (isTRUE(all(has_equiv_column(rv)[-(1:3)]))) {
      new_cols <- rv[,c(1, 3)]
      colnames(new_cols)[2] <- rv[[2]][1]
    } else {
      warning(yellow("No equivalence found for variable", rv$var[1], 
                     "using most complete data set."))
      na_counts <- unlist(lapply(rv, function(x) sum(is.na(x))))
      min_na_val <- min(na_counts[-(1:2)])
      rv <- rv[, na_counts <= min_na_val]
      if (isTRUE(all(has_equiv_column(rv)[-(1:3)]))) {
        new_cols <- rv[,c(1, 3)]
        colnames(new_cols)[2] <- rv[[2]][1]
      } else {
        stop(red("Contradictions in repeated variables.\n", 
                 "  You need to manually fix variable: ", rv[[2]][1], 
                 "\n  It appears in data sets:\n\t",
                 paste(names(na_counts)[-(1:2)], collapse = "\n\t"),
                 "\n", sep = ""))
      }
    }
    if (is.null(new_arg)) {
      new_arg <- new_cols
    } else {
      new_arg <- full_join(new_arg, new_cols, by = collapse_on)
    }
  }
  for (i in seq_along(arg_list)) {
    rem_inds <- na.omit(match(names(rvs), colnames(arg_list[[i]])))
    if (length(rem_inds) > 0) {
      arg_list[[i]] <- arg_list[[i]][, -rem_inds]
    }
  }
  c(arg_list, list(new_arg))
}

#' Create a Set of data.frame with Contradictions.
#'
#' @param x the list of ADaM data sets.
#' @param collapse_on which variable should be collapsed on? 
#' @importFrom dplyr full_join distinct 
#' @importFrom foreach foreach %dopar% %do%
#' @importFrom crayon yellow
#' @export
contradiction_tibbles <- function(x, collapse_on) {
  i <- j <- NULL

  cvs <- contradicting_vars(x, collapse_on)
  contras <- foreach(i = seq_along(cvs)) %dopar% {
    rs <- foreach (j = seq_along(cvs[[i]])) %do% {
      ds <- distinct(x[cvs[[i]]][[j]][, c(collapse_on, names(cvs)[i])])
      if (isTRUE(any(duplicated(ds[[collapse_on]])))) {
        warning(yellow("Removing repeated values in variable name: ", 
                       names(cvs)[i], " data set: ", cvs[[i]][j], sep = ""))
        ds <- ds[!duplicated(ds[[collapse_on]]),]
      }
      names(ds)[2] <- cvs[[i]][j]
      ds
    }
    Reduce(function(x, y) full_join(x, y, by = collapse_on), rs)
  }
  names(contras) <- names(cvs)
  contras
}

#' Find the Data Sets with Conflicting Columns
#' 
#' @param x the list of ADaM data sets.
#' @param collapse_on which variable should be collapsed on? 
#' @importFrom tibble as_tibble
#' @importFrom foreach foreach %dopar% registerDoSEQ getDoParName
#' @export
contradicting_vars <- function(x, collapse_on) {
  rv <- NULL

  rvs <- repeat_vars(x, collapse_on = collapse_on)
  if (is.null(getDoParName())) {
    registerDoSEQ()
  }
  ret <- foreach(rv = rvs) %dopar% {
#lapply(rvs, function(rv) {
    r <- c()
    if (!isTRUE(all(has_equiv_column(rv)[-(1:3)]))) {
      colnames(rv)[-(1:2)]
    }
  }
#)
  names(ret) <- names(rvs)
  ret[unlist(lapply(ret, function(x) length(x) > 0))]
}

#' Consolidate multiple data sets
#'
#' @param ... a set of ADaM formatted data.frames.
#' @param collapse_on which variable should be collapsed on? 
#' @param verbose should extra information be provided? (Default: TRUE)
#' @return A single data.frame composed of the collapsed and merged input
#' data.frames.
#' @importFrom dplyr full_join
#' @importFrom crayon red
#' @export
consolidate <- function(..., collapse_on, verbose = FALSE) {
  # Get the set of data sets.
  arg_list <- as.list(...)

  # Make sure we have an on variable in each data set.
  name_check <- vapply(arg_list, function(x) collapse_on %in% names(x), FALSE)
  if (!isTRUE(all(name_check))) {
    stop(red(
      paste("Join variable missing in data set", which(name_check != TRUE))))
  }

  # Make sure if a variable appears in more than one data set it is the 
  # same in each data set.
  rvs <- repeat_vars(arg_list, collapse_on = collapse_on)
  if (length(rvs) > 0) {
    if (verbose) {
      cat(italic("\tHandling repeated variables."))
    }
    arg_list <- handle_repeated_vars(arg_list, rvs, collapse_on)
  }  

  col_names <- list(colnames(arg_list[[1]]))
  all_col_names <- unique(unlist(lapply(arg_list, colnames)))
  all_col_names <- setdiff(all_col_names, colnames(arg_list[[1]]))
  for (i in 2:length(arg_list)) {
    keep_col_names <- intersect(colnames(arg_list[[i]]), all_col_names)
    col_names <- c(col_names, list(c(collapse_on, keep_col_names)))
    all_col_names <- setdiff(all_col_names, keep_col_names)
  }
  keep <- vapply(col_names, function(x) length(x) > 1, FALSE)
  col_names <- col_names[keep]
  arg_list <- arg_list[keep]
  ret <- arg_list[[1]][,col_names[[1]]]
  for (i in 2:length(arg_list)) {
    ret <- full_join(ret, arg_list[[i]][,col_names[[i]]], by = collapse_on)
  }
  ret
}

