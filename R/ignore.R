#' Ignore and remove certain checks
#'
#' @param checks \code{list} with computed checks.
#' @param variables_newdata \code{character} names of variables in new data.
#' @param ignore_check_names \code{character} with names of checks to ignore. 
#' @param ignore_cols \code{character} with names of columns/variables to ignore.
#' @param ignore_combinations \code{list} with combinations of checks and
#' columns to ignore.
#'
#' @return \code{list} with only the relevant checks.
ignore <- function(checks,
                   variables_newdata,
                   ignore_check_names = NULL,
                   ignore_cols = NULL,
                   ignore_combinations = NULL) {

  # ignore _certain checks_.
  checks <- ignore_checks(checks = checks,
                          check_names = ignore_check_names)

  # ignore checks of _certain columns_.
  checks <- ignore_cols(checks = checks,
                        col_names = ignore_cols,
                        variables_newdata = variables_newdata)

  # ignore _certain combinations of checks and colums_.
  checks <- ignore_combinations(checks = checks,
                                combinations = ignore_combinations,
                                variables_newdata = variables_newdata)

  # return checks after removing certain checks.
  checks

}

ignore_checks <- function(checks, check_names = c("new_variable")) {

  # if NULL, do nothing - just return 'as is'.
  if (is.null(check_names)) return(checks)

  # validate input.
  if (!is.character(check_names) && length(check_names) == 0) {
    stop("'check_names' must be a character vector with positive length (or NULL).")
  }

  # do check names exist?
  if (!all(check_names %in% names(checks))) {
    stop("The following check names do not exist, please check:",
         paste0(check_names[!check_names %in% names(checks)], collapse = ","),
         "\n")
  }

  # subset only checks, that are not to be ignored.
  subset_cols <- names(checks)[!names(checks) %in% check_names]
  checks[subset_cols]

}

ignore_cols <- function(checks, col_names, variables_newdata) {

  # if NULL, do nothing - just return 'as is'.
  if (is.null(col_names)) return(checks)

  # validate input.
  if (!is.character(col_names) && length(col_names) == 0) {
    stop("'col_names' must be a character vector with positive length (or NULL).")
  }

  # does any of these cols have any violations at all?
  if (!all(col_names %in% variables_newdata)) {
    message("The following columns do not exist in training data, please check: ",
         paste0(col_names[!col_names %in% names(tape$class_variables)], collapse = ","),
         "\n")
  }

  # subset only columns, that are not to be ignored.
  lapply(checks, function(x) {
    x[col_names] <- NULL
    x})

}

ignore_combinations <- function(checks, combinations, variables_newdata) {

  # if NULL, do nothing - just return 'as is'.
  if (is.null(combinations)) return(checks)

  # validate input.
  if (!is.list(combinations) && (length(combinations) == 0 ||
                                 is.null(names(combinations)))) {
    stop("'combinations' must be a named list with positive length.")
  }

  
  if (length(names(combinations)) > length(unique(names(combinations)))) {
    stop("Names of 'combinations' list must be unique.")
  }
  
  # are there any incomplete combinations, where no columns have been selected?
  incomplete_combinations <- vapply(combinations, 
                                    length, 
                                    FUN.VALUE = integer(1)) == 0
  if (any(incomplete_combinations)) {
    stop("The following combinations were not complete (no columns selected): ",
         names(combinations)[which(incomplete_combinations)])
  }

  # 
  if (!all(col_names %in% names(variables_newdata))) {
    stop("The following columns do not exist in new data, please check: ",
         paste0(col_names[!col_names %in% variables_newdata], collapse = ","),
         "\n")
  }

  # subset only relevant combinations of checks and columns, that are not to be ignored.
  checks[names(combinations)] <-
    mapply(FUN = function(checks, col_names) {
      checks[col_names] <- NULL
      checks},
      checks = checks[names(combinations)],
      col_names = combinations,
      SIMPLIFY = FALSE)

  # return only relevant subset of checks.
  checks

}

