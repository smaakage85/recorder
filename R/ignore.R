#' Ignore and remove certain tests
#'
#' @param tests \code{list} with computed tests.
#' @param variables_newdata \code{character} names of variables in new data.
#' @param ignore_tests \code{character} with names of tests to ignore. 
#' @param ignore_cols \code{character} with names of columns/variables to ignore.
#' @param ignore_combinations \code{list} with combinations of tests and
#' columns to ignore.
#'
#' @return \code{list} with only the relevant tests.
ignore <- function(tests,
                   variables_newdata,
                   ignore_tests = NULL,
                   ignore_cols = NULL,
                   ignore_combinations = NULL) {

  # ignore _certain tests_.
  tests <- ignore_tests(tests = tests,
                          test_names = ignore_tests)

  # ignore tests of _certain columns_.
  tests <- ignore_cols(tests = tests,
                        col_names = ignore_cols,
                        variables_newdata = variables_newdata)

  # ignore _certain combinations of tests and columns_.
  tests <- ignore_combinations(tests = tests,
                                combinations = ignore_combinations,
                                variables_newdata = variables_newdata)

  # return tests after removing certain tests.
  tests

}

ignore_tests <- function(tests, test_names = c("new_variable")) {

  # if NULL, do nothing - just return 'as is'.
  if (is.null(test_names)) return(tests)

  # validate input.
  if (!is.character(test_names) && length(test_names) == 0) {
    stop("'test_names' must be a character vector with positive length (or NULL).")
  }

  # do test names exist?
  if (!all(test_names %in% names(tests))) {
    stop("The following test names do not exist, please check:",
         paste0(test_names[!test_names %in% names(tests)], collapse = ","),
         "\n")
  }

  # subset only tests, that are not to be ignored.
  subset_cols <- names(tests)[!names(tests) %in% test_names]
  tests[subset_cols]

}

ignore_cols <- function(tests, col_names, variables_newdata) {

  # if NULL, do nothing - just return 'as is'.
  if (is.null(col_names)) return(tests)

  # validate input.
  if (!is.character(col_names) && length(col_names) == 0) {
    stop("'col_names' must be a character vector with positive length (or NULL).")
  }

  # does any of these cols have any violations at all?
  if (!all(col_names %in% variables_newdata)) {
    message("The following columns do not exist in training data, please check: ",
         paste0(col_names[!col_names %in% names(variables_newdata)], collapse = ","),
         "\n")
  }

  # subset only columns, that are not to be ignored.
  lapply(tests, function(x) {
    x[col_names] <- NULL
    x})

}

ignore_combinations <- function(tests, combinations, variables_newdata) {

  # if NULL, do nothing - just return 'as is'.
  if (is.null(combinations)) return(tests)

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

  # do columns exist in new data?
  col_names <- unique(do.call(c, lapply(combinations, names)))
  if (length(col_names) > 0 && !all(col_names %in% names(variables_newdata))) {
    stop("The following columns do not exist in new data, please check: ",
         paste0(col_names[!col_names %in% variables_newdata], collapse = ","),
         "\n")
  }

  # subset only relevant combinations of tests and columns, that are not to be ignored.
  tests[names(combinations)] <-
    mapply(FUN = function(tests, col_names) {
      tests[col_names] <- NULL
      tests},
      tests = tests[names(combinations)],
      col_names = combinations,
      SIMPLIFY = FALSE)

  # return only relevant subset of tests.
  tests

}

