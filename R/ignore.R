#' Ignore Certain Test Results
#'
#' Ignores certain test results in accordance with user inputs.
#'
#' @param tests \code{list} test results.
#' @param variables_newdata \code{character} names of variables in new data.
#' @param ignore_tests \code{character} ignore test results from tests with 
#' these names. 
#' @param ignore_cols \code{character} ignore test results from tests of 
#' columns with these names.
#' @param ignore_combinations \code{list} ignore test results from specific 
#' tests of specific columns.
#'
#' @details Look up the descriptions and other meta data of the available
#' validation tests with \code{\link{get_tests_meta_data}}.
#'
#' @return \code{list} only the relevant test results.
ignore <- function(tests,
                   variables_newdata,
                   ignore_tests = NULL,
                   ignore_cols = NULL,
                   ignore_combinations = NULL) {

  # ignore test results from _specific tests_.
  tests <- ignore_tests(tests = tests,
                        test_names = ignore_tests)

  # ignore test results from tests of _specific columns_.
  tests <- ignore_cols(tests = tests,
                       col_names = ignore_cols,
                       variables_newdata = variables_newdata)

  # ignore test results from _specific tests of specific columns_.
  tests <- ignore_combinations(tests = tests,
                               combinations = ignore_combinations,
                               variables_newdata = variables_newdata)

  # return tests after removals.
  tests

}

#' Ignore Results from Specific Tests
#'
#' @param tests \code{list} test results.
#' @param test_names \code{character} names of tests to be ignored.
#'
#' @return \code{list} results after removing specific tests.
ignore_tests <- function(tests, test_names = NULL) {

  # if NULL, do nothing - just return 'as is'.
  if (is.null(test_names)) return(tests)

  # validate input.
  if (!is.character(test_names) && length(test_names) == 0) {
    stop("'test_names' must be a character vector with positive length (or NULL).")
  }

  # do test names exist?
  tests_meta_data <- create_tests_meta_data()
  if (!all(test_names %in% names(tests_meta_data))) {
    warning("The following tests do not exist: ",
         paste0(test_names[!test_names %in% names(tests_meta_data)],
                collapse = ","), "\n")
  }

  # subset only tests, that are not to be ignored.
  subset_tests <- names(tests)[!names(tests) %in% test_names]
  tests[subset_tests]

}

#' Ignore Test Results from Tests of Specific Columns
#'
#' @param tests \code{list} test results.
#' @param col_names \code{character} names of columns for which test results
#' should be ignored.
#' @param variables_newdata \code{character} names of variables in new data.
#'
#' @return \code{list} results after removing tests.
ignore_cols <- function(tests, col_names, variables_newdata) {

  # if NULL, do nothing - just return 'as is'.
  if (is.null(col_names)) return(tests)

  # validate input.
  if (!is.character(col_names) && length(col_names) == 0) {
    stop("'col_names' must be a character vector with positive length (or NULL).")
  }

  # do the variables/columns exist in new data?
  if (!all(col_names %in% variables_newdata)) {
    warning("The following columns do not exist in new data: ",
         paste0(col_names[!col_names %in% variables_newdata], collapse = ", "),
         "\n")
  }

  # subset only columns, that are not to be ignored.
  lapply(tests, function(x) {
    x[col_names] <- NULL
    x})

}

#' Ignore Test Results from Specific Tests of Specific Columns
#'
#' @param tests \code{list} test results.
#' @param combinations \code{list} combinations of tests and columns from which
#' test results should be ignored.
#' @param variables_newdata \code{character} names of variables in new data.
#'
#' @return \code{list} test results after removals.
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

  if (!all(vapply(combinations, is.character, FUN.VALUE = logical(1)) == TRUE)) {
    "Please supply column names must as characters."
  }

  # do test names exist?
  tests_meta_data <- create_tests_meta_data()
  if (!all(names(combinations) %in% names(tests_meta_data))) {
    warning("The following tests do not exist: ",
            paste0(names(combinations)[!names(combinations) %in% names(tests_meta_data)],
                   collapse = ","),
            "\n")
  }

  # are there any incomplete combinations, where no columns have been selected?
  incomplete_combinations <- vapply(combinations,
                                    length,
                                    FUN.VALUE = integer(1)) == 0
  if (any(incomplete_combinations)) {
    stop("The following combinations were not complete (no columns selected): ",
         names(combinations)[which(incomplete_combinations)])
  }

  # do selected columns exist in new data?
  col_names <- unique(do.call(c, combinations))
  if (length(col_names) > 0 && !all(col_names %in% variables_newdata)) {
    warning("The following columns do not exist in new data: ",
            paste0(col_names[!col_names %in% variables_newdata], collapse = ", "),
            "\n")
  }

  # subset only relevant combinations of tests and columns, that should not
  # be ignored.
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

