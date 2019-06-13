#' Get Failed Tests
#'
#' @param playback \code{data.playback} to extract failed tests from.
#' @inheritParams ignore
#'
#' @return \code{data.table} with test results as logicals for all of the tests
#' with at least one failure. A failed test for any given row is 
#' equivalent to a value of TRUE. If all tests passed, the function will simply 
#' return a data.table with one column, 'any_failures', that is always FALSE, 
#' to ensure that the output is (type) stable and consistent.
#' 
#' @export
#' 
#' @examples
#' # record tape from `iris`.
#' tape <- record(iris)
#' # load data.
#' data(iris_newdata)
#' # validate new data by playing new tape on it.
#' playback <- play(tape, iris_newdata)
#' 
#' get_failed_tests(playback)
#' get_failed_tests(playback, ignore_tests = "outside_range")
#' get_failed_tests(playback, ignore_cols = "junk")
#' get_failed_tests(playback, ignore_combinations = list(outside_range = "Sepal.Width"))
get_failed_tests <- function(playback,
                             ignore_tests = NULL,
                             ignore_cols = NULL,
                             ignore_combinations = NULL) {
  
  # validate input.
  if (!inherits(playback, "data.playback")) {
    stop("'playback' must belong to the 'data.playback' class.")
  }
  
  # if there are no failed tests, there are no failures to ignore, return 
  # simple output.
  if (all(vapply(playback$tests, length, FUN.VALUE = integer(1)) == 0)) {
    return(data.table(any_failures = rep(FALSE, playback$nrow_newdata))) 
  }
  
  # ignore certain tests.
  tests <- ignore(playback$tests,
                  playback$variables,
                  ignore_tests = ignore_tests,
                  ignore_cols = ignore_cols,
                  ignore_combinations = ignore_combinations)
  
  # create data.frame with test results.
  test_results_df <- create_test_results_df(tests)
  
  # handle special cases:
  # => return any_failures = FALSE, if there are no test failures, nrow = 0 or
  # NULL return.
  if (is.null(test_results_df) || nrow(test_results_df) == 0) {
    test_results_df <- data.table(any_failures = rep(FALSE, playback$nrow_newdata))
  } else if (nrow(test_results_df) == 1) {
    # => handle case, where _only_ tests on column level have failed, nrow = 1.
    test_results_df <- test_results_df[rep(1, playback$nrow_newdata), ]
  } 
  
  test_results_df
  
}

#' Get Failed Tests as a String
#'
#' Concatenates information of the tests that failed into one single
#' character vector.
#' 
#' @inheritParams get_failed_tests
#'
#' @return \code{character} with one entry for each row in new data. Each
#' entry concatenates information of the tests, that did NOT pass for the
#' corresponding row in new data.
#' 
#' @details Look up the descriptions and other meta data of the available 
#' validation tests with \code{\link{get_tests_meta_data}}.
#' 
#' @export
#' 
#' @examples
#' # record tape from `iris`.
#' tape <- record(iris)
#' # load data.
#' data(iris_newdata)
#' # validate new data by playing new tape on it.
#' playback <- play(tape, iris_newdata)
#' 
#' get_failed_tests_string(playback)
#' get_failed_tests_string(playback, ignore_tests = "outside_range")
#' get_failed_tests_string(playback, ignore_cols = "junk")
#' get_failed_tests_string(playback, ignore_combinations = list(outside_range = "Sepal.Width"))
get_failed_tests_string <- function(playback,
                                    ignore_tests = NULL,
                                    ignore_cols = NULL,
                                    ignore_combinations = NULL)  {
  
  # create data.frame with failed tests.
  df_ft <- get_failed_tests(playback = playback,
                            ignore_tests = ignore_tests,
                            ignore_cols = ignore_cols,
                            ignore_combinations = ignore_combinations)
  
  # handle (special) case, where all rows passed all tests.
  if (identical(names(df_ft), "any_failures")) {
    return(rep("", playback$nrow_newdata))
  } else {
    concatenate_test_failures(df_ft)
  }
  
}

#' Get Clean Rows
#'
#' @inheritParams get_failed_tests
#'
#' @details Look up the descriptions and other meta data of the available 
#' validation tests with \code{\link{get_tests_meta_data}}.
#' 
#' @return \code{logical} with the same length as the number of rows in new 
#' data. The value is TRUE, if the row passed all tests, otherwise FALSE.
#' 
#' @export
#' 
#' @examples
#' # record tape from `iris`.
#' tape <- record(iris)
#' # load data.
#' data(iris_newdata)
#' # validate new data by playing new tape on it.
#' playback <- play(tape, iris_newdata)
#' 
#' get_clean_rows(playback)
#' get_clean_rows(playback, ignore_tests = "outside_range")
#' get_clean_rows(playback, ignore_cols = "junk")
#' get_clean_rows(playback, ignore_combinations = list(outside_range = "Sepal.Width"))
get_clean_rows <- function(playback,
                           ignore_tests = NULL,
                           ignore_cols = NULL,
                           ignore_combinations = NULL) {
  
  # get failed tests.
  df_ft <- get_failed_tests(playback = playback,
                            ignore_tests = ignore_tests,
                            ignore_cols = ignore_cols,
                            ignore_combinations = ignore_combinations)
  
  # compute indicator, that tells whether a row is 'clean' or not. 
  rowSums(df_ft) == 0
  
}
