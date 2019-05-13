#### get_clean_rows() ####

#' Create Data Frame with Test Results
#'
#' @param x \code{list} results of tests.
#'
#' @import data.table
#'
#' @return \code{data.table} with test results as columns.
create_test_results_df <- function(x) {

  # convert tests to data.tables and bind them.
  dts <- lapply(x, as.data.table)

  # subset elements with # rows > 0.
  dts <- dts[vapply(dts, function(x) {length(x) > 0}, FUN.VALUE = logical(1))]

  # bind columns of all data.tables to just one data.table.
  do.call(cbind, dts)

}

#' Concatenate Validation Test Failures Descriptions
#'
#' Concatenates validation test failures descriptions to a single character
#' vector.
#'
#' @param test_failures \code{data.frame} with test results as columns.
#'
#' @return \code{character} concatenated descriptions of test failures with
#' one string pr. row.
concatenate_test_failures <- function(test_failures) {

  # create test failures matrix with colnames as entries.
  tfm  <- matrix(data  = t(rep(x = paste0(colnames(test_failures), ";"),
                               times = nrow(test_failures))),
                 ncol  = ncol(test_failures),
                 byrow = TRUE)

  # replace FALSE with empty string.
  tfm[test_failures == FALSE] <- ""

  # concatenate failures to one string pr. row.
  do.call(what = paste0, args = data.frame(tfm))

}

#### play() ####

#' Compress Results of Detailed Tests
#'
#' Subsets results of the tests, where at least one row failed.
#'
#' @param dt \code{list} results of detailed tests.
#'
#' @return \code{list} with test failures.
compress_detailed_tests <- function(dt) {

  # order list by test names (in stead of columns).
  dt <- order_by_tests(dt)

  # compress tests.
  lapply(dt, compress_tests)

  }

#' Order Test Results by Test Names
#'
#' @param dt \code{list} test results.
#'
#' @return \code{list} test results ordered by test names.
order_by_tests <- function(dt) {

  # extract unique test names.
  tests <- unique(do.call(c, lapply(dt, names)))

  # extract tests.
  by_tests <- lapply(tests, function (x) {lapply(dt, '[[', x)})

  # set names.
  names(by_tests) <- tests

  by_tests

  }

compress_tests <- function(x) {
  x[vapply(x, any, logical(1))]
}

#### print.data.playback() ####

paste_colnames <- function(x) {
  if (length(x) > 0) {
    paste0(names(x), collapse = ", ")
  } else {"no failures"}
}

print_tests_collevel <- function(x, name) {

  results <- paste_colnames(x$tests[[name]])
  if (results == "no failures") {
    results <- green(results)
  } else {
    results <- red(results)
  }

  cat("> '",
      name,
      "': ",
      results,
      "\n",
      sep = "")

  # return invisibly.
  invisible()

}

print_tests_rowlevel <- function(x, name, first = 10) {

  results <- paste_all_cols_with_rows(x$tests[[name]], first = first)
  if (results == "no failures") {
    results <- green(results)
  } else {
    results <- red(results)
  }

  cat("> '",
      name,
      "': ",
      results,
      "\n",
      sep = "")

  # return invisibly.
  invisible()

}

paste_all_cols_with_rows <- function(x, first = 10) {

  if (length(x) == 0) {
    return("no failures")
  }

  single_cols <- mapply(paste_col_with_rows, names(x), x, first = first, SIMPLIFY = FALSE)
  # paste results for all columns to one string.
  paste0(single_cols, collapse = ",\n")

}

paste_col_with_rows <- function(name, x, first = 10) {
  x <- which(x)
  exceed_length <- max(length(x) - first)
  paste0(name, "[row(s): ", paste0(paste("#", x[seq_len(min(first, length(x)))], sep = ""),
                                   collapse = ", "),
         if (exceed_length > 0) {
           paste0(" and ", exceed_length, " more rows")
         } else {""},
         "]")
}

print_test_description <- function(pb, name) {

  cat("'",
      name,
      "': ",
      create_tests_meta_data()[[name]]$description,
      "\n",
      sep = "")

  # return invisibly.
  invisible()

}

#### get_tests_meta_data ####

#' Get Meta Data of Validation Tests in a Data Frame
#'
#' Gets meta data of available validation tests as a data.frame.
#'
#' @details The meta data of a validation test consists of:
#'
#' \describe{
#'   \item{test_name}{name of the test}
#'   \item{evaluate_level}{is the test evaluated on column level (`col`) or on
#'   row level (`row`)?}
#'   \item{evaluate_class}{what classes of variables are being tested with this
#'   specific test?}
#'   \item{description}{a short description of what a test failure means for
#'   the given test}
#' }
#'
#' @return \code{data.frame} meta data of validation tests.
#' @export
#'
#' @examples
#' get_tests_meta_data()
get_tests_meta_data <- function() {
  # get meta data of tests in list form.
  tests_meta_data <- create_tests_meta_data()
  # get test names.
  dt <- data.table(test_name = names(tests_meta_data))
  # combine with meta data.
  cbind(dt, rbindlist(tests_meta_data))
}

#' Create Meta Data of Validation Tests
#'
#' Creates meta data of available validation tests as a list. The list has as
#' many elements as the number of available validation test - one for each test.
#' Entries are named after the different tests.
#'
#' @details The meta data of a validation test consists of:
#'
#' \describe{
#'   \item{evaluate_level}{is the test evaluated on column level (`col`) or on
#'   row level (`row`)?}
#'   \item{evaluate_class}{what classes of variables are being tested with this
#'   specific test?}
#'   \item{description}{a short description of what a test failure means for
#'   the given test}
#' }
#'
#' @return \code{list} meta data of validation tests.
#'
#' @export
#'
#' @examples
#' create_tests_meta_data()
create_tests_meta_data <- function() {
  list(
    missing_variable = list(evaluate_level = "col",
                            evaluate_class = "all",
                            description = "variable observed in training data but missing in new data"),
    mismatch_class = list(evaluate_level = "col",
                          evaluate_class = "all",
                          description = "'class' in new data does not match 'class' in training data"),
    mismatch_levels = list(evaluate_level = "col",
                           evaluate_class = "factor",
                           description = "'levels' in new data and training data are not identical"),
    new_variable = list(evaluate_level = "col",
                        evaluate_class = "all",
                        description = "variable observed in new data but not in training data"),
    outside_range = list(evaluate_level = "row",
                         evaluate_class = "numeric, integer",
                         description = "value in new data outside recorded range in training data"),
    new_level = list(evaluate_level = "row",
                     evaluate_class = "factor",
                     description = "new 'level' in new data compared to training data"),
    new_NA = list(evaluate_level = "row",
                  evaluate_class = "all",
                  description = "NA observed in new data but not in training data"),
    new_text = list(evaluate_level = "row",
                    evaluate_class = "character",
                    description = "new text in new data compared to training data")
  )
}
