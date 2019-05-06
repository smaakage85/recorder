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

#' Concatenate Validation Test Failure Descriptions
#'
#' Concatenates validation test failure descriptions to a single character 
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
  
  cat("> '", 
      name,
      "': ",
      paste_colnames(x$tests[[name]]), 
      "\n",
      sep = "")
  
  # return invisibly.
  invisible()
  
}

print_tests_rowlevel <- function(x, name, first = 10) {
  
  cat("> '", 
      name,
      "': ",
      paste_all_cols_with_rows(x$tests[[name]], first = first), 
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
  paste0(single_cols, collapse = ", ")

}

paste_col_with_rows <- function(name, x, first = 10) {
  x <- which(x)
  exceed_length <- max(length(x) - first)
  paste0(name, "[rows: ", paste0(x[seq_len(min(first, length(x)))],
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
      get("tests_meta_data")[[name]]$description, 
      "\n",
      sep = "")
  
  # return invisibly.
  invisible()
  
}

#### get_test_descriptions ####

#' Get Descriptions of Validation Tests
#'
#' @return \code{data.frame} test descriptions.
#' @export
#'
#' @examples
#' get_test_descriptions()
get_test_descriptions <- function() {
  descriptions <- lapply(get("tests_meta_data"), '[[', "description")
  descriptions_df <- as.data.frame(as.matrix(descriptions))
  names(descriptions_df) <- "Description"
  descriptions_df
}
