#' Meta Data of Validation Tests
#'
#' A dataset containing relevant information of the validation tests.
#'
#' @format A list with 7 entries, one for each validation test. Entries are 
#' named after the different tests. 
#' 
#' @details For each validation test there are three
#' pieces of information available:
#' 
#' \describe{
#'   \item{evaluate_level}{is the test evaluated on column level (`col`) or on
#'   row level (`row`)?}
#'   \item{evaluate_class}{what classes of variables are being tested with this
#'   specific test?}
#'   \item{description}{a short description of what a test failure means for
#'   the given test}
#' }
#' @source Script attached.
"tests_meta_data"

#' Simulated Iris New Data
#'
#' A mutated version of the famous `iris` data set.
#'
#' @format A data.frame with 150 rows and 5 columns.
#' 
#' @source Script attached.
"iris_newdata"