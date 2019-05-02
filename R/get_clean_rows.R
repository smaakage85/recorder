#' Get Violations
#'
#' @param playback \code{data.playback} to generate violation matrix from.
#' @inheritParams ignore
#'
#' @return \code{data.table} with logicals for all of the checks, that one or more
#' rows failed to pass. A failed check for any given row is equivalent to a value
#' of TRUE. If all checks passed, the function will simply return a matrix with
#' one column, 'any_violations', that is always FALSE, to ensure that the output
#' is stable and consistent.
#' 
#' @export
get_violations <- function(playback,
                           ignore_check_names = NULL,
                           ignore_cols = NULL,
                           ignore_combinations = NULL) {
  
  # validate input.
  if (!inherits(playback, "data.playback")) {
    stop("'playback' must belong to the 'data.playback' class.")
  }
  
  # ignore certain checks.
  checks <- ignore(playback$checks,
                   playback$variables,
                   ignore_check_names = ignore_check_names,
                   ignore_cols = ignore_cols,
                   ignore_combinations = ignore_combinations)
  
  # compute violation matrix.
  cm <- check_matrix(checks)
  
  # return any_violations = FALSE, if there are no violations.
  if (nrow(cm) == 0) {
    return(data.table(any_violations = rep(FALSE, playback$duration)))
  } else {cm}
  
}

#' Get Violations as a String
#'
#' @inheritParams get_violations
#'
#' @return \code{character} with one entry for each row in new data. Each
#' entry concatenates information of the checks, that did NOT pass for the
#' corresponding row in new data.
#' 
#' @details 
#' - 'mismatch_levels': (only relevant for factors and characters).
#' - 'missing_variable': the variable was recorded on training data but not 
#' observed in new data.
#' @export
get_violations_string <- function(playback,
                                  ignore_check_names = c("new_variable"),
                                  ignore_cols = NULL,
                                  ignore_combinations = list(mismatch_class = NULL))  {
  
  # get violation matrix.
  cm <- get_violations(playback = playback,
                       ignore_check_names = ignore_check_names,
                       ignore_cols = ignore_cols,
                       ignore_combinations = ignore_combinations)
  
  # handle case, where all rows passed all checks.
  if (identical(names(cm), "any_violations")) {
    return(rep("", playback$duration))
  } else {
    write_violations(cm)
  }
  
}

#' Get Clean Rows
#'
#' @inheritParams get_violations
#'
#' @return \code{logical} with the same length as the number of rows in new 
#' data. The value is TRUE, if the row passed all checks, otherwise FALSE.
#' 
#' @export
get_clean_rows <- function(playback,
                           ignore_check_names = NULL,
                           ignore_cols = NULL,
                           ignore_combinations = NULL) {

  # get violation matrix.
  cm <- get_violations(playback = playback,
                       ignore_check_names = ignore_check_names,
                       ignore_cols = ignore_cols,
                       ignore_combinations = ignore_combinations)
  
  # compute indicator, that tells whether a row is 'clean' or not. 
  rowSums(cm) == 0
  
}