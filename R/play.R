#' Run Validation Tests on Variable in New Data
#' 
#' Runs a set of validation tests on a variable in new data. These tests are 
#' based on parameters recorded (with \code{\link{record}}) from the training 
#' data.
#' 
#' @param x variable in new data.
#' @param ... further arguments passed to or from other methods. Not used at
#' the moment.
#' @param parameters \code{list} parameters and meta data of the same variable
#' recorded from training data (with \code{\link{record}}). 
#'
#' @return \code{list} results from validations tests. 
#' 
#' @export
run_validation_tests <- function (x, parameters, ...) {
  UseMethod("run_validation_tests", x)
}

#' Run Validation Tests on Numeric
#'
#' Runs a set of validation tests on a \code{numeric} in new data. These tests
#' are based on parameters recorded (with \code{\link{record}}) from the 
#' training data.
#' 
#' @param x \code{numeric} in new data.
#' @param ... further arguments passed to or from other methods. Not used at
#' the moment.
#' @param parameters \code{list} parameters and meta data of the same variable
#' recorded from training data (with \code{\link{record}}).  
#'
#' @method run_validations_tests numeric
#' 
#' @export
#' 
#' @return \code{list} results from validations tests.
run_validation_tests.numeric <- function(x, parameters, ...) {

  # run tests.
  list(
    outside_range = !is.na(x) & (x < parameters$min | x > parameters$max),
    new_NA = !parameters$any_NA & is.na(x)
  )

}

#' Run Validation Tests on Character
#'
#' Runs a set of validation tests on a \code{character} in new data. These tests
#' are based on parameters recorded (with \code{\link{record}}) from the 
#' training data.
#' 
#' @param x \code{character} in new data.
#' @param ... further arguments passed to or from other methods. Not used at
#' the moment.
#' @param parameters \code{list} parameters and meta data of the same variable
#' recorded from training data (with \code{\link{record}}).  
#'
#' @method run_validation_tests character
#' 
#' @export
#' 
#' @return \code{list} results from validations tests.
run_validation_tests.character <- function(x, parameters, ...) {

  # run tests.
  list(
    new_NA = !parameters$any_NA & is.na(x),
    new_level = !is.na(x) & (!x %in% parameters$levels)
  )

}

#' Run Validation Tests on Factor
#'
#' Runs a set of validation tests on a \code{factor} in new data. These tests
#' are based on parameters recorded (with \code{\link{record}}) from the 
#' training data.
#' 
#' @param x \code{factor} in new data.
#' @param ... further arguments passed to or from other methods. Not used at
#' the moment.
#' @param parameters \code{list} parameters and meta data of the same variable
#' recorded from training data (with \code{\link{record}}).  
#'
#' @method run_validation_tests factor
#' 
#' @export
#' 
#' @return \code{list} results from validations tests.
run_validation_tests.factor <- function(x, parameters, ...) {

  # run tests.
  list(
    mismatch_levels = !identical(levels(x), parameters$levels),
    new_NA = !parameters$any_NA & is.na(x),
    new_level = !is.na(x) & (!x %in% parameters$levels)
  )

}

#' Run Validation Tests on Integer
#'
#' Runs a set of validation tests on a \code{integer} in new data. These tests
#' are based on parameters recorded (with \code{\link{record}}) from the 
#' training data.
#' 
#' @param x \code{integer} in new data.
#' @param ... further arguments passed to or from other methods. Not used at
#' the moment.
#' @param parameters \code{list} parameters and meta data of the same variable
#' recorded from training data (with \code{\link{record}}).    
#' 
#' @method run_validations_tests numeric
#' 
#' @export
#' 
#' @return \code{list} results from validations tests.
play.integer <- function(x, parameters, ...) {

  # run tests.
  list(
    outside_range = !is.na(x) & (x < parameters$min | x > parameters$max),
    new_NA = !parameters$any_NA & is.na(x)
  )

}

#' Run Validation Tests on Variable
#'
#' Runs a set of validation tests on variable in new data. These tests
#' are based on parameters recorded (with \code{\link{record}}) from the 
#' training data.
#' 
#' @param x anything.
#' @param ... further arguments passed to or from other methods. Not used at
#' the moment.
#' @param parameters \code{list} parameters and meta data of the same variable
#' recorded from training data (with \code{\link{record}}).  
#'
#' @method run_validation_tests default
#' 
#' @export
#' 
#' @return \code{list} results from validations tests.
run_validation_tests.default <- function(x, parameters, ...) {

  # run tests.
  list(
    new_NA = !parameters$any_NA & is.na(x)
  )

}

#' Validate New Data by Playing a Data Tape on It
#' 
#' Runs a set of validation tests on new data to be predicted with an existing 
#' machine learning model. These tests are based on parameters recorded (with 
#' \code{\link{record}}) from training data.
#' 
#' @param tape \code{data.tape} parameters and meta data recorded from 
#' training data. 
#' @param verbose \code{logical} should messages be printed?
#' @param newdata \code{data.frame} new data to be predicted with an existing 
#' machine learning model.
#'
#' @export
#' 
#' @return \code{data.playback} results from validations tests.
#' 
#' @examples
#' # record tape from `iris`.
#' tape <- record(iris)
#' # simulate new data.
#' newdata <- simulate_newdata_iris()
#' # validate new data by playing new tape on it.
#' play(tape, newdata)
play <- function(tape, newdata, verbose = TRUE) {

  # check, if input belongs to correct class.
  if (!inherits(tape, "data.tape")) {
    stop("'tape' must belong to 'data.tape' class.")
  }
  
  # check, if input belongs to correct class.
  if (!inherits(newdata, "data.frame")) {
    stop("'newdata' must belong to 'data.frame' class.")
  }

  # how many rows in new data.set (="duration")?
  duration <- nrow(newdata)
  if (duration == 0) {stop("New data set is empty - contains 0 rows.")}
  
  # save variable names of variables in new data set.
  variables <- names(newdata)

  if (verbose) {
    cat("\n[PLAY]\n\n")
    cat("... playing data.tape on new data with", ncol(newdata),
      "columns and", nrow(newdata), "rows ...\n\n")
  }

  # check if there any new variables in new data set, that have not been
  # observed before.
  new_variable <- as.list(!names(newdata) %in% names(tape$class_variables))
  names(new_variable) <- names(newdata)

  # check if one or more variables are missing from new data set.
  missing_variable <- as.list(!names(tape$class_variables) %in% names(newdata))
  names(missing_variable) <- names(tape$class_variables)

  # check if there are any class mismatches.
  variables_to_check <- names(newdata)[!names(newdata) %in% c(names(missing_variable)[which(as.logical(missing_variable))],
                                                  names(new_variable)[which(as.logical(new_variable))])]
  
  # compute classes of these variables in new dataset.
  class_variables_newdata <- lapply(newdata[variables_to_check], class)

  # check for class mismatches.
  mismatch_class <- mapply(function(x,y) {!identical(x,y)},
                           class_variables_newdata,
                           tape$class_variables[variables_to_check],
                           SIMPLIFY = FALSE)
  mismatch_class_names <- variables_to_check[as.logical(mismatch_class)]
  names(mismatch_class) <- variables_to_check

  # subset columns for detailed checks.
  variables_to_check <- variables_to_check[!variables_to_check %in% mismatch_class_names]
  tape <- tape$parameters[variables_to_check]
  newdata <- newdata[variables_to_check]

  # perform detailed checks.
  detailed_checks <- mapply(run_validation_tests, newdata, tape, SIMPLIFY = FALSE)
  detailed_checks <- compress_detailed_checks(detailed_checks)
  
  # set meta data for all checks.
  checks_meta_data <- list(
    missing_variable = list(compute_level = "col",
                            description = "variable(s) observed in training data but missing in new data"),
    mismatch_class = list(compute_level = "col",
                          description = "'class' in new data does not match 'class' in training data"),
    mismatch_levels = list(compute_level = "col",
                           description = "'levels' in new data and training data are not identical"),
    new_variable = list(compute_level = "col",
                        description = "variable(s) observed in new data but not in training data"),
    outside_range = list(compute_level = "row",
                         description = "value(s) in new data without recorded range in training data"),
    new_level = list(compute_level = "row",
                     description = "one or more new 'level'(s) in new data compared to training data. Characters treated as factors."),
    new_NA = list(compute_level = "row",
                  description = "NA(s) observed in new data but not in training data")
  )

  # combine results into one list, the structure of which defines the
  # 'data.playback' class.
  playback <- list(
    tape = tape,
    duration = duration,
    variables = variables,
    checks_meta_data = checks_meta_data,
    checks = append(detailed_checks,
                    list(new_variable = compress_checks(new_variable),
                         missing_variable = compress_checks(missing_variable),
                         mismatch_class = compress_checks(mismatch_class)))
  )

  # set class.
  class(playback) <- append("data.playback", class(playback))

  if (verbose) {
    cat("[STOP]\n")
  }

  playback

}
