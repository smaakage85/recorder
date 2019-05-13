#' Run Validation Tests on Variable in New Data
#' 
#' Runs a set of validation tests on a variable in new data. These tests are 
#' based on statistics and meta data of the same variable recorded 
#' (with \code{\link{record}}) from the training data.
#' 
#' @details Look up the descriptions and other meta data of the available 
#' validation tests with \code{\link{get_tests_meta_data}}.
#' 
#' @param x variable in new data.
#' @param ... further arguments passed to or from other methods. Not used at
#' the moment.
#' @param parameters \code{list} statistics and meta data of the same variable
#' recorded from training data (with \code{\link{record}}). 
#'
#' @return \code{list} results from validation tests. 
#' 
#' @export
run_validation_tests <- function (x, parameters, ...) {
  UseMethod("run_validation_tests", x)
}

#' Run Validation Tests on a Numeric
#'
#' Runs a set of validation tests on a \code{numeric} in new data. These tests
#' are based on statistics and meta data of the same variable recorded 
#' (with \code{\link{record}}) from the training data.
#' 
#' @param x \code{numeric} in new data.
#' @param ... further arguments passed to or from other methods. Not used at
#' the moment.
#' @param parameters \code{list} statistics and meta data of the same variable
#' recorded from training data (with \code{\link{record}}).  
#'
#' @method run_validation_tests numeric
#' 
#' @export
#' 
#' @return \code{list} results from validation tests.
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
#' are based on statistics and meta data of the same variable recorded 
#' (with \code{\link{record}}) from the training data.
#' 
#' @param x \code{character} in new data.
#' @param ... further arguments passed to or from other methods. Not used at
#' the moment.
#' @param parameters \code{list} statistics and meta data of the same variable
#' recorded from training data (with \code{\link{record}}).  
#'
#' @method run_validation_tests character
#' 
#' @export
#' 
#' @return \code{list} results from validation tests.
run_validation_tests.character <- function(x, parameters, ...) {

  # run tests.
  list(
    new_NA = !parameters$any_NA & is.na(x),
    new_text = !is.na(x) & (!x %in% parameters$unique_values)
  )

}

#' Run Validation Tests on Factor
#'
#' Runs a set of validation tests on a \code{factor} in new data. These tests
#' are based on statistics and meta data of the same variable recorded 
#' (with \code{\link{record}}) from the training data.
#' 
#' @param x \code{factor} in new data.
#' @param ... further arguments passed to or from other methods. Not used at
#' the moment.
#' @param parameters \code{list} statistics and meta data of the same variable
#' recorded from training data (with \code{\link{record}}).  
#'
#' @method run_validation_tests factor
#' 
#' @export
#' 
#' @return \code{list} results from validation tests.
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
#' are based on statistics and meta data of the same variable recorded 
#' (with \code{\link{record}}) from the training data.
#' 
#' @param x \code{integer} in new data.
#' @param ... further arguments passed to or from other methods. Not used at
#' the moment.
#' @param parameters \code{list} statistics and meta data of the same variable
#' recorded from training data (with \code{\link{record}}).    
#' 
#' @method run_validation_tests integer
#' 
#' @export
#' 
#' @return \code{list} results from validation tests.
run_validation_tests.integer <- function(x, parameters, ...) {

  # run tests.
  list(
    outside_range = !is.na(x) & (x < parameters$min | x > parameters$max),
    new_NA = !parameters$any_NA & is.na(x)
  )

}

#' Run Validation Tests on Variable
#'
#' Runs a set of validation tests on variable in new data. These tests
#' are based on statistics and meta data of the same variable recorded 
#' (with \code{\link{record}}) from the training data.
#' 
#' @param x anything.
#' @param ... further arguments passed to or from other methods. Not used at
#' the moment.
#' @param parameters \code{list} statistics and meta data of the same variable
#' recorded from training data (with \code{\link{record}}).  
#'
#' @method run_validation_tests default
#' 
#' @export
#' 
#' @return \code{list} results from validation tests.
run_validation_tests.default <- function(x, parameters, ...) {

  # run tests.
  list(
    new_NA = !parameters$any_NA & is.na(x)
  )

}

#' Validate New Data by Playing a Data Tape on It
#' 
#' Runs a set of validation tests on new data to be predicted with an existing 
#' predictive model. These tests are based on statistics and meta data of
#' the variables in the training data - recorded with \code{\link{record}}.
#' 
#' @details Look up the descriptions and other meta data of the available 
#' validation tests with \code{\link{get_tests_meta_data}}.
#' 
#' @param tape \code{data.tape} statistics and meta data recorded from 
#' training data. 
#' @param verbose \code{logical} should messages be printed?
#' @param newdata \code{data.frame} new data to be predicted with an existing 
#' predictive model.
#'
#' @export
#' 
#' @return \code{data.playback} results from validation tests.
#' 
#' @examples
#' # record tape from `iris`.
#' tape <- record(iris)
#' # load data.
#' data(iris_newdata)
#' # validate new data by playing new tape on it.
#' play(tape, iris_newdata)
play <- function(tape, newdata, verbose = TRUE) {

  # test if tape belongs to correct class.
  if (!inherits(tape, "data.tape")) {
    stop("'tape' must belong to 'data.tape' class.")
  }
  
  # test if newdata belongs to correct class.
  if (!inherits(newdata, "data.frame")) {
    stop("'newdata' must belong to 'data.frame' class.")
  }

  # how many rows in new data.set?
  nrow_newdata <- nrow(newdata)
  if (nrow_newdata == 0) {stop("New data set is empty - contains 0 rows.")}
  
  # save variable names of variables in new data set.
  variables <- names(newdata)

  if (verbose) {
    cat(bgMagenta("\n[PLAY]\n\n"))
    cat("... playing data.tape on new data with", nrow(newdata),
      "rows with", ncol(newdata), "columns ...\n\n")
  }

  # test if there any new variables in new data set, that have not been
  # observed before.
  new_variable <- as.list(!names(newdata) %in% names(tape$class_variables))
  names(new_variable) <- names(newdata)

  # test if one or more variables are missing from new data set.
  missing_variable <- as.list(!names(tape$class_variables) %in% names(newdata))
  names(missing_variable) <- names(tape$class_variables)

  # test if there are any class mismatches.
  variables_to_test <- names(newdata)[!names(newdata) %in% c(names(missing_variable)[which(as.logical(missing_variable))],
                                                  names(new_variable)[which(as.logical(new_variable))])]
  
  # compute classes of these variables in new dataset.
  class_variables_newdata <- lapply(newdata[variables_to_test], class)

  # test for class mismatches.
  mismatch_class <- mapply(function(x,y) {!identical(x,y)},
                           class_variables_newdata,
                           tape$class_variables[variables_to_test],
                           SIMPLIFY = FALSE)
  mismatch_class_names <- variables_to_test[as.logical(mismatch_class)]
  names(mismatch_class) <- variables_to_test

  # subset columns for detailed tests.
  variables_to_test <- variables_to_test[!variables_to_test %in% mismatch_class_names]
  tape <- tape$parameters[variables_to_test]
  newdata <- newdata[variables_to_test]

  # perform detailed tests.
  detailed_tests <- mapply(run_validation_tests, newdata, tape, SIMPLIFY = FALSE)
  detailed_tests <- compress_detailed_tests(detailed_tests)
  
  # combine results into one list, the structure of which defines the
  # 'data.playback' class.
  playback <- list(
    tape = tape,
    nrow_newdata = nrow_newdata,
    variables = variables,
    tests = append(detailed_tests,
                    list(new_variable = compress_tests(new_variable),
                         missing_variable = compress_tests(missing_variable),
                         mismatch_class = compress_tests(mismatch_class)))
  )

  # set class.
  class(playback) <- append("data.playback", class(playback))

  if (verbose) {
    cat(bgMagenta("[STOP]\n"))
  }

  playback

}
