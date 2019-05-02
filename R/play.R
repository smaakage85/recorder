#' Validate New Data by Playing a Data Tape on Them
#' 
#' Runs a set of validation tests on new data for an existing machine learning 
#' model. These tests are based on parameters recorded (with 
#' \code{\link{record}}) from the training data.
#' 
#' @param x \code{data.frame} new data.
#' @param ... further arguments passed to or from other methods.
#' @param tape \code{data.tape} parameters and meta data recorded from 
#' training data. 
#'
#' @return \code{data.playback} results from validations tests. The list will 
#' inherit from the \code{data.playback} class.
#' 
#' @export
#' 
#' @examples
#' # record tape from `iris`.
#' tape <- record(iris)
#' # simulate new data.
#' newdata <- simulate_newdata_iris()
#' # validate new data by playing new tape on it.
#' play(newdata, tape)
play <- function (x, tape, ...) {
  UseMethod("play", x)
}

#' Validate Numeric
#'
#' Runs a set of validation tests on a \code{numeric} in new data. These tests
#' are based on parameters recorded (with \code{\link{record}}) from the 
#' training data.
#' 
#' @param x \code{numeric} from new data.
#' @param ... all further arguments.
#' @param tape \code{list} parameters and meta data recorded from training data. 
#'
#' @method play numeric
#' 
#' @export
#' 
#' @return \code{list} results from validations tests.
play.numeric <- function(x, tape, ...) {

  # run tests.
  list(
    outside_range = !is.na(x) & (x < tape$min | x > tape$max),
    new_NA = !tape$any_NA & is.na(x)
  )

}

#' Validate Character
#'
#' Runs a set of validation tests on a \code{character} in new data. These tests
#' are based on parameters recorded (with \code{\link{record}}) from the 
#' training data.
#' 
#' @param x \code{character} from new data.
#' @param ... all further arguments.
#' @param tape \code{list} parameters and meta data recorded from training data. 
#'
#' @method play character
#' 
#' @export
#' 
#' @return \code{list} results from validations tests.
play.character <- function(x, tape, ...) {

  # run tests.
  list(
    new_NA = !tape$any_NA & is.na(x),
    new_level = !is.na(x) & (!x %in% tape$levels)
  )

}

#' Validate Factor
#'
#' Runs a set of validation tests on a \code{factor} in new data. These tests
#' are based on parameters recorded (with \code{\link{record}}) from the 
#' training data.
#' 
#' @param x \code{factor} from new data.
#' @param ... all further arguments.
#' @param tape \code{list} parameters and meta data recorded from training data. 
#'
#' @method play factor
#' 
#' @export
#' 
#' @return \code{list} results from validations tests.
play.factor <- function(x, tape, ...) {

  # run tests.
  list(
    mismatch_levels = !identical(levels(x), tape$levels),
    new_NA = !tape$any_NA & is.na(x),
    new_level = !is.na(x) & (!x %in% tape$levels)
  )

}

#' Validate Integer
#'
#' Runs a set of validation tests on a \code{integer} in new data. These tests
#' are based on parameters recorded (with \code{\link{record}}) from the 
#' training data.
#' 
#' @param x \code{integer} from new data.
#' @param ... all further arguments.
#' @param tape \code{list} parameters and meta data recorded from training data. 
#' 
#' @export
#' 
#' @return \code{list} results from validations tests.
play.integer <- function(x, tape, ...) {

  # run tests.
  list(
    outside_range = !is.na(x) & (x < tape$min | x > tape$max),
    new_NA = !tape$any_NA & is.na(x)
  )

}

#' Validate Variable
#'
#' Runs a set of validation tests on variable in new data. These tests
#' are based on parameters recorded (with \code{\link{record}}) from the 
#' training data.
#' 
#' @param x anything.
#' @param ... all further arguments.
#' @param tape \code{list} parameters and meta data recorded from training data. 
#'
#' @method play default
#' 
#' @export
#' 
#' @return \code{list} results from validations tests.
play.default <- function(x, tape, ...) {

  # run tests.
  list(
    new_NA = !tape$any_NA & is.na(x)
  )

}

#' Validate New Data by Playing a Data Tape on Them
#' 
#' Runs a set of validation tests on new data for an existing machine learning 
#' model. These tests are based on parameters recorded (with 
#' \code{\link{record}}) from the training data.
#' 
#' @param x \code{data.frame} new data.
#' @param ... further arguments passed to or from other methods.
#' @param tape \code{data.tape} parameters and meta data recorded from 
#' training data. 
#' @param verbose \code{logical} should messages be printed?
#'
#' @method play data.frame
#' 
#' @export
#' 
#' @return \code{data.playback} results from validations tests. The list will 
#' inherit from the \code{data.playback}.
play.data.frame <- function(x, tape, verbose = TRUE, ...) {

  # check, if input belongs to correct class.
  if (!inherits(tape, "data.tape")) {
    stop("'tape' must belong to 'data.tape' class.")
    }

  # how many rows in new data.set (="duration")?
  duration <- nrow(x)
  if (duration == 0) {stop("New data set is empty - contains 0 rows.")}
  
  # save variable names of variables in new data set.
  variables <- names(x)

  if (verbose) {
    cat("\n[PLAY]\n\n")
    cat("... playing data.tape on new data with", ncol(x),
      "columns and", nrow(x), "rows ...\n\n")
  }

  # check if there any new variables in new data set, that have not been
  # observed before.
  new_variable <- as.list(!names(x) %in% names(tape$class_variables))
  names(new_variable) <- names(x)

  # check if one or more variables are missing from new data set.
  missing_variable <- as.list(!names(tape$class_variables) %in% names(x))
  names(missing_variable) <- names(tape$class_variables)

  # check if there are any class mismatches.
  variables_to_check <- names(x)[!names(x) %in% c(names(missing_variable)[which(as.logical(missing_variable))],
                                                  names(new_variable)[which(as.logical(new_variable))])]
  
  # compute classes of these variables in new dataset.
  class_variables_newdata <- lapply(x[variables_to_check], class)

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
  x <- x[variables_to_check]

  # perform detailed checks.
  detailed_checks <- mapply(play, x, tape, SIMPLIFY = FALSE)
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
