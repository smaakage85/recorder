#' Play Data Recording on a New Data Set
#'
#' @param x \code{data.frame} data set to validate.
#' @param ... all other arguments.
#'
#' @return \code{list} relevant statistics used for checking a new data set.
#' @export
play <- function (x, ...) {
  UseMethod("play", x)
}

play.numeric <- function(x, tape) {

  # compute checks.
  list(
    outside_range = !is.na(x) & (x < tape$min | x > tape$max),
    new_NA = !tape$any_NA & is.na(x)
  )

}

play.character <- function(x, tape) {

  # compute checks.
  list(
    new_NA = !tape$any_NA & is.na(x),
    new_level = !is.na(x) & (!x %in% tape$levels)
  )

}

play.factor <- function(x, tape) {

  # compute checks.
  list(
    mismatch_levels = !identical(levels(x), tape$levels),
    new_NA = !tape$any_NA & is.na(x),
    new_level = !is.na(x) & (!x %in% tape$levels)
  )

}

play.integer <- function(x, tape) {

  # compute checks.
  list(
    outside_range = !is.na(x) & (x < tape$min | x > tape$max),
    new_NA = !tape$any_NA & is.na(x)
  )

}

play.default <- function(x, tape) {

  # compute checks.
  list(
    new_NA = !tape$any_NA & is.na(x)
  )

}

play.data.frame <- function(x, tape) {

  # check, if input belongs to correct class.
  if (!inherits(tape, "data.tape")) {stop("'tape' must belong to 'data.tape' class.")}

  # how many rows in new data.set (="duration")?
  duration <- nrow(x)
  if (duration == 0) {stop("New data set is empty - contains 0 rows.")}

  cat("[PLAY]\n\n")
  cat("... playing data.tape on new data with", ncol(x),
      "columns and", nrow(x), "rows ...\n\n")

  # check if there any new variables in new data set, that have not been
  # observed before.
  new_variable <- as.list(!names(x) %in% names(tape$classes))
  names(new_variable) <- names(x)

  # check if one or more variables are missing from new data set.
  missing_variable <- as.list(!names(tape$classes) %in% names(x))
  names(missing_variable) <- names(tape$classes)

  # check if there are any class mismatches.
  variables_to_check <- names(x)[!names(x) %in% c(names(missing_variable)[which(as.logical(missing_variable))],
                                                  names(new_variable)[which(as.logical(new_variable))])]
  # compute classes of these variables in new dataset.
  classes_newdata <- lapply(x[variables_to_check], class)

  # check for class mismatches.
  mismatch_class <- mapply(function(x,y) {!identical(x,y)},
                           classes_newdata,
                           tape$classes[variables_to_check],
                           SIMPLIFY = FALSE)
  mismatch_class_names <- variables_to_check[as.logical(mismatch_class)]
  names(mismatch_class) <- variables_to_check

  # subset columns for detailed checks.
  variables_to_check <- variables_to_check[!variables_to_check %in% mismatch_class_names]
  tape <- tape$stats[variables_to_check]
  x <- x[variables_to_check]

  # perform detailed checks.
  detailed_checks <- mapply(play, x, tape, SIMPLIFY = FALSE)
  detailed_checks <- compress_detailed_checks(detailed_checks)

  # combine results into one list, the structure of which defines the
  # 'playback' class.
  playback <- list(
    tape = tape,
    duration = duration,
    checks = append(detailed_checks,
                    list(new_variable = compress_checks(new_variable),
                         missing_variable = compress_checks(missing_variable),
                         mismatch_class = compress_checks(mismatch_class)))
  )

  # set class.
  class(playback) <- append("playback", class(playback))

  cat("[STOP]")

  playback

}
