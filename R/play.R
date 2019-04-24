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
    outside_range = which(!is.na(x) &
                            x < tape$min |
                            x > tape$max),
    new_NA = which(!tape$any_NA & is.na(x))
  )

}

play.character <- function(x, tape) {

  # compute checks.
  list(
    new_NA = which(!tape$any_NA & is.na(x)),
    new_level = which(!is.na(x) & (!x %in% tape$levels))
  )

}

play.factor <- function(x, tape) {

  # compute checks.
  list(
    mismatch_levels = !identical(levels(x), tape$levels),
    new_NA = which(!tape$any_NA & is.na(x)),
    new_level = which(!is.na(x) & (!x %in% tape$levels))
  )

}

play.integer <- function(x, tape) {

  # compute checks.
  list(
    outside_range = which(!is.na(x) &
                            x < tape$min |
                            x > tape$max),
    new_NA = which(!tape$any_NA & is.na(x))
  )

}

play.default <- function(x, tape) {

  # compute checks.
  list(
    new_NA = which(!tape$any_NA & is.na(x))
  )

}

play.data.frame <- function(x, tape) {

  # check, if input belongs to correct class.
  if (!inherits(tape, "data.tape")) {stop("'tape' must belong to 'data.tape' class.")}
  
    # how many rows in new data.set (="duration")?
  duration <- nrow(x)
  if (duration == 0) {stop("New data set is empty - contains 0 rows.")}

  cat("▶ PLAY\n\n")
  cat("... ♩ ♪ ♫ ♬\n\n")
  cat("[playing data.tape on new data with", ncol(x), 
      "columns and", nrow(x), "rows]\n\n")
  # check if there any new variables in new data set, that have not been
  # observed before.
  new_variable <- names(x)[!names(x) %in% names(tape$classes)]

  # check if one or more variables are missing from new data set.
  missing_variable <- names(tape$classes)[!names(tape$classes) %in% names(x)]

  # check if there are any class mismatches.
  variables_to_check <- names(x)[!names(x) %in% c(missing_variable, new_variable)]
  # compute classes of these variables in new dataset.
  classes_newdata <- lapply(x[variables_to_check], class)

  # check for class mismatches.
  mismatch_class <- mapply(identical,
                           classes_newdata,
                           tape$classes[variables_to_check],
                           SIMPLIFY = TRUE)
  mismatch_class <- variables_to_check[!mismatch_class]

  # subset columns to check in details.
  variables_to_check <- variables_to_check[!variables_to_check %in% mismatch_class]
  tape <- tape$stats[variables_to_check]
  x <- x[variables_to_check]

  # perform detailed checks.
  detailed_checks <- mapply(play, x, tape, SIMPLIFY = FALSE)
  detailed_checks <- compress_detailed_checks(detailed_checks)

  # combine results into one list, the structure of which 
  playback <- list(
    tape = tape,
    misc = list(duration = duration,
                new_variable = new_variable),
    aggregated_checks = list(
      missing_variable = missing_variable,
      mismatch_class = mismatch_class
    ),
    detailed_checks = detailed_checks
    )

  # set class.
  class(playback) <- append("playback", class(playback))

  cat("♬ ♫ ♪ ♩ ...\n\n")
  cat("\n∎ STOP\n\n")
  
  playback

}
