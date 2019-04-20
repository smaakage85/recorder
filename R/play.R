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

play.numeric <- function(x, rec) {

  # compute checks.
  list(
    outside_range = which(!is.na(x) &
                            x < rec$min |
                            x > rec$max),
    new_NA = which(!rec$any_NA & is.na(x))
  )

}

play.character <- function(x, rec) {

  # compute checks.
  list(
    new_NA = which(!rec$any_NA & is.na(x)),
    new_level = which(!is.na(x) & (!x %in% rec$levels))
  )

}

play.factor <- function(x, rec) {

  # compute checks.
  list(
    levels_mismatch = !identical(levels(x), rec$levels),
    new_NA = which(!rec$any_NA & is.na(x)),
    new_level = which(!is.na(x) & (!x %in% rec$levels))
  )

}

play.integer <- function(x, rec) {

  # compute checks.
  list(
    outside_range = which(!is.na(x) &
                            x < rec$min |
                            x > rec$max),
    new_NA = which(!rec$any_NA & is.na(x))
  )

}

play.other <- function(x, rec) {

  # compute checks.
  list(
    new_NA = which(!rec$any_NA & is.na(x))
  )

}

play.data.frame <- function(x, rec, verbose = TRUE) {

  # check if there any new variables in new data set, that has not been
  # observed before.
  new_variable <- names(x)[!names(x) %in% names(rec$classes)]
  if (verbose && length(new_variable) > 0) {
    message("New variables detected in new data set: ",
            paste0(new_variable, collapse = ", "))
  }

  # check if one or more variables are missing in new data set.
  missing_variable <- names(rec$classes)[!names(rec$classes) %in% names(x)]

  # check if there are any class mismatches.
  variables_to_check <- names(x)[!names(x) %in% c(missing_variable, new_variable)]
  # compute classes of these variables in new dataset.
  classes_newdata <- lapply(x[variables_to_check], class)

  # check for class mismatches.
  mismatch_class <- mapply(identical,
                           classes_newdata,
                           rec$classes[variables_to_check],
                           SIMPLIFY = TRUE)
  mismatch_class <- variables_to_check[!mismatch_class]

  # subset columns to check in details.
  variables_to_check <- variables_to_check[!variables_to_check %in% mismatch_class]
  rec <- rec$stats[variables_to_check]
  x <- x[variables_to_check]

  # before performing checks, set 'other' class to treat variables of classes,
  # for which no specific 'play' method is defined.
  x <- lapply(x, set_other_class)

  # perform checks.
  detailed_checks <- mapply(play, x, rec, SIMPLIFY = FALSE)

  list(missing_variable = missing_variable,
       mismatch_class = mismatch_class,
       detailed_checks = detailed_checks)

  }
