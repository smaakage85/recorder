#' Record Data Statistics
#'
#' @param x what variable or dataset to record/learn statistics from.
#' @param ... all other arguments.
#'
#' @return \code{list} relevant statistics used for validating a new data set.
#' @export
record <- function (x, ...) {
  UseMethod("record", x)
}

record.numeric <- function(x) {

  # record statistics.
  list(
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    any_NA = any_NA(x)
  )

}

record.integer <- function(x) {

  # record statistics.
  list(
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    any_NA = any_NA(x)
  )

}

record.factor <- function(x) {

  # record statistics.
  list(
    levels = levels(x),
    any_NA = any_NA(x)
  )

}

record.character <- function(x) {

  # record statistics.
  list(
    levels = unique(x),
    any_NA = any_NA(x)
  )

}

record.other <- function(x) {

  # record statistics.
  list(
    any_NA = any_NA(x)
  )

}

record.data.frame <- function(x) {

  # record classes.
  classes <- lapply(x, class)

  # set 'other' class to treat variables of classes, for which no
  # specific 'play' method is defined.
  x <- lapply(x, set_other_class)

  # record statistics for all variables.
  stats <- lapply(x, record)

  # combine into one list - one 'recording'.
  recording <- list(classes = classes, stats = stats)

  # set class accordingly.
  class(recording) <- append(class(recording), "recording")

  recording

}

any_NA <- function(x) {any(is.na(x))}
set_other_class <- function(x) {
  class(x) <- append(class(x), "other")
  x
}
