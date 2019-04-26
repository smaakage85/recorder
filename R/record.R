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

record.default <- function(x) {

  # record statistics.
  list(
    any_NA = any_NA(x)
  )

}

record.data.frame <- function(x) {

  cat("[RECORD]\n\n")
  cat("... recording metadata and statistics for", ncol(x), 
      "columns and", nrow(x), "rows... \n\n")
  
  # record classes.
  classes <- lapply(x, class)

  # record statistics for all variables.
  stats <- lapply(x, record)

  # combine into one list, the structure of which defines the 'data.tape' 
  # by convention.
  data.tape <- list(classes = classes, stats = stats)

  # set class.
  class(data.tape) <- append(class(data.tape), "data.tape")

  cat("[STOP]\n\n")
  
  # return data.tape.
  data.tape

}

any_NA <- function(x) {any(is.na(x))}