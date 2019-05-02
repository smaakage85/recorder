#' Record Parameters and Meta Data from Training Data
#' 
#' Records relevant meta data and parameters from training data for a machine 
#' learning model. The recorded data can then be used to compute a set of 
#' validation tests on new data with the `play()` function.
#'
#' @param x data set or variable to record parameters and other relevant 
#' metadata from.
#'
#' @return \code{list} recorded statistics and metadata. The list will inherit
#' from the \code{data.tape} class when `record` is invoked with a 
#' \code{data.frame}.
#' 
#' @export
#' @examples
#' # The typical use case for this function is applications on data.frames.
#' record(iris)
#' 
#' # But you can also use it 
#' record(iris$Sepal.Width)
#' record(iris$Species)
record <- function (x) {
  UseMethod("record", x)
}

record.numeric <- function(x) {

  # record parameters.
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