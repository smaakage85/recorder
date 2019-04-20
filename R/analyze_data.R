analyze <- function(df) {

  # extract classes.
  classes <- lapply(df, function(x) {list(class = class(x))})

  # return metadata.
  classes

}

#' @import data.table
analyze_numerics <- function(df) {

  # what variables belong to the numeric analysis type.
  nums <- vapply(df, function(x) {inherits(x, c("numeric", "integer"))}, FUN.VALUE = logical(1))

  # return NULL, there are not any variables of the specific type.
  if (!any(nums)) {
    return(NULL)
  }

  # subset relevant columns.
  df <- df[nums]

  # learn metadata.
  min <- lapply(df, min, na.rm = TRUE)
  max <- lapply(df, max, na.rm = TRUE)
  any_NA <- lapply(df, function(x) {any(is.na(x))})
  any_NULL <- lapply(df, function(x) {any(is.null(x))})

  tester(list(min = min, max = max, any_NA = any_NA, any_NULL = any_NULL))

}

tester <- function(lst) {
  keys <- unique(unlist(lapply(lst, names)))
  lst <- do.call(mapply, c(FUN = c, lapply(lst, `[`, keys)), SIMPLIFY = FALSE)
  lst
}

