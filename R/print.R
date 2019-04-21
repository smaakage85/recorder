#' Print a Playback
#'
#' @aliases print.playback
#' @param x A `playback` object
#' @param form_width The number of characters used to print the variables or
#'   terms in a formula
#' @param ... further arguments passed to or from other methods (not currently
#'   used).
#' @return The original object (invisibly)
#'
#' @author Lars Kjeldgaard
#' @export
print.playback <- function(x, form_width = 30, ...) {
  cat("-- recordr Playback --\n\n")

  # misc. metadata:
  cat("Duration (number of rows in new data):", x$misc$duration, "\n\n")

  cat("Soft checks:\n")
  cat("- 'new_variable': Variables in new data not previously observed:",
      if (length(x$misc$new_variable) != 0) {
        paste0(x$misc$new_variable, collapse = ", ")
      } else {"None."},
      "\n\n")

  cat("Rough checks:\n")
  cat("- 'missing_variable': Variables not found in new data:",
      if (length(x$rough_checks$missing_variable) != 0) {
        paste0(x$rough_checks$missing_variable, collapse = ", ")
      } else {"None."},
      "\n")
  cat("- 'mismatch_class': class' mismatches:",
      if (length(x$rough_checks$mismatch_class) != 0) {
        paste0(x$rough_checks$mismatch_class, collapse = ", ")
      } else {"None."},
      "\n")

  invisible(x)
}

# https://github.com/tidymodels/recipes/blob/master/R/recipe.R
