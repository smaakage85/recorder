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
print.playback <- function(x, first = 10) {
  
  cat("▶ PLAY\n\n")
  cat("... ♩ ♪ ♫ ♬\n\n")

  # misc. metadata:
  cat("Duration (number of rows in new data):", x$misc$duration, "\n\n")

  cat("Soft checks:\n")
  cat("- 'new_variable': unexpected variables variables in new data:",
      paste_vector(x$misc$new_variable), "\n")

  cat("\nAggregated checks:\n")
  cat("- 'missing_variable': variables missing in new data:",
      paste_vector(x$aggregated_checks$missing_variable), "\n")
  cat("- 'mismatch_class': 'class' differs from recorded classes:",
      paste_vector(x$aggregated_checks$mismatch_class), "\n")
  
  cat("\nDetailed checks:\n")
  cat("- 'new_NA': NA's in new data but not recorded on training data:",
      paste_all_cols_with_rows(x$detailed_checks$new_NA, first = first), "\n")
  cat("- 'outside_range': variable outside recorded range:",
      paste_all_cols_with_rows(x$detailed_checks$outside_range, first = first), "\n")
  cat("- 'mismatch_levels': 'levels' differs from recorded levels:",
      paste_vector(x$detailed_checks$mismatch_levels), "\n")
  cat("- 'new_level': new 'level' detected in new data:",
      paste_all_cols_with_rows(x$detailed_checks$new_level, first = first), "\n")

  cat("\n♬ ♫ ♪ ♩ ...")
  cat("\n\n∎ STOP\n\n")
  
  invisible(x)
}

# https://github.com/tidymodels/recipes/blob/master/R/recipe.R
