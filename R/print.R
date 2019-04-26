#' Print a Playback
#'
#' @aliases print.playback
#' @param x A `playback` object
#' @param first The number of row indices to print pr. check.
#'
#' @return The original object (invisibly)
#'
#' @author Lars Kjeldgaard
#' @export
print.playback <- function(x, first = 10) {
 
  cat("[PLAY]\n\n")

  # Number of rows:
  cat("Number of rows in new data):", x$misc$duration, "\n\n")

  # cat("Soft checks:\n")
  # cat("- 'new_variable': unexpected variables variables in new data:",
  #     paste_vector(x$misc$new_variable), "\n")

  cat("Checks:\n\n")
  cat("Check Name         Check Summary\n")
  cat("missing_variable   variable missing in new data:",
      paste_vector(x$aggregated_checks$missing_variable), "\n")
  cat("mismatch_class     'class' differs from recorded classes:",
      paste_vector(x$aggregated_checks$mismatch_class), "\n")
  cat("mismatch_levels    levels' differs from recorded levels:",
      paste_vector(x$detailed_checks$mismatch_levels), "\n")
  
  cat("new_NA             NA in new data but not recorded on training data:",
      paste_all_cols_with_rows(x$detailed_checks$new_NA, first = first), "\n")
  cat("outside_range      variable outside recorded range:",
      paste_all_cols_with_rows(x$detailed_checks$outside_range, first = first), "\n")
  cat("new_level          new 'level' detected in new data:",
      paste_all_cols_with_rows(x$detailed_checks$new_level, first = first), "\n")

  cat("\n[STOP]")
  
  # return invisibly.
  invisible(x)
  
}
