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
  
  cat("▶ PLAY\n\n")

  # misc. metadata:
  cat("Duration (number of rows in new data):", x$misc$duration, "\n\n")

  cat("Soft checks:\n")
  cat("- 'new_variable': Unexpected variables variables in new data:",
      paste_vector(x$misc$new_variable))
  cat("\n")

  cat("Aggregated checks:\n")
  cat("- 'missing_variable': Variables missing in new data:",
      paste_vector(x$aggregated_checks$missing_variable))
  cat("- 'mismatch_class': class' mismatches:",
      paste_vector(x$aggregated_checks$mismatch_class))

  cat("\n∎ STOP\n\n")
  
  invisible(x)
}

paste_vector <- function(x) {
  paste0(
    if (length(x) != 0) {
      paste0(x, collapse = ", ")
      } else {"None."},
    "\n")
}


# dc <- playb$detailed_checks
# checks <- lapply(dc, names)
# checks <- do.call(c, checks)
# checks <- unique(checks)
# 
# by_checks <- lapply(checks, function (x) {
#   lapply(dc, '[[', x)
# })
# names(by_checks) <- checks
# 
# 
# 
# 
# 
# lapply(playb$detailed_checks, '[[', "new_NA")

# https://github.com/tidymodels/recipes/blob/master/R/recipe.R
