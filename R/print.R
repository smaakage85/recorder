#' Print Playback
#'
#' @aliases print.playback
#' @param x A `playback` object.
#' @param first The number of row indices to print pr. failed check.
#'
#' @return The original object (invisibly)
#'
#' @author Lars Kjeldgaard
#' @export
print.playback <- function(x, ...) {
 
  cat("[PLAY]\n\n")

  # Number of rows:
  cat("Number of rows in new data: ", x$duration, "\n\n", sep = "")

  # print checks computed on column level.
  lapply(x$checks_collevel_names,
         function (g) {print_checks_collevel(x, g)})
  
  # print checks computed on row level.
  checks_rowlevel_names <- names(playback$checks)[!names(playback$checks) %in% 
                                                    x$checks_collevel_names]
  lapply(checks_rowlevel_names,
         function (g, ...) {print_checks_rowlevel(x, g, ...)})
  cat("-----------------------")
  cat("\n", "Number of rows, that passed all checks: ", 
      sum(get_clean_rows(x)), "\n", sep = "")
  cat("Number of rows, that failed one or more checks: ", 
      sum(!get_clean_rows(x)), "\n", sep = "")
  
  
  cat("\n[STOP]")
  
  # return invisibly.
  invisible(x)
  
  
  
}
