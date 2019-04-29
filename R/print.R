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
  cat("# of rows in new data: ", x$duration, "\n", sep = "")
  cat("# of rows passing all tests: ", 
      sum(get_clean_rows(x)), "\n", sep = "")
  cat("# of rows, that failed one or more tests: ", 
      sum(!get_clean_rows(x)), "\n", sep = "")
  cat("\n", "Tests failed:\n", sep = "")
  # print checks computed on column level.
  # identify checks, that are computed on column level.
  checks_compute_level <- vapply(x$checks_metadata, '[[', "compute_level", 
                                 FUN.VALUE = character(1))
  checks_col_level <- names(checks_compute_level)[checks_compute_level == "col"]
  # print.
  lapply(checks_col_level, function (g) {print_checks_collevel(x, g)})
  
  # print checks computed on row level.
  # identify checks, that are computed on row level.
  checks_row_level <- names(checks_compute_level)[checks_compute_level == "row"]
  lapply(checks_row_level,
         function (g, ...) {print_checks_rowlevel(x, g, ...)})
  
  cat("\nTest descriptions:\n")
  lapply(names(x$checks), function (g) {print_test_description(x, g)})
  
  cat("\n[STOP]")
  
  # return invisibly.
  invisible(x)
  
}

