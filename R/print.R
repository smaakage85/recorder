#' Print Data Playback
#'
#' @aliases print.data.playback
#' @param x A `data.playback` object.
#' @param ... further arguments passed to or from other methods.
#'
#' @return The original object (invisibly)
#'
#' @export
print.data.playback <- function(x, ...) {

  cat("\n[PLAY]\n\n")

  # Number of rows:
  cat("# of rows in new data: ", x$nrow_newdata, "\n", sep = "")
  cat("# of rows passing all tests: ", 
      sum(get_clean_rows(x)), "\n", sep = "")
  cat("# of rows failing one or more tests: ", 
      sum(!get_clean_rows(x)), "\n", sep = "")
  cat("\n", "Tests (failed):\n", sep = "")
  # print tests computed on column level.
  # identify tests, that are computed on column level.
  tests_evaluate_level <- vapply(get("tests_meta_data"), '[[', "evaluate_level", 
                                 FUN.VALUE = character(1))
  tests_col_level <- names(tests_evaluate_level)[tests_evaluate_level == "col"]
  # print.
  lapply(tests_col_level, function (g) {print_tests_collevel(x, g)})
  
  # print tests computed on row level.
  # identify tests, that are computed on row level.
  tests_row_level <- names(tests_evaluate_level)[tests_evaluate_level == "row"]
  lapply(tests_row_level,
         function (g, ...) {print_tests_rowlevel(x, g, ...)})
  
  cat("\nTest descriptions:\n")
  lapply(names(x$tests), function (g) {print_test_description(x, g)})
  
  cat("\n[STOP]")
  
  # return invisibly.
  invisible(x)
  
}

