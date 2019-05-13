#' Print Data Playback
#'
#' @aliases print.data.playback
#' @param x A `data.playback` object.
#' @param ... further arguments passed to or from other methods.
#'
#' @return The original object (invisibly)
#'
#' @importFrom crayon red blue green bgMagenta
#'
#' @export
#'
#' @examples
#' # record tape from `iris`.
#' tape <- record(iris)
#' # load data.
#' data(iris_newdata)
#' # validate new data by playing new tape on it.
#' playback <- play(tape, iris_newdata)
#' # print it.
#' print(playback)
print.data.playback <- function(x, ...) {

  cat(bgMagenta("\n[PLAY]\n\n"))

  # Short summary:
  cat("# of rows in new data: ", blue(x$nrow_newdata), "\n", sep = "")
  cat("# of rows passing all tests: ",
      green(sum(get_clean_rows(x))), "\n", sep = "")
  cat("# of rows failing one or more tests: ",
      red(sum(!get_clean_rows(x))), "\n", sep = "")
  cat("\n", "Test results (failures):\n", sep = "")

  # print tests evaluated on column level.
  # first, identify tests, that are evaluated on column level.
  tests_evaluate_level <- vapply(create_tests_meta_data(), '[[', "evaluate_level",
                                 FUN.VALUE = character(1))
  tests_col_level <- names(tests_evaluate_level)[tests_evaluate_level == "col"]
  # print test results.
  lapply(tests_col_level, function (g) {print_tests_collevel(x, g)})

  # print tests evaluated on row level.
  # first, identify tests, that are evaluated on row level.
  tests_row_level <- names(tests_evaluate_level)[tests_evaluate_level == "row"]
  lapply(tests_row_level,
         function (g, ...) {print_tests_rowlevel(x, g, ...)})

  cat("\nTest descriptions:\n")
  lapply(names(create_tests_meta_data()),
         function (g) {print_test_description(x, g)})

  cat(bgMagenta("\n[STOP]\n"))

  # return invisibly.
  invisible(x)

}

