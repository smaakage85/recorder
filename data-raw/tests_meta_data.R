# set meta data for tests.
tests_meta_data <- list(
  missing_variable = list(evaluate_level = "col",
                          description = "variable observed in training data but missing in new data"),
  mismatch_class = list(evaluate_level = "col",
                        description = "'class' in new data does not match 'class' in training data"),
  mismatch_levels = list(evaluate_level = "col",
                         description = "'levels' in new data and training data are not identical"),
  new_variable = list(evaluate_level = "col",
                      description = "variable observed in new data but not in training data"),
  outside_range = list(evaluate_level = "row",
                       description = "value in new data without recorded range in training data"),
  new_level = list(evaluate_level = "row",
                   description = "new 'level' in new data compared to training data"),
  new_NA = list(evaluate_level = "row",
                description = "NA observed in new data but not in training data"),
  new_text = list(evaluate_level = "row",
                  description = "new text in new data compared to training data")
)
# usethis::use_data(tests_meta_data, overwrite = TRUE)