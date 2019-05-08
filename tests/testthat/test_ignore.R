context("ignore()")
# load data.
data("iris_newdata")

tape <- record(iris, verbose = FALSE)
playback <- play(tape, iris_newdata, verbose = FALSE)

test_that("ignore_tests()", {
  
  # ignore nothing.
  results <- ignore_tests(playback$tests, NULL)
  expect_identical(results, playback$tests)
  
  # ignore single test.
  results <- ignore_tests(playback$tests, "mismatch_levels")
  expect_is(results, "list")
  expect_true(!("mismatch_levels" %in% names(results)))
  
  # ignore two tests.
  tests <- c("mismatch_levels", "missing_variable")
  results <- ignore_tests(playback$tests, tests)
  expect_true(all(!(tests %in% names(results))))
  
  # ignore everything.
  tests <- names(playback$tests)
  results <- ignore_tests(playback$tests, tests)
  expect_length(results, 0)
  
  # invalid test name.
  tests <- "bogus"
  expect_warning(ignore_tests(playback$tests, tests))
  
})

test_that("ignore_cols()", {
  
  # ignore nothing.
  results <- ignore_cols(playback$tests, NULL, playback$variables)
  expect_identical(results, playback$test)
  expect_is(results, "list")
  
  # ignore single column.
  results <- ignore_cols(playback$tests, "Petal.Width", playback$variables)
  expect_true("Petal.Width" %in% names(playback$tests$outside_range))
  expect_true(!("Petal.Width" %in% names(results$outside_range)))
  expect_true("Petal.Width" %in% names(playback$tests$new_NA))
  expect_true(!("Petal.Width" %in% names(results$new_NA)))
  
  # ignore two columns.
  cols <- c("Petal.Width", "Sepal.Width")
  results <- ignore_cols(playback$tests, cols, playback$variables)
  expect_true("Sepal.Width" %in% names(playback$tests$outside_range))
  expect_true(!("Sepal.Width" %in% names(results$outside_range)))
  expect_true(!("Petal.Width" %in% names(results$outside_range)))
 
  # ignore all columns.
  cols <- unique(c(names(iris_newdata), names(iris)))
  expect_warning(results <- ignore_cols(playback$tests, cols, playback$variables))
  expect_true(all(vapply(results, length, FUN.VALUE = integer(1)) == 0))
  
  # invalid column name.
  cols <- "bogus"
  expect_warning(ignore_cols(playback$tests, cols, playback$variables))
  
})

test_that("ignore_combinations()", {
  
  # ignore nothing.
  results <- ignore_combinations(playback$tests, NULL, playback$variables)
  expect_identical(results, playback$tests)
  
  # ignore single combination, single column.
  results <- ignore_combinations(playback$tests, 
                                 list(outside_range = c("Sepal.Width")),
                                 playback$variables)
  expect_is(results, "list")
  expect_true(!("Sepal.Width" %in% names(results$outside_range)))
  
  # ignore single combination, two columns.
  cols <- c("Sepal.Width", "Petal.Width")
  results <- ignore_combinations(playback$tests, 
                                 list(outside_range = cols),
                                 playback$variables)
  expect_true(!any(cols %in% names(results$outside_range)))
  expect_true("Petal.Width" %in% names(results$new_NA))
  
  # ignore multiple columns.
  results <- ignore_combinations(playback$tests, 
                                 list(new_level = "Species",
                                      new_variable = "junk"),
                                 playback$variables)
  expect_true(!("Species" %in% names(results$new_level)))
  expect_true(!("junk" %in% names(results$new_variable)))
   
  # handling of invalid inputs.
  expect_warning(ignore_combinations(playback$tests, 
                                   list(new_level = "TeStEr"),
                                   playback$variables))
  expect_warning(ignore_combinations(playback$tests, 
                                   list(new_level = 2),
                                   playback$variables))
  expect_warning(ignore_combinations(playback$tests, 
                                     list(tesTER = "Sepal.Width"),
                                     playback$variables))
  
})
  