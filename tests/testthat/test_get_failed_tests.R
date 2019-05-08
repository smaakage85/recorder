context("get_failed_tests()")
# load data.
# record tape from `iris`.
tape <- record(iris, verbose = FALSE)
# load data.
data(iris_newdata)
# validate new data by playing new tape on it.
playback <- play(tape, iris_newdata, verbose = FALSE)

results <- get_failed_tests(playback)
expect_is(results, "data.table")
expect_true(nrow(results) == nrow(iris_newdata))
expect_true(all(vapply(results, is.logical, FUN.VALUE = logical(1)) == TRUE))

# ignore functionality.
results <- get_failed_tests(playback, ignore_tests = "outside_range")
expect_true(!any(grepl("^outside_range", names(results))))
expect_warning(get_failed_tests(playback, ignore_combinations = list(missing_variable = "Petal.Length")))

results <- get_failed_tests(playback, ignore_cols = "junk")
expect_true(!any(grepl("junk$", names(results))))

results <- get_failed_tests(playback, ignore_combinations = list(outside_range = "Petal.Width"))
expect_true(!"outside_range.Petal.Width" %in% names(results))
expect_true("new_NA.Petal.Width" %in% names(results))

# everything combined.
results <- get_failed_tests(playback, 
                            ignore_tests = "missing_variable",
                            ignore_cols = "junk",
                            ignore_combinations = list(outside_range = "Petal.Width"))
expect_true(!"outside_range.Petal.Width" %in% names(results))
expect_true(!any(grepl("^missing_variable", names(results))))
expect_true(!any(grepl("junk$", names(results))))

# handling of no test failures.
playback <- play(tape, iris, verbose = FALSE)
results <- get_failed_tests(playback)
expect_true(names(results) == "any_failures")
expect_true(nrow(results) == nrow(iris))
expect_is(results$any_failures, "logical")
expect_true(!any(results$any_failures))

# handling of test failures in col level tests.
iris_copy <- iris
iris_copy$tester <- "smaakage"
playback <- play(tape, iris_copy, verbose = FALSE)
results <- get_failed_tests(playback)
expect_true(ncol(results) == 1)
expect_true(nrow(results) == nrow(iris))
expect_true(all(results[[1]]))

test_that("concatenate_test_failures", {
  df <- data.frame(a = c(FALSE, FALSE, TRUE, TRUE),
                   b = c(FALSE, TRUE, FALSE, TRUE))
  results <- concatenate_test_failures(df)
  expect_is(results, "character")
  expect_equal(results, c("", "b;", "a;", "a;b;"))
})



