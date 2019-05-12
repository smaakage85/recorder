context("play()")

# record iris data set.
tape <- record(iris, verbose = FALSE)
# play on the same data - no test failures expected!
playback <- play(tape, iris, verbose = FALSE)

# basic output tests.
expect_is(playback, "data.playback")
# all tests produced.
tests_meta_data <- create_tests_meta_data()
expect_true(all(names(playback$tests) %in% names(tests_meta_data)))

# expected results.
expect_true(all(vapply(playback$tests, length, FUN.VALUE = integer(1)) == 0))

# error, when invoking with empty data set.
expect_error(play(tape, iris[0,], verbose = FALSE))

# does the function handle data.frame with solely new variables reasonably?
iris_copy <- iris
names(iris_copy) <- letters[1:5]
playback <- play(tape, iris_copy, verbose = FALSE)
expect_is(playback, "data.playback")
expect_equal(names(playback$tests$new_variable), names(iris_copy))
expect_true(all(as.logical(playback$tests$new_variable)))
expect_equal(names(playback$tests$missing_variable), names(iris))
expect_true(all(as.logical(playback$tests$missing_variable)))

# are only col level tests computed?
col_level_tests <- vapply(tests_meta_data, '[[', "evaluate_level", FUN.VALUE = character(1)) == "col"
col_level_tests <- names(col_level_tests)[col_level_tests]
expect_true(all(names(playback$tests) %in% col_level_tests))

# only NA's for computing statistics. Correct handling.
data(iris)
iris$Sepal.Width <- NA_real_
expect_warning(tape <- record(iris, verbose = FALSE))
data(iris)
playback <- play(tape, iris, verbose = FALSE)
expect_length(length(playback$tests$outside_range), 1)
expect_true(all(playback$tests$outside_range$Sepal.Width))

# test 'play.character'.
df <- data.frame(a = letters[1:10], stringsAsFactors = FALSE)
tape <- record(df, verbose = FALSE)
results <- play(tape, df, verbose = FALSE)
expect_length(results$tests$new_level, 0)
newdata <- data.frame(a = letters[2:11], stringsAsFactors = FALSE)
results <- play(tape, newdata, verbose = FALSE)
expect_true(all(c("unique_values","any_NA") %in% names(tape$parameters$a)))
expect_true(any(results$tests$new_text$a))
