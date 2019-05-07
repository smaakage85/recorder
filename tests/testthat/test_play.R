context("play()")

# record iris data set.
tape <- record(iris, verbose = FALSE)
# play on the same data - no test failures expected!
playback <- play(tape, iris, verbose = FALSE)

# basic output tests.
expect_is(playback, "data.playback")
# all tests produced.
tests_meta_data <- get("tests_meta_data")
expect_true(all(names(tests_meta_data) %in% names(playback$tests)))
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






