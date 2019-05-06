context("get_clean_rows()")
data("iris_newdata")

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
