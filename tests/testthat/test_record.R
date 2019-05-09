context("record()")

# record iris data set.
tape <- record(iris, verbose = FALSE)

# basic output tests.
expect_is(tape, "data.tape")
expect_equal(length(tape$parameters), ncol(iris))
expect_true(all(vapply(tape$parameters, length, FUN.VALUE = double(1)) > 0))
expect_error(record(iris[0,], verbose = FALSE))

# test computations.
expect_identical(tape$parameters$Species$levels, levels(iris$Species))
expect_identical(tape$parameters$Petal.Length$max, max(iris$Petal.Length))
expect_identical(tape$parameters$Sepal.Width$min, min(iris$Sepal.Width))

# record tape on twisted iris data set.
data("iris_newdata")
tape_twist <- record(iris_newdata, FALSE)
expect_identical(
  vapply(tape_twist$parameters, '[[', "any_NA", FUN.VALUE = logical(1)),
  vapply(iris_newdata, function(x) {any(is.na(x))}, FUN.VALUE = logical(1))
  )

# test `record` default method.
class(iris_newdata$junk) <- "crap"
tester <- record(iris_newdata$junk, verbose = FALSE)
expect_true("any_NA" %in% names(tester))

# test 'record' on data.frame with one or more only NA columns.
data("iris_newdata")
iris_newdata[] <- NA
expect_warning({tape <- record(iris_newdata, verbose = FALSE)})
expect_is(tape, "data.tape")
expect_equal(tape$parameters$Sepal.Length$any_NA, TRUE)

# test 'record.character'.
df <- data.frame(a = letters[1:10], stringsAsFactors = FALSE)
tape <- record(df, verbose = FALSE)
expect_true(all(c("unique_values","any_NA") %in% names(tape$parameters$a)))
expect_identical(tape$parameters$a$unique_values, unique(df$a))

