context("record()")

# record iris data set.
tape <- record(iris)

expect_is(tape, "data.tape")
