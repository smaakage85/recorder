#' Record Statistics and Meta Data of Variables in Training Data
#' 
#' Records statistics and meta data of variables in the training data for a 
#' predictive model. The recorded data can then be used to compute a set 
#' of validation tests on new data with \code{\link{play}}.
#'
#' @param x training data (or just a single variable from the training data) to 
#' record the statistics and other relevant meta data of.
#' @param ... further arguments passed to or from other methods.
#'
#' @return \code{list} recorded statistics and meta data. The list will inherit
#' from the \code{data.tape} class when the function is invoked with a 
#' \code{data.frame}.
#' 
#' @export
#' 
#' @examples
#' record(iris)
record <- function (x, ...) {
  UseMethod("record", x)
}

#' Record Statistics and Meta Data of a Numeric
#'
#' Records statistics and meta data of a \code{numeric}.
#' 
#' @param x \code{numeric}
#' @param ... all further arguments.
#'
#' @method record numeric
#' 
#' @return \code{list} recorded statistics and meta data.
#'
#' @export
#' 
#' @examples
#' record(iris$Sepal.Length)
record.numeric <- function(x, ...) {

  # record statistics and meta data.
  list(
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    any_NA = any_NA(x)
  )

}

#' Record Statistics and Meta Data of an Integer
#'
#' Records statistics and meta data of an \code{integer}.
#' 
#' @param x \code{integer}
#' @param ... all further arguments.
#'
#' @method record integer
#' 
#' @return \code{list} recorded statistics and meta data.
#'
#' @export
#' 
#' @examples
#' record(c(1:10, NA_integer_))
record.integer <- function(x, ...) {

  # record statistics and meta data.
  list(
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    any_NA = any_NA(x)
  )

}

#' Record Statistics and Meta Data of a Factor
#'
#' Records statistics and meta data of a \code{factor}.
#' 
#' @param x \code{factor}
#' @param ... all further arguments.
#'
#' @method record factor
#' 
#' @return \code{list} recorded statistics and meta data.
#'
#' @export
#' 
#' @examples
#' record(iris$Species)
record.factor <- function(x, ...) {

  # record statistics and meta data.
  list(
    levels = levels(x),
    any_NA = any_NA(x)
  )

}

#' Record Statistics and Meta Data of a Character
#'
#' Records statistics and meta data of a \code{character}.
#' 
#' @param x \code{character}
#' @param ... all further arguments.
#'
#' @method record character
#' 
#' @return \code{list} recorded statistics and meta data.
#'
#' @export
#' 
#' @examples
#' record(letters)
record.character <- function(x, ...) {

  # record statistics and meta data.
  list(
    unique_values = unique(x),
    any_NA = any_NA(x)
  )

}

#' Record Statistics and Meta Data 
#'
#' Records statistics and meta data.
#' 
#' @param x anything.
#' @param ... all further arguments.
#'
#' @method record default
#' 
#' @return \code{list} recorded statistics and meta data.
#'
#' @export
#' 
#' @examples
#' some_junk_letters <- letters[1:10]
#' class(some_junk_letters) <- "junk"
#' record(some_junk_letters)
record.default <- function(x, ...) {

  # record statistics and meta data.
  list(
    any_NA = any_NA(x)
  )

}

#' Record Statistics and Meta Data of a Data Frame
#'
#' Records Statistics and meta data of a data.frame.
#' 
#' @param x \code{data.frame} training data for predictive model.
#' @param verbose \code{logical} should messages be printed?
#' @param ... all further arguments.
#'
#' @method record data.frame
#' 
#' @return \code{list} recorded statistics and meta data. 
#'
#' @export
#' 
#' @examples
#' record(iris)
record.data.frame <- function(x, verbose = TRUE, ...) {

  if (verbose) {
    cat(bgMagenta("\n[RECORD]\n\n"))
    cat("... recording meta data and statistics of", nrow(x), 
        "rows with", ncol(x), "columns... \n\n")
  }
  
  # validate input.
  if (nrow(x) == 0) {
    stop("Number of rows must be greather than zero.")
  }
  
  # check, if there are any variables, that only consist of NA's => warning.
  only_na <- vapply(x, function(x) {all(is.na(x))}, FUN.VALUE = logical(1))
  only_na <- names(only_na)[only_na]
  if (length(only_na) > 0) {
    warning("The following variables consist solely of NA's. Are you sure, ",
            "you want them in your data set? Variables: ", 
            paste0(only_na, collapse = ", "))
  }
  
  # record classes of indvidual variables.
  class_variables <- lapply(x, class)

  # record statistics and meta data of all variables.
  parameters <- lapply(x, record)

  # combine results into one list, the structure of which defines the 
  # 'data.tape' class by convention.
  data.tape <- list(class_variables = class_variables, 
                    parameters = parameters)

  # set class.
  class(data.tape) <- append(class(data.tape), "data.tape")
  
  if (verbose) {
    cat(bgMagenta("[STOP]\n"))
  }
  
  # return data.tape.
  data.tape

}

any_NA <- function(x) {any(is.na(x))}