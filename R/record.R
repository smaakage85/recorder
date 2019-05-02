#' Record Parameters and Meta Data from Training Data
#' 
#' Records relevant meta data and parameters from training data for a machine 
#' learning model. The recorded data can then be used to compute a set of 
#' validation tests on new data with \code{\link{play}}.
#'
#' @param x training data or just a single variable to record parameters and 
#' other relevant metadata from.
#' @param ... further arguments passed to or from other methods.
#'
#' @return \code{list} recorded parameters and metadata. The list will inherit
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

#' Record Parameters and Meta Data from Numeric
#'
#' Records parameters and meta data from \code{numeric}.
#' 
#' @param x \code{numeric}
#' @param ... all further arguments.
#'
#' @method record numeric
#' 
#' @return \code{list} recorded parameters and meta data.
#'
#' @export
#' 
#' @examples
#' record(iris$Sepal.Length)
record.numeric <- function(x, ...) {

  # record parameters and meta data.
  list(
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    any_NA = any_NA(x)
  )

}

#' Record Parameters and Meta Data from Integer
#'
#' Records parameters and meta data from \code{integer}.
#' 
#' @param x \code{integer}
#' @param ... all further arguments.
#'
#' @method record integer
#' 
#' @return \code{list} recorded parameters and meta data.
#'
#' @export
#' 
#' @examples
#' record(c(1:10, NA_integer_))
record.integer <- function(x, ...) {

  # record parameters and meta data.
  list(
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    any_NA = any_NA(x)
  )

}

#' Record Parameters and Meta Data from Factor
#'
#' Records parameters and meta data from \code{factor}.
#' 
#' @param x \code{factor}
#' @param ... all further arguments.
#'
#' @method record factor
#' 
#' @return \code{list} recorded parameters and meta data.
#'
#' @export
#' 
#' @examples
#' record(iris$Species)
record.factor <- function(x, ...) {

  # record parameters and meta data.
  list(
    levels = levels(x),
    any_NA = any_NA(x)
  )

}

#' Record Parameters and Meta Data from Character
#'
#' Records parameters and meta data from \code{character}.
#' 
#' @param x \code{character}
#' @param ... all further arguments.
#'
#' @method record character
#' 
#' @return \code{list} recorded parameters and meta data. The unique values
#' of the vector are recorded as `levels`.
#'
#' @export
#' 
#' @examples
#' record(letters)
record.character <- function(x, ...) {

  # record parameters and meta data.
  list(
    levels = unique(x),
    any_NA = any_NA(x)
  )

}

#' Record Parameters and Meta Data 
#'
#' Records parameters and meta data.
#' 
#' @param x anything.
#' @param ... all further arguments.
#'
#' @method record default
#' 
#' @return \code{list} recorded parameters and meta data.
#'
#' @export
#' 
#' @examples
#' some_junk_letters <- letters[1:10]
#' class(some_junk_letters) <- "junk"
#' record(some_junk_letters)
record.default <- function(x, ...) {

  # record parameters and meta data.
  list(
    any_NA = any_NA(x)
  )

}

#' Record Parameters and Meta Data from Data Frame
#'
#' Records parameters and meta data from a data.frame.
#' 
#' @param x \code{data.frame} training data for machine learning model.
#' @param verbose \code{logical} should messages be printed?
#' @param ... all further arguments.
#'
#' @method record data.frame
#' 
#' @return \code{list} recorded parameters and meta data. 
#'
#' @export
#' 
#' @examples
#' record(iris)
record.data.frame <- function(x, verbose = TRUE, ...) {

  if (verbose) {
    cat("\n[RECORD]\n\n")
    cat("... recording metadata and statistics for", ncol(x), 
        "columns and", nrow(x), "rows... \n\n")
  }
  
  # record class of data.frame/training data as a whole.
  class_training_data <- class(x)
    
  # record classes of indvidual variables.
  class_variables <- lapply(x, class)

  # record parameters for all variables.
  parameters <- lapply(x, record)

  # combine results into one list, the structure of which defines the 
  # 'data.tape' class by convention.
  data.tape <- list(class_training_data = class_training_data,
                    class_variables = class_variables, 
                    parameters = parameters)

  # set class.
  class(data.tape) <- append(class(data.tape), "data.tape")
  
  if (verbose) {
    cat("[STOP]\n\n")
  }
  
  # return data.tape.
  data.tape

}

any_NA <- function(x) {any(is.na(x))}