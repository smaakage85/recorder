% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/record.R
\name{record.default}
\alias{record.default}
\title{Record Statistics and Meta Data}
\usage{
\method{record}{default}(x, ...)
}
\arguments{
\item{x}{anything.}

\item{...}{all further arguments.}
}
\value{
\code{list} recorded statistics and meta data.
}
\description{
Records statistics and meta data.
}
\examples{
some_junk_letters <- letters[1:10]
class(some_junk_letters) <- "junk"
record(some_junk_letters)
}
