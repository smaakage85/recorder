#' Simulate New Data from Iris Data Set
#'
#' @return \code{data.frame} simulated data set.
#' 
#' @export
#'
#' @examples
#' simulate_newdata_iris()
simulate_newdata_iris <- function() {
  
  # create samples of row indices.
  samples <- lapply(1:4, function(x) {
    set.seed(x) 
    sample(nrow(iris), 20, replace = FALSE)})
  
  # create numeric values without range, -Inf and Inf.
  iris$Sepal.Width[samples[[1]]] <- -Inf
  
  # insert NA's in numeric vector.
  iris$Petal.Width[samples[[2]]] <- Inf
  iris$Petal.Width[samples[[3]]] <- NA_real_

  # simulate new level in factor variable.
  Species_newlevel <- as.character(iris$Species)
  Species_newlevel[samples[[4]]] <- "bogus"
  Species_newlevel <- as.factor(Species_newlevel)
  iris$Species <- Species_newlevel
  
  # insert new column.
  iris$junk <- seq_len(nrow(iris))
  
  # remove existing column.
  iris <- iris[names(iris) != "Petal.Length"]
  
  # return simulated data set.
  iris
  
}