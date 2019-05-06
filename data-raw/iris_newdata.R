# create copy of `iris` data set.
iris_newdata <- iris

# create samples of row indices.
samples <- lapply(1:4, function(x) {
  set.seed(x) 
  sample(nrow(iris_newdata), 20, replace = FALSE)})

# create numeric values without range, -Inf and Inf.
iris_newdata$Sepal.Width[samples[[1]]] <- -Inf

# insert NA's in numeric vector.
iris_newdata$Petal.Width[samples[[2]]] <- Inf
iris_newdata$Petal.Width[samples[[3]]] <- NA_real_

# simulate new level in factor variable.
Species_newlevel <- as.character(iris_newdata$Species)
Species_newlevel[samples[[4]]] <- "bogus"
Species_newlevel <- as.factor(Species_newlevel)
iris_newdata$Species <- Species_newlevel

# insert new column.
iris_newdata$junk <- seq_len(nrow(iris_newdata))

# remove existing column.
iris_newdata <- iris_newdata[names(iris_newdata) != "Petal.Length"]

# save data set to package.
# usethis::use_data(iris_newdata, overwrite = TRUE)