library(testthat)
library(tibble)
library(dplyr)

context("Process testing")

data(iris)

iris$Species <- as.character(iris$Species)
iris$s3 <- sample(letters, nrow(iris), replace = TRUE)
iris2 <- iris %>% 
  as_tibble %>%
  normalize(collapse_on="Species", collapse_name = "data")

expect_equal(c(3, 2), dim(iris2))
expect_equal(c(50, 5), dim(iris2$data[[1]]))

iris2 <- iris %>% as_tibble() %>%
  select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
  
iris2$rownum <- seq_len(nrow(iris2))
expect_equal(iris2, normalize(iris2, collapse_on = "rownum", 
                              collapse_name = "data"))


