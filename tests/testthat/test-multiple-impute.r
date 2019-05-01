library(testthat)
library(dplyr)
library(purrr)
library(normalizer)


# Create the data from the online simputation example.

dat <- iris
# empty a few fields
dat[1:3, 1] <- dat[3:7, 2] <- dat[8:10, 5] <- NA

form <- Species + Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width

mi <- impute_n_times(dat, form, impute_rf)

# Create a list of imputed data sets.
mis <- multiple_impute(dat, form)

expect_true(inherits(mi, "data.frame"))

mis %>% mutate(gs = map(data, gram_schmidt))
