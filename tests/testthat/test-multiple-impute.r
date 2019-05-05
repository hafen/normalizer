library(testthat)
library(dplyr)
library(purrr)

# Create the data from the online simputation example.

dat <- iris
# empty a few fields
dat[1:3, 1] <- dat[3:7, 2] <- dat[8:10, 5] <- NA

# Imputation model description.
impute_form <- Species + Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width

mi <- impute_n_times(dat, impute_form, impute_rf)

# Create an imputed data set.
mi <- multiple_impute(dat, impute_form) %>%
  combine_mi_tibble(mi) %>%
  group_numeric_vars( ~ .) %>%
  orthogonalize_columns()

expect_true(inherits(mi, "data.frame"))

mis <- mis %>% mutate(gs = map(data, gram_schmidt))


## FINISH THIS

impute_form <- Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width

orth_form1 <- 
  ~ Sepal.Length + Sepal.Width | Species + Petal.Length + Petal.Width

orth_form2 <- 
  ~ Petal.Length + Petal.Width | Species + Sepal.Length + Sepal.Width

subtype_form <- Species ~.

dat %>%
  orthogonalize(orth_form1) %>%
  orthogonalize(orth_form2) %>%
  multiple_impute(impute_form)

#  quantum_eraser_learn(subtype_form)
    
  
