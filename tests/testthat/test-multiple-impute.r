library(testthat)
library(dplyr)

# Create the data from the online simputation example.

dat <- iris
# empty a few fields
dat[1:3, 1] <- dat[3:7, 2] <- dat[8:10, 5] <- NA

# Imputation model description.
impute_form <- Species + Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width

mi <- impute_n_times(dat, impute_form, impute_rf)

# Impute using multiple models and combine them.
mit <- multiple_impute(dat, impute_form) %>%
  combine_mi_tibble() 

mit$Sepal.Length2 <- mit$Sepal.Length + rnorm(150, sd = 0.1)

# Orthogonalize columns based on Louvain clustering of the correlation
# matrix.
mit %>% 
  group_numeric_vars( ~ .) %>%
  orthogonalize_columns() %>%
  as_tibble()

# Orthogonalize all columns
mit %>% 
  orthogonalize_columns() %>%
  as_tibble()

# Orthogonalize Sepal.Length and Petal.Length along with 
# Sepal.Width and Petal.Width.
mit %>% 
  orthogonalize_columns( ~ Sepal.Length + Petal.Length | .) %>%
  orthogonalize_columns( ~ Sepal.Width + Petal.Width | .) %>%
  as_tibble()

# Orthogonalize Sepal.Length and Petal.Length along with 
# Sepal.Width and Petal.Width prioritizing Petal.Length and Petal.Width
imp_ordering <- c("Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width")
mit %>% 
  orthogonalize_columns( ~ Sepal.Length + Petal.Length | .,
    imp_ordering = imp_ordering) %>%
  orthogonalize_columns( ~ Sepal.Width + Petal.Width | .,
    imp_ordering = imp_ordering) %>%
  as_tibble()

attributes(mit)$colinear_groups

  
