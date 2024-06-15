## code to prepare `metdata` dataset goes here
library(tidyverse)
set.seed(1)

metdata1 <- expand.grid(row = factor(1:10), col = factor(1:8), trial = factor(1:3), year = 2021:2022) |>
  mutate(yield = rnorm(n(), mean = 10, sd = 2),
         genotype = sample(paste0("G", 1:10), n(), replace = TRUE))

usethis::use_data(metdata1, overwrite = TRUE)
