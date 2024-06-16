## code to prepare `trialdata` dataset goes here
set.seed(1)
trialdata1 <- expand.grid(row = factor(1:10), col = factor(1:8))
trialdata1$yield <- rnorm(nrow(trialdata1), mean = 10, sd = 2)
trialdata1$genotype <- sample(paste0("G", 1:10), nrow(trialdata1), replace = TRUE)

usethis::use_data(trialdata1, overwrite = TRUE)

set.seed(2)
trialdata2 <- expand.grid(row = factor(1:10), col = factor(1:8))
trialdata2$yield <- rnorm(nrow(trialdata1), mean = 10, sd = 2)
trialdata2$genotype <- sample(paste0("Geno", 1:10), nrow(trialdata1), replace = TRUE)

usethis::use_data(trialdata2, overwrite = TRUE)
