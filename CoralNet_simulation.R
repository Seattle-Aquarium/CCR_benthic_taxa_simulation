library(truncnorm)

i1 <- rgamma(n = 100, shape = c(rep(4, times = 50), rep(2, times = 50)), scale = 2)

i2 <- rtruncnorm(n = 100, a = -Inf, b = (50 - i1), mean = 10, sd = 1)

u1 <- 50 - i1 - i2

df.1 <- cbind(i1, i2, u1)

rowSums(df.1)





library(tidyverse)

categories <- as.factor(c("i1", "i2", "u1", "u2"))

rows <- map(1:100, ~ sample(categories, 50, replace = TRUE))

df.2 <- lapply(rows, summary)

df.2 <- unlist(df.2)

df.2 <- as.data.frame(matrix(df.2, nrow = 100, ncol = 4))