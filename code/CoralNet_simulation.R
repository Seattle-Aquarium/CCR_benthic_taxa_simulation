## CoralNet simulation code ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## ZR to do: understand rgamma() and rtruncnorm()
## generalize to add additional columns
## add site / transect metadata



## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
library(truncnorm)
library(tidyverse)
## END start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## params ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
n <- 100
times <- 50
gamma_mu_1 <- 4
gamma_mu_2 <- 2 
scale <- 2

a <- -Inf ## what is this param in rtruncnorm
b <- (50 - i1) ## what is this param in rtruncnorm
norm_mu <- 10
norm_sd <- 1
## END params ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## simulate ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## simulate gamma with shape and scale
i1 <- rgamma(n, shape = c(rep(mu_1, times), rep(mu_2, times)), scale)


## simulate with a truncated normal distribution, with 50-i1 preventing values 
## would sum greater than 50
i2 <- rtruncnorm(n, a, b, norm_mu, norm_sd)


## u1 rounds out the rest of 50
u1 <- 50 - i1 - i2


## bind the simulated columns together
df.1 <- cbind(i1, i2, u1)


## check row sums
rowSums(df.1)
## END simulation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##






library(tidyverse)

categories <- as.factor(c("i1", "i2", "u1", "u2"))

rows <- map(1:100, ~ sample(categories, 50, replace = TRUE))

df.2 <- lapply(rows, summary)

df.2 <- unlist(df.2)

df.2 <- as.data.frame(matrix(df.2, nrow = 100, ncol = 4))