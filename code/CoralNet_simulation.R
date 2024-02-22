## CoralNet simulation code ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## ZR to do: understand rgamma() and rtruncnorm()
## add site / transect metadata
## better understand gamma and truncnorm params -> connect param values to pop'n means 
## split up residual values into additional columns
## uniform across rows - no signal 





## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
rm(list = ls())

library(truncnorm)
library(tidyverse)
## END start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## params ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
n <- 100
reps <- 50
gamma_mu_1 <- 10
gamma_mu_2 <- 2 
scale <- 2

a <- -Inf ## lower bound
b2 <- (reps - i1) ## upper bound
b3 <- (reps - i1 - i2) 
b4 <- (reps - i1 - i2 - i3)
b5 <- (reps - i1 - i2 - i3 - i4)

norm_mu_2 <- 10
norm_sd_2 <- 1

norm_mu_3 <- 5
norm_sd_3 <- 1

norm_mu_4 <- 5
norm_sd_4 <- 1

norm_mu_5 <- 5
norm_sd_5 <- 1
## END params ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## simulate ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## simulate gamma with shape and scale
iZ <- rgamma(n, shape = c(
  rep(gamma_mu_1, reps), 
  rep(gamma_mu_2, reps)), scale)


## simulate with a truncated normal distribution, with 50-i1 preventing values 
## would sum greater than 50
i2 <- rtruncnorm(n, a, b2, norm_mu_2, norm_sd_2)


## add a third important variable
i3 <- rtruncnorm(n, a, b3, norm_mu_3, norm_sd_3)


## add a fourth important variable
i4 <- rtruncnorm(n, a, b4, norm_mu_4, norm_sd_4)


## add a fifth important variable
i5 <- rtruncnorm(n, a, b5, norm_mu_5, norm_sd_5)


## r1 rounds out the rest of 50
r1 <- (reps - i1 - i2 - i3 - i4 - i5)


## split up u1 into remaining, unimportant columns
## but how to do so in a way that adds noise? 
#u1 <- r1/5


## bind the simulated columns together
df.1 <- round(cbind(i1, i2, i3, i4, i5, r1), 0)


## check row sums
rowSums(df.1)
## END simulation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##



sites <- rep(c("Site_1", n), ("Site_2", n))



rep(1:4, c(1,1,1,1))

rep(1:4, each = 1, len = 10) 


sites <- rep(c("Site_1", "Site_2"), each = 50, len=100)
transects <- rep(c("T1", "T2", "T3", "T4"), each = 25, len=100)
df.2 <- cbind(df.1, sites, transects)












dat <- as.data.frame(i1)
dat$SU <- seq(1,100,1)



p1 <- ggplot(dat, aes(x=SU, y=i1)) +
  geom_point() + ylim(0,10)

print(p1)




library(tidyverse)

categories <- as.factor(c("i1", "i2", "u1", "u2"))

rows <- map(1:100, ~ sample(categories, 100, replace = TRUE))

df.2 <- lapply(rows, summary)

df.2 <- unlist(df.2)

df.2 <- as.data.frame(matrix(df.2, nrow = 100, ncol = 4))

rowSums(df.2)





