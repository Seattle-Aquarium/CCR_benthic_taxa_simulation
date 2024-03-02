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


## hardcode relative file paths
code <- "../code"
data_input <- "../data_input"
data_output <- "../data_output"
figs <- "../figs"
## END start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## simulate ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## simulate gamma with shape and scale

simulate.impt.vars <- function(n, reps, col_sum){

i1 <- round(rgamma(n, shape = c(
  rep(gamma_mu_1, reps), 
  rep(gamma_mu_2, reps)), scale),0)


## use i1 to set upper bound
b2 <- (col_sum - i1) 

## simulate with a truncated normal distribution, with 50-i1 preventing values 
## would sum greater than 50
i2 <- round(rtruncnorm(n, a, b2, norm_mu_2, norm_sd_2), 0)
b3 <- (col_sum - i1 - i2) 

## add a third important variable
i3 <- round(rtruncnorm(n, a, b3, norm_mu_3, norm_sd_3), 0)
b4 <- (col_sum - i1 - i2 - i3)

## add a fourth important variable
i4 <- round(rtruncnorm(n, a, b4, norm_mu_4, norm_sd_4), 0)
b5 <- (col_sum - i1 - i2 - i3 - i4)

## add a fifth important variable
i5 <- round(rtruncnorm(n, a, b5, norm_mu_5, norm_sd_5), 0)
b6 <- (col_sum - i1 - i2 - i3 - i4 - i5)

## add a sixth important variable
i6 <- round(rtruncnorm(n, a, b6, norm_mu_6, norm_sd_6), 0)

## bind the simulated columns together
df.1 <- cbind(i1, i2, i3, i4, i5, i6)

## close
return(df.1)
}


## function to simulate "unimportant" variables that round each row 
## (each image) up to num.vars i.e. the number of data points
simulate.unimpt.vars <- function(col_sum, num.vars){
  
  remainder <- (col_sum - df.1[,1] - df.1[,2] - df.1[,3] - df.1[,4] - df.1[,5] - df.1[,6])
  categories <- as.factor(num.vars)
  rows <- map(remainder, ~ sample(categories, ., replace = TRUE))
  df.2 <- lapply(rows, summary)
  df.2 <- as.data.frame(do.call(rbind, df.2))
  
  dat <- cbind(df.1, df.2)
  return(dat)
}





## params ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## gamma distribution boundaries and parameters 
a <- 0
gamma_mu_1 <- 10
gamma_mu_2 <- 2 
scale <- 2


## truncated normal distribution parameters 
norm_mu_2 <- 10
norm_sd_2 <- 1

norm_mu_3 <- 1
norm_sd_3 <- 1

norm_mu_4 <- 20
norm_sd_4 <- 1

norm_mu_5 <- 1
norm_sd_5 <- 1

norm_mu_6 <- 5
norm_sd_6 <- 1
## END params ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## invoke functrions to simulate data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ## 
## simulate important variables
df.1 <- simulate.impt.vars(100, 50, 50)


## simulate unimportant variables and bind together data
dat <- simulate.unimpt.vars(50, c("r1"))


## check row sums
rowSums(dat)
## END data simulation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##










test <- rpois(100, 1)

S1 <- 10#2795
S2 <- 15#2352
S3 <- 20#2815
S4 <- 30#2691

pois.n <- S1 + S2 + S3 + S4

test <- rpois(pois.n, c(100, 5))

test <- rpois(c(S1, S2, S3, S4), 10)

test


## 


i1 <- round(rgamma(n, shape = c(
  rep(gamma_mu_1, reps), 
  rep(gamma_mu_2, reps)), scale),0)



n <- 150
reps <- 50
lam_1 <- 20
lam_2 <- 2
lam_3 <- 10


test <- rpois(n, lambda = c(rep(lam_1, reps),
                            rep(lam_2, reps), 
                            rep(lam_3, reps)))


plot(test)












# add df.2 to df.1 in place of remainder


## add meta data
sites <- rep(c("Site_1", "Site_2"), each = 50, len=100)
transects <- rep(c("T1", "T2", "T3", "T4"), each = 25, len=100)
dat <- cbind(dat, sites, transects)


## plot data 
dat <- as.data.frame(i1)
dat$SU <- seq(1,100,1)


## visualize
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





