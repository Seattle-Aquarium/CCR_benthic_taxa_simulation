## CoralNet simulation code ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
rm(list = ls())

library(truncnorm)
library(tidyverse)

## hardcode relative file paths
code <- "../code"
data_input <- "../data_input"
data_output <- "../data_output"
figs <- "../figs"

## pull in row length csv
setwd(data_output)
rows <- read.csv("nrows_site.csv")
## END start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## simulate ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## simulate gamma with shape and scale

simulate.CoralNet <- function(total_n, col_sum){

i1 <- round(rgamma(total_n, shape = c(rep(gamma_mu_S1, rows[1,2]), 
                                      rep(gamma_mu_S2, rows[2,2]),
                                      rep(gamma_mu_S3, rows[3,2]),
                                      rep(gamma_mu_S4, rows[4,2]),
                                      rep(gamma_mu_S5, rows[5,2]),
                                      rep(gamma_mu_S6, rows[6,2]),
                                      rep(gamma_mu_S7, rows[7,2]),
                                      rep(gamma_mu_S8, rows[8,2])), 
                     scale), 0)
  
b2 <- (col_sum - i1) 

i2 <- round(rtruncnorm(total_n, a, b2, 
                       mean = c(rep(norm_mu_2_S1, rows[1,2]), 
                                rep(norm_mu_2_S2, rows[2,2]),
                                rep(norm_mu_2_S3, rows[3,2]),
                                rep(norm_mu_2_S4, rows[4,2]),
                                rep(norm_mu_2_S5, rows[5,2]), 
                                rep(norm_mu_2_S6, rows[6,2]),
                                rep(norm_mu_2_S7, rows[7,2]),
                                rep(norm_mu_2_S8, rows[8,2])),
                       sd = i2_sd), 0)

b3 <- (col_sum - i1 - i2) 

i3 <- round(rtruncnorm(total_n, a, b3, 
                       mean = c(rep(norm_mu_3_S1, rows[1,2]), 
                                rep(norm_mu_3_S2, rows[2,2]),
                                rep(norm_mu_3_S3, rows[3,2]),
                                rep(norm_mu_3_S4, rows[4,2]),
                                rep(norm_mu_3_S5, rows[5,2]), 
                                rep(norm_mu_3_S6, rows[6,2]),
                                rep(norm_mu_3_S7, rows[7,2]),
                                rep(norm_mu_3_S8, rows[8,2])),
                       sd = i3_sd), 0)

b4 <- (col_sum - i1 - i2 - i3)

i4 <- round(rtruncnorm(total_n, a, b4, 
                       mean = c(rep(norm_mu_4_S1, rows[1,2]), 
                                rep(norm_mu_4_S2, rows[2,2]),
                                rep(norm_mu_4_S3, rows[3,2]),
                                rep(norm_mu_4_S4, rows[4,2]),
                                rep(norm_mu_4_S5, rows[5,2]), 
                                rep(norm_mu_4_S6, rows[6,2]),
                                rep(norm_mu_4_S7, rows[7,2]),
                                rep(norm_mu_4_S8, rows[8,2])),
                       sd = i4_sd), 0)

b5 <- (col_sum - i1 - i2 - i3 - i4)

i5 <- round(rtruncnorm(total_n, a, b5, 
                       mean = c(rep(norm_mu_5_S1, rows[1,2]), 
                                rep(norm_mu_5_S2, rows[2,2]),
                                rep(norm_mu_5_S3, rows[3,2]),
                                rep(norm_mu_5_S4, rows[4,2]),
                                rep(norm_mu_5_S5, rows[5,2]), 
                                rep(norm_mu_5_S6, rows[6,2]),
                                rep(norm_mu_5_S7, rows[7,2]),
                                rep(norm_mu_5_S8, rows[8,2])),
                       sd = i5_sd), 0)

b6 <- (col_sum - i1 - i2 - i3 - i4 - i5)

i6 <- round(rtruncnorm(total_n, a, b6, 
                       mean = c(rep(norm_mu_6_S1, rows[1,2]), 
                                rep(norm_mu_6_S2, rows[2,2]),
                                rep(norm_mu_6_S3, rows[3,2]),
                                rep(norm_mu_6_S4, rows[4,2]),
                                rep(norm_mu_6_S5, rows[5,2]), 
                                rep(norm_mu_6_S6, rows[6,2]),
                                rep(norm_mu_6_S7, rows[7,2]),
                                rep(norm_mu_6_S8, rows[8,2])),
                       sd = i6_sd), 0)

df.1 <- cbind(i1, i2, i3, i4, i5, i6)
df.1[is.na(df.1)] <- 0

colnames(df.1)[1] <- "red_algae"
colnames(df.1)[2] <- "sugar_kelp"
colnames(df.1)[3] <- "green_algae"
colnames(df.1)[4] <- "soft_sediment"
colnames(df.1)[5] <- "rock"
colnames(df.1)[6] <- "hard_substrate"

return(df.1)
}


## function to simulate "unimportant" variables that round each row 
## (each image) up to num.vars i.e. the number of data points
simulate.remainder <- function(col_sum, num.vars){
  
  remainder <- (col_sum - df.1[,1] - df.1[,2] - df.1[,3] - df.1[,4] - df.1[,5] - df.1[,6])
  categories <- as.factor(num.vars)
  rows <- map(remainder, ~ sample(categories, ., replace = TRUE))
  df.2 <- lapply(rows, summary)
  df.2 <- as.data.frame(do.call(rbind, df.2))
  
  dat <- cbind(df.1, df.2)
  return(dat)
}


## function to simulate abundances across eight sites
simulate.abundances <- function(n){
  
spp <- rpois(n, lambda = c(rep(lam_1, rows[1,2]),
                           rep(lam_2, rows[2,2]), 
                           rep(lam_3, rows[3,2]), 
                           rep(lam_4, rows[4,2]),
                           rep(lam_5, rows[5,2]),
                           rep(lam_6, rows[6,2]),
                           rep(lam_7, rows[7,2]),
                           rep(lam_8, rows[8,2])))
as.data.frame(spp)
return(spp)
}
## END functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## params ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## total number of rows
n <- sum(rows[2])

## lower bound
a <- 0

## red algae 
gamma_mu_S1 <- 1   
gamma_mu_S2 <- 5
gamma_mu_S3 <- 10
gamma_mu_S4 <- 15
gamma_mu_S5 <- 2
gamma_mu_S6 <- 3
gamma_mu_S7 <- 2
gamma_mu_S8 <- 5

## sugar kelp
norm_mu_2_S1 <- 5   
norm_mu_2_S2 <- 10
norm_mu_2_S3 <- 15
norm_mu_2_S4 <- 5
norm_mu_2_S5 <- 2
norm_mu_2_S6 <- 5
norm_mu_2_S7 <- 4
norm_mu_2_S8 <- 2

## green algae, ulva
norm_mu_3_S1 <- 5   
norm_mu_3_S2 <- 10
norm_mu_3_S3 <- 15
norm_mu_3_S4 <- 5
norm_mu_3_S5 <- 2
norm_mu_3_S6 <- 5
norm_mu_3_S7 <- 4
norm_mu_3_S8 <- 2

## soft sediment
norm_mu_4_S1 <- 5   
norm_mu_4_S2 <- 10
norm_mu_4_S3 <- 15
norm_mu_4_S4 <- 5
norm_mu_4_S5 <- 2
norm_mu_4_S6 <- 5
norm_mu_4_S7 <- 4
norm_mu_4_S8 <- 2

## rock
norm_mu_5_S1 <- 5   
norm_mu_5_S2 <- 10
norm_mu_5_S3 <- 15
norm_mu_5_S4 <- 5
norm_mu_5_S5 <- 2
norm_mu_5_S6 <- 5
norm_mu_5_S7 <- 4
norm_mu_5_S8 <- 2

## bedrock, hard substrate
norm_mu_6_S1 <- 5   
norm_mu_6_S2 <- 10
norm_mu_6_S3 <- 15
norm_mu_6_S4 <- 5
norm_mu_6_S5 <- 2
norm_mu_6_S6 <- 5
norm_mu_6_S7 <- 4
norm_mu_6_S8 <- 2

## variance parameters 
scale <- 1
i2_sd <- 1
i3_sd <- 1
i4_sd <- 1
i5_sd <- 1
i6_sd <- 1
## END params ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## invoke functrions to simulate data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ## 
## simulate important variables
df.1 <- simulate.CoralNet(n, 50)


## simulate unimportant variables and bind together data
dat <- simulate.remainder(50, c("r1"))


## check row sums
rowSums(dat)
## END data simulation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## params for abundance simulation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## row length
n <- sum(rows[2])

## mean params
lam_1 <- 20
lam_2 <- 2
lam_3 <- 10
lam_4 <- 1
lam_5 <- 35
lam_6 <- 4
lam_7 <- 50
lam_8 <- 15
## END params ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## invoke function ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
bull_kelp <- simulate.abundances(n)
plot(bull_kelp)

kelp_crabs <- simulate.abundances(n)
plot(kelp_crabs)
## END abundance invocation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## bind data frames and process ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
abundances <- as.data.frame(cbind(bull_kelp, kelp_crabs))

dat <- cbind(dat, abundances)

setwd(data_output)
metadata <- read.csv("metadata.csv")

dat <- cbind(dat, abundances, metadata)
## END abundances ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

