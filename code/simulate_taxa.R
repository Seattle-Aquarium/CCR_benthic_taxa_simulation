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
colnames(df.1)[5] <- "small_rocks"
colnames(df.1)[6] <- "boulder_bedrock"

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

## Magnolia: 
## Red = 3, Sugar = 25, Green = 1, Soft sediment = 20, Rock = 5, Bedrock = 1


## red algae 
gamma_mu_S1 <- 3   
gamma_mu_S2 <- 5
gamma_mu_S3 <- 10
gamma_mu_S4 <- 15
gamma_mu_S5 <- 1
gamma_mu_S6 <- 5
gamma_mu_S7 <- 10
gamma_mu_S8 <- 1

## sugar kelp
norm_mu_2_S1 <- 25  
norm_mu_2_S2 <- 5
norm_mu_2_S3 <- 10
norm_mu_2_S4 <- 15
norm_mu_2_S5 <- 1
norm_mu_2_S6 <- 5
norm_mu_2_S7 <- 10
norm_mu_2_S8 <- 15

## green algae, ulva
norm_mu_3_S1 <- 1   
norm_mu_3_S2 <- 10
norm_mu_3_S3 <- 15
norm_mu_3_S4 <- 5
norm_mu_3_S5 <- 2
norm_mu_3_S6 <- 5
norm_mu_3_S7 <- 4
norm_mu_3_S8 <- 2

## soft sediment
norm_mu_4_S1 <- 20   
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
norm_mu_6_S1 <- 1   
norm_mu_6_S2 <- 10
norm_mu_6_S3 <- 15
norm_mu_6_S4 <- 5
norm_mu_6_S5 <- 2
norm_mu_6_S6 <- 5
norm_mu_6_S7 <- 4
norm_mu_6_S8 <- 2

## variance parameters 
scale <- 2
i2_sd <- 2
i3_sd <- 2
i4_sd <- 2
i5_sd <- 2
i6_sd <- 2
## END params ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## invoke functrions to simulate data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ## 
## simulate important variables
df.1 <- simulate.CoralNet(n, 50)


## simulate unimportant variables and bind together data
dat <- simulate.remainder(50, c("remainder"))


## check row sums
dat$row_sum <- rowSums(dat)
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
#abundances <- as.data.frame(cbind(bull_kelp, kelp_crabs))

#dat <- cbind(dat, abundances)

setwd(data_output)
metadata <- read.csv("metadata.csv")

dat <- cbind(dat, metadata)
dat$SU <- seq(1:nrow(dat))
## END abundances ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## plot data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
my.theme = theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(), 
                 axis.line = element_line(colour = "black"),
                 axis.title.x = element_text(size=15),
                 axis.text.x = element_text(size=15),
                 axis.title.y = element_text(size=15),
                 axis.text.y = element_text(size=15))


legend.theme <- theme(#legend.position=c(1,1),
                     #legend.justification=c(.98,.98), 
                     legend.text=element_text(size=15), 
                     legend.title = element_text(size=15),
                     #legend.title.align = 0.5, 
                     #legend.key.height = unit(.2, "cm"),
                     legend.key=element_rect(fill = FALSE, color=FALSE), 
                     legend.background=element_rect(fill=FALSE, color=FALSE))





S1 <- filter(dat, site %in% c("1"))

S1T1 <- filter(S1, key %in% c("1_1"))
  
S1T1_comm <- subset(S1T1, select=c("red_algae", "sugar_kelp", 
                                   "green_algae", "soft_sediment", 
                                   "small_rocks", "boulder_bedrock",
                                   "remainder", "SU"))

S1T1_comm <- melt(S1T1_comm, id.vars="SU")


graphics.off()
windows(12,7)

pal <- c("#ab2221", "#be5e00", "#b3a100", "#4d6934", "#3386a2", "#803280", "#000000")
my.cols <- scale_color_manual(values=pal)



# Everything on the same plot
p1 <- ggplot(S1T1_comm, aes(SU, value, col=variable)) + 
  my.theme + geom_point(size=1.75) + my.cols + legend.theme +
  xlab("Simulated images") + ylab("Percent-cover points")

print(p1)



graphics.off()
windows(12,7)

p1 <- ggplot(dat, aes(SU, sugar_kelp)) +
  geom_point(alpha=0.75) + my.theme + 
  xlab("Simulated images") + ylab("Sugar kelp percent-cover per image")

print(p1)
