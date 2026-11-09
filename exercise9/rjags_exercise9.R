rm(list=ls())

library(boot)
library(rjags)
library(coda)
library(scales)

load.module("glm")

#file.choose() #use this to find your file and create your working directory pathway

setwd("C:/Users/mabe2494/Documents/PhD/Bayesian Course/exercise9/") #set your working directory to your working folder
vague_d<-read.csv("vague_prior.csv", header=T) #bring in the data

names(vague_d)
#"X"         "treatment" "weight"   
str(vague_d)
summary(vague_d) 

mean(vague_d$weight[vague_d$treatment=="a"])
mean(vague_d$weight[vague_d$treatment=="b"])
mean(vague_d$weight[vague_d$treatment=="c"])


#plot(storm_data$year, storm_data$storm)

data <- NULL
data <- list(
  y=vague_d$weight, #the response variable
  y.obs=nrow(vague_d), #the number of observations
  b=ifelse(vague_d$treatment=="b", 1, 0),
  c=ifelse(vague_d$treatment=="c", 1, 0)
  #x.pred=seq(from=min(vague_d$year), to=max(storm_data$year), .1)
) 
data

inits <- list(
  list(
    alpha=4,
    treat_b=0.09,
    treat_c=0.1,
    sigma=40
  ))

# inits <- list(
#   list(
#    alpha=3,
#    b_year=80,
#    sigma=-50
#   ),
#   list(
#    alpha=3,
#    b_year=80,
#    sigma=-50
#   ),
#   list(
#    alpha=3,
#    b_year=80,
#    sigma=-50
#   ),
# )


model <- "JAGS_priorsmodel.R"


jm <- jags.model(model,
                 data=data, 
                 n.adapt=5000, 
                 inits=inits, 
                 n.chains=1
                 #n.chains=3
) 


burn.in <- 10000

update(jm, n.iter=burn.in) 


samples <- 10000
n.thin <- 5

#sample the MCMC chains using CODA

zc <- coda.samples(jm,
                   variable.names=c("alpha","treat_b", "treat_c", "output_a", "output_b", "output_c", "sigma" ), 
                   n.iter=samples, 
                   thin=n.thin)

summary(zc)

#with the wrong priors
#          Mean     SD Naive SE Time-series SE
# alpha    416.0  7.948   0.1777         0.1777
# output_a 416.0  7.948   0.1777         0.1777
# output_b 959.0 10.839   0.2424         0.2713
# output_c 687.1  6.970   0.1559         0.1559
# treat_b  542.9 15.160   0.3390         0.3804
# treat_c  271.1 11.043   0.2469         0.2569
# 
# 2. Quantiles for each variable:
#   
#   2.5%   25%   50%   75% 97.5%
# alpha    401.7 410.5 415.9 421.3 432.8
# output_a 401.7 410.5 415.9 421.3 432.8
# output_b 935.8 951.7 960.0 966.5 978.7
# output_c 673.2 682.4 687.3 691.7 700.9
# treat_b  511.8 532.8 543.9 553.8 570.5
# treat_c  249.1 263.9 271.6 278.5 291.8

#wíth the real flat priors
#           Mean    SD Naive SE Time-series SE
# alpha    394.84 6.068  0.13568        0.13568
# output_a 394.84 6.068  0.13568        0.13568
# output_b 993.78 6.435  0.14389        0.14389
# output_c 699.50 5.483  0.12261        0.12954
# sigma     32.31 2.534  0.05667        0.05422
# treat_b  598.94 8.820  0.19723        0.19723
# treat_c  304.66 8.182  0.18295        0.18924
# 
# 2. Quantiles for each variable:
#   
#   2.5%    25%    50%    75%  97.5%
# alpha    383.11 390.62 394.75 398.88  406.6
# output_a 383.11 390.62 394.75 398.88  406.6
# output_b 980.41 989.59 993.71 997.98 1005.9
# output_c 688.50 695.81 699.42 703.27  710.2
# sigma     27.79  30.51  32.16  33.97   37.8
# treat_b  581.36 593.26 599.01 604.72  616.0
# treat_c  288.30 299.23 304.99 309.89  320.1

####       plot the priors       ####
coda.matrix <- as.matrix(zc[[1]]) #collect the chains as a matrix to save or plot
head(coda.matrix)

par(mfrow=c(1,1))

plot(density(coda.matrix[,1]), xlim=c(-200,700))
prior_alpha <- density(coda.matrix[,2])
lines(x=prior_alpha$x, y=prior_alpha$y, lty=2, col="red")

plot(density(coda.matrix[,6]), xlim=c(-50,200))
prior_sigma <- density(coda.matrix[,7])
lines(x=prior_sigma$x, y=prior_sigma$y, lty=2, col="red")

plot(zc) 

#HPDinterval(zc, prob=0.95) #give the 95% CIs
#HPDinterval(zc, prob=0.99) #give the 99% CIs
#         lower        upper
# alpha  0.90599973  1.779518366
# b1     0.01541111  0.603563987
# b2    -0.04509379 -0.000602824
# attr(,"Probability")
# [1] 0.99

coda.matrix <- as.matrix(zc[[1]]) #collect the chains as a matrix to save or plot
head(coda.matrix)


#sample the MCMC chains using JAGS.samples

zj <- jags.samples(jm, 
                   variable.names=c("alpha", "output_a", "output_b", "output_c", "diff_ba", "diff_ca", "diff_bc"), 
                   n.iter=samples, 
                   thin=n.thin)

ecdf(zj$diff_ca)(0) #it is always more than 0, then treatment c has a higher mean than treatment a
ecdf(zj$diff_ba)(0) #it is always more than 0, then treatment b has a higher mean than treatment a
ecdf(zj$diff_bc)(0) #it is always more than 0, then treatment b has a higher mean than treatment c

pred_ca <- summary(zj$diff_ca, quantile, c(.025,.5,.975))$stat #get the 95% CIs and median from a jags.samples object
pred_sigma <- summary(zj$output_sigma, quantile, c(.025,.5,.975))$stat #get the 95% CIs and median from a jags.samples object
sim_out <- summary(zj$sim_out, quantile, c(0.025, 0.25, 0.5, 0.75, 0.975))$stat #get the 95% CIs and median from a jags.samples object

#plotting prediction & 95%CIs using polygon
x <- data$x.pred
y <- pred_storm


#plot 
par(mfrow=c(1,2))

#plot for mean
plot(NA,NA, xlim=c(-0.5,55) ,ylim=c(-0.5,105), typ="n", xlab="Year", ylab="Storm severity")
#plot(x,y[2,], col="blue", xlab="Date", ylab="Number of chicks fledged", cex=1.4, typ="l", tck=0.03, bty="l", ylim=c(0,10)) #plot the median

points(storm_data$year, storm_data$storm)

lines(x,y[2,], col="blue")

polygon(c(x,rev(x)), c(y[1,], rev(y[3,])), col=alpha("blue", alpha=0.1), border=alpha("blue", alpha=0.1)) 


#plot for standard deviation
plot(NA,NA, xlim=c(-0.5,55) ,ylim=c(-5,105), typ="n", xlab="Year", ylab="SD storm severity")
points(storm_data$year, storm_data$storm)


lines(x,pred_sigma[2,], col="blue")
polygon(x=c(x, rev(x)), y=c(pred_sigma[1,], rev(pred_sigma[3,])), density=30, col="blue", angle=4)


# 





#plot for simulated data to check that it matches my data
par(mfrow=c(1,1))

plot(x=storm_data$year, y=storm_data$storm, typ="p", pch=16, col=alpha("blue", 0.4))
lines(x=data$x.pred, y=sim_out[2,], lty=2, col="red", lwd=2)
lines(x=data$x.pred, y=sim_out[4,], lty=2, col="red", lwd=2)
lines(x=data$x.pred, y=sim_out[1,], lty=2, col="dark blue", lwd=2)
lines(x=data$x.pred, y=sim_out[5,], lty=2, col="dark blue", lwd=2)
polygon(x=c(x, rev(x)), y=c(sim_out[1,], rev(sim_out[5,])), col= alpha("dark blue", alpha=0.1))




#MCMC convergence diagnostics (from coda.samples())
raftery.diag(zc)
heidel.diag(zc)
gelman.diag(zc) #needs at least 2 chains