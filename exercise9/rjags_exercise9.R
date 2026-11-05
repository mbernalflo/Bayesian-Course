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
    alpha=1,
    treat_b=0.09,
    treat_c=0.1,
    sigma=0.1
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
                   variable.names=c("alpha","treat_b", "treat_c"), 
                   n.iter=samples, 
                   thin=n.thin)

summary(zc)
#                2.5%      25%     50%     75%   97.5%
# alpha       36.63136 39.16649 40.5256 41.9749 44.5479
# alpha_sigma 12.32833 14.29096 15.3396 16.4245 18.7487
# b_sigma      0.09683  0.17925  0.2257  0.2707  0.3524
# b_year      -0.07539  0.03338  0.0882  0.1435  0.2442

#                  2.5%       25%       50%      75%   97.5%
# alpha       36.309076 39.065833 40.586227 42.01278 44.6704
# alpha_sigma  2.623719  2.723702  2.781597  2.83993  2.9501
# b_sigma      0.004141  0.007856  0.009834  0.01175  0.0158
# b_year      -0.067036  0.029808  0.087819  0.14477  0.2582

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
                   variable.names=c("alpha", "b_year", "output_storm", "output_sigma", "sim_out"), 
                   n.iter=samples, 
                   thin=n.thin)



pred_storm <- summary(zj$output_storm, quantile, c(.025,.5,.975))$stat #get the 95% CIs and median from a jags.samples object
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