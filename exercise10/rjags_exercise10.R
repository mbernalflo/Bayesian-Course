rm(list=ls())

library(boot)
library(rjags)
library(coda)
library(scales)

load.module("glm")

#file.choose() #use this to find your file and create your working directory pathway

setwd("C:/Users/mabe2494/Documents/PhD/Bayesian Course/exercise10/") #set your working directory to your working folder
willow<-read.csv("willow_tit_data.csv", header=T) #bring in the data

names(willow)
#[1] "number_visits"     "number_detections" "elevation"         "forest"       
#[5] "length"            "search_duration"    
str(willow)
summary(willow) 


#plot(storm_data$year, storm_data$storm)

data <- NULL
data <- list(
  y.obs=nrow(willow), #the number of observations
  forest=as.vector(scale(willow$forest)),
  elevation=as.vector(scale(willow$elevation)),
  n_detection=willow$number_detections,
  n_surveys=willow$number_visits,
  sd_elev=sd(willow$elevation),
  mean_elev=mean(willow$elevation),
  x.pred_f=seq(from=min(willow$forest), to=max(willow$forest), .1),
  x.pred_e=seq(from=min(willow$elevation), to=max(willow$elevation), .1),
  x.pred_f_scaled=as.vector(scale(seq(from=min(willow$forest), to=max(willow$forest), .1))),
  x.pred_e_scaled=as.vector(scale(seq(from=min(willow$elevation), to=max(willow$elevation), .1)))
) 
data



inits <- list(
  list(
    intercept=4,
    b_elevation1=0.09,
    b_forest=0.1,
    b_elevation2=40,
    p=0.5,
    z=rep(1,237)
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


model <- "JAGS_willowmodel.R"


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
                   variable.names=c("intercept",
                                    "b_elevation1",
                                    "b_forest",
                                    "b_elevation2",
                                    "p"), 
                   n.iter=samples, 
                   thin=n.thin)

summary(zc)

#                2.5%     25%     50%      75%   97.5%
# b_elevation1  1.4781  1.8451  2.0668  2.28729  2.7360
# b_elevation2 -1.7360 -1.3340 -1.1410 -0.95768 -0.6284
# b_forest      0.4168  0.7130  0.8745  1.04742  1.3551
# intercept    -0.7525 -0.3972 -0.2039 -0.01186  0.3416
# p             0.7260  0.7669  0.7870  0.80666  0.8423


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
                   variable.names=c("output_pz_forest", "output_pz_elev", "b_forest", "b_elevation1", "b_elevation2", "max_e"), 
                   n.iter=samples, 
                   thin=n.thin)

# ecdf(zj$diff_ca)(0) #it is always more than 0, then treatment c has a higher mean than treatment a
# ecdf(zj$diff_ba)(0) #it is always more than 0, then treatment b has a higher mean than treatment a
# ecdf(zj$diff_bc)(0) #it is always more than 0, then treatment b has a higher mean than treatment c

max_e <- summary(zj$max_e, quantile, c(.025,.5,.975))$stat

pred_e <- summary(zj$output_pz_elev, quantile, c(.025,.5,.975))$stat #get the 95% CIs and median from a jags.samples object
pred_f <- summary(zj$output_pz_forest, quantile, c(.025,.5,.975))$stat #get the 95% CIs and median from a jags.samples object
#sim_out <- summary(zj$sim_out, quantile, c(0.025, 0.25, 0.5, 0.75, 0.975))$stat #get the 95% CIs and median from a jags.samples object

#plotting prediction & 95%CIs using polygon
x <- data$x.pred_e
y <- pred_e


#plot 
par(mfrow=c(1,1))

#plot for mean
plot(NA,NA, xlim=c(0,3000) ,ylim=c(0,1), typ="n", xlab="", ylab="")
#plot(x,y[2,], col="blue", xlab="Date", ylab="Number of chicks fledged", cex=1.4, typ="l", tck=0.03, bty="l", ylim=c(0,10)) #plot the median

#points(willow$elevation, willow$storm)

lines(x,y[2,], col="blue")

polygon(c(x,rev(x)), c(y[1,], rev(y[3,])), col=alpha("blue", alpha=0.1), border=alpha("blue", alpha=0.1)) 

max_e <- (-zj$b_elevation1)/(2*zj$b_elevation2)

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