rm(list=ls())

library(boot)
library(rjags)
library(coda)
library(scales)

load.module("glm")

#file.choose() #use this to find your file and create your working directory pathway

setwd("C:/Users/mabe2494/Documents/PhD/Bayesian Course/exercise8/") #set your working directory to your working folder
road_d<-read.csv("invasive_road_data.csv", header=T) #bring in the data
plant_d<-read.csv("invasive_plant_data.csv", header=T) #bring in the data


names(road_d)
#[1] "study.area"   "road.density"
str(road_d)
summary(road_d) 

names(plant_d)
#[1] "plot.id"        "study.area"     "disturbance"    "number.exotics"
str(plant_d)
summary(plant_d) 
plot(plant_d$disturbance, plant_d$number.exotics)

data <- NULL
data <- list(
  number.exotics=plant_d$number.exotics, #the response variable
  y.obs=nrow(plant_d), #the number of observations
  n.groups=nrow(road_d),
  disturbance=plant_d$disturbance, #explanatory variable
  study.area=plant_d$study.area,
  road.density=road_d$road.density,
  x.pred=seq(from=min(plant_d$disturbance), to=max(plant_d$disturbance), .0001)
) 
data

inits <- list(
  list(
    #alpha=1,
    alpha=rep(1,8),
    b_dist=8,
    sigma_alpha=0.1,
    #mu_alpha=8
    a_alpha=1,
    b_alpha=0.4
    
  ))
# 
# inits <- list(
#   list(
#     alpha=1,
#     b1=0.1,
#     b2=0.1
#   ),
#   list(
#     alpha=10,
#     b1=0.005,
#     b2=-0.5
#   ),
#   list(
#     alpha=-1,
#     b1=-0.01,
#     b2=-0.001
#   )
# )


model <- "JAGS_invasivemodel.R"

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
                   variable.names=c("alpha", "b_dist", "a_alpha", "b_alpha"), 
                   n.iter=samples, 
                   thin=n.thin)

summary(zc)
#values without hierarchical model
#         2.5%    25%   50%    75%  97.5%
# alpha  0.5142 0.5768 0.612 0.6455 0.7074
# b_dist 7.8229 8.1075 8.263 8.4045 8.6786

#values with hierarchical model on the intercept but no deterministic
#            2.5%    25%    50%    75%  97.5%
# alpha[1] 0.6206 0.7124 0.7592 0.8019 0.8890
# alpha[2] 0.3232 0.4062 0.4508 0.4966 0.5803
# alpha[3] 0.5096 0.5868 0.6245 0.6663 0.7314
# alpha[4] 0.5471 0.6244 0.6682 0.7147 0.7951
# alpha[5] 0.4814 0.5779 0.6217 0.6675 0.7543
# alpha[6] 0.4498 0.5340 0.5730 0.6157 0.6945
# alpha[7] 0.4806 0.5603 0.6021 0.6428 0.7177
# alpha[8] 0.5353 0.6118 0.6514 0.6914 0.7673
# b_dist   7.8351 8.0827 8.2338 8.3825 8.6489

#values with hierarchical model with deterministic
#           2.5%    25%    50%    75%  97.5%
# a_alpha  0.6359 0.7554 0.8273 0.9024 1.0565
# alpha[1] 0.6382 0.7287 0.7759 0.8194 0.8982
# alpha[2] 0.3030 0.3862 0.4295 0.4737 0.5526
# alpha[3] 0.4981 0.5823 0.6258 0.6649 0.7393
# alpha[4] 0.5519 0.6383 0.6828 0.7260 0.7992
# alpha[5] 0.4861 0.5799 0.6269 0.6700 0.7511
# alpha[6] 0.4808 0.5667 0.6122 0.6582 0.7404
# alpha[7] 0.4766 0.5574 0.6014 0.6436 0.7212
# alpha[8] 0.4925 0.5828 0.6275 0.6672 0.7404
# b_alpha  0.2366 0.5930 0.8314 1.1498 1.8280
# b_dist   7.8005 8.0669 8.2169 8.3812 8.7132

plot(zc) 

HPDinterval(zc, prob=0.95) #give the 95% CIs
HPDinterval(zc, prob=0.99) #give the 99% CIs

coda.matrix <- as.matrix(zc[[1]]) #collect the chains as a matrix to save or plot
head(coda.matrix)



#sample the MCMC chains using JAGS.samples

zj <- jags.samples(jm, 
                   variable.names=c("out_lambda", "b_dist"), 
                   n.iter=samples, 
                   thin=n.thin)

 


pred_exot <- summary(zj$out_lambda, quantile, c(.025,.5,.975))$stat #get the 95% CIs and median from a jags.samples object
#pred2 <- summary(zj$b2, quantile, c(.025,.5,.975))$stat #get the 95% CIs and median from a jags.samples object

#plotting prediction & 95%CIs using polygon
x <- data$x.pred
y <- pred_exot
#y1 <- pred_exot[,,1]


#plot 
par(mfrow=c(1,1))

#plot for mean
plot(NA,NA, xlim=c(-0.01,0.31) ,ylim=c(-0.05,31), typ="n", xlab="Disturbance", ylab="Number exotic species")
#plot(x,y[2,], col="blue", xlab="Date", ylab="Number of chicks fledged", cex=1.4, typ="l", tck=0.03, bty="l", ylim=c(0,10)) #plot the median

points(plant_d$disturbance, plant_d$number.exotics)

lines(x,y[2,], col="blue")

polygon(c(x,rev(x)), c(y[1,], rev(y[3,])), col=alpha("blue", alpha=0.3), border=alpha("blue", alpha=0.3)) 


# #plot for standard deviation
# plot(NA,NA, xlim=c(-0.5,55) ,ylim=c(-5,105), typ="n", xlab="Year", ylab="SD storm severity")
# points(storm_data$year, storm_data$storm)
# 
# 
# lines(x,pred_sigma[2,], col="blue")
# polygon(x=c(x, rev(x)), y=c(pred_sigma[1,], rev(pred_sigma[3,])), density=30, col="blue", angle=4)


#MCMC convergence diagnostics (from coda.samples())
raftery.diag(zc)
heidel.diag(zc)
gelman.diag(zc) #needs at least 2 chains


#model validation / fit code
#we will get to this in the 'Model Fit' topic
#extracting and plotting the mean, coeff var & sum squares

zj = jags.samples(jm, 
                  variable.names=c("p.mean" ,"mean.y","mean.y.sim", "cv.y", "cv.y.sim", "p.cv", "fit", "fit.sim", "p.fit"), 
                  n.iter=samples, 
                  thin=n.thin)

#plot model fit  (you may need to adjust some of the plotting parameters)
#HINT if you can't see your points in the plot, adjust your xlim=c() and ylim=c() plotting parameters
#means
plot(zj$mean.y,zj$mean.y.sim, xlab="mean data", ylab="mean simulated",cex=.05)
abline(0,1) #want at least 10% on each side of the line (the closer to 50% the better)
p<-summary(zj$p.mean, mean)
text(12,17,paste("P =",round(as.numeric(p[1]),4)), cex=1.0) #adjust the x y coords here as appropriate

#coeff var
plot(zj$cv.y,zj$cv.y.sim, xlab="cv data", ylab="cv simulated",cex=.05)
abline(0,1) #want at least 10% on each side of the line (the closer to 50% the better)
p<-summary(zj$p.cv, mean)
text(0.3,0.45,paste("P =",round(as.numeric(p[1]),4)), cex=1.0) #adjust the x y coords here as appropriate

#sum squares
plot(zj$fit,zj$fit.sim, xlab="fit data", ylab="fit simulated",cex=.05)
abline(0,1) #want at least 10% on each side of the line (the closer to 50% the better)
p<-summary(zj$p.fit, mean)
text(12000,16000,paste("P =",round(as.numeric(p[1]),4)), cex=1.0) #adjust the x y coords here as appropriate
