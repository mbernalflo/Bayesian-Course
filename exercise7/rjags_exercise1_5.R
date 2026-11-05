rm(list=ls())

library(boot)
library(rjags)
library(coda)
library(scales)

load.module("glm")

#file.choose() #use this to find your file and create your working directory pathway

setwd("C:/Users/mabe2494/Documents/PhD/Bayesian Course/exercise7/") #set your working directory to your working folder
breed<-read.csv("bird_data.csv", header=T) #bring in the data

names(breed)
#[1] "year"           "nest_success"   "breeding_date"  "fem_age"        "clutch_size"    "chicks_fledged"
str(breed)
summary(breed) 

data <- NULL
data <- list(
  y=breed$chicks_fledged, #the response variable
  y.obs=nrow(breed), #the number of observations
  fem_age=breed$fem_age, #explanatory variable
  breed_date=breed$breeding_date, #explanatory variable
  clutch_size=breed$clutch_size
) 
data

# inits <- list(
#   list(
#     alpha=1,
#     b1=0.1,
#     b2=0.1
#   ))

inits <- list(
  list(
    alpha=1,
    b1=0.1,
    b2=0.1
  ),
  list(
    alpha=10,
    b1=0.005,
    b2=-0.5
  ),
  list(
    alpha=-1,
    b1=-0.01,
    b2=-0.001
  )
)


model <- "JAGS_fledgemodel.R"

jm <- jags.model(model,
                 data=data, 
                 n.adapt=5000, 
                 inits=inits, 
                 #n.chains=1
                 n.chains=3
                 ) 


burn.in <- 10000

update(jm, n.iter=burn.in) 


samples <- 10000
n.thin <- 5

#sample the MCMC chains using CODA

zc <- coda.samples(jm,
                   variable.names=c("alpha", "b1", "b2"), 
                   n.iter=samples, 
                   thin=n.thin)

summary(zc)
plot(zc) 

HPDinterval(zc, prob=0.95) #give the 95% CIs
HPDinterval(zc, prob=0.99) #give the 99% CIs
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
                   variable.names=c("b1", "b2"), 
                   n.iter=samples, 
                   thin=n.thin)

ecdf(zj$b1)(0) #see what proportion of the posterior is below 0 
ecdf(zj$b2)(0) #see what proportion of the posterior is below 0 


pred1 <- summary(zj$b1, quantile, c(.025,.5,.975))$stat #get the 95% CIs and median from a jags.samples object
pred2 <- summary(zj$b2, quantile, c(.025,.5,.975))$stat #get the 95% CIs and median from a jags.samples object


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
