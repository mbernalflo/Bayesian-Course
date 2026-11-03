rm(list=ls())

library(boot)
library(rjags)
library(coda)
library(scales)

load.module("glm")

#file.choose() #use this to find your file and create your working directory pathway

setwd("C:/Users/mabe2494/Documents/PhD/Bayesian Course/exercise3/") #set your working directory to your working folder
legs<-read.csv("legs.csv", header=T) #bring in the data

names(legs)
#[1] "height"    "left_leg"  "right_leg"
str(legs)
summary(legs) 

data <- NULL
data <- list(
  y=legs$height, #the response variable
  y.obs=nrow(legs), #the number of observations
  right_leg=legs$right_leg, #explanatory variable
  #breed_date=as.vector(scale(breed$breeding_date)), #explanatory variable
  left_leg=legs$left_leg
  #x.pred=seq(from=min(as.vector(scale(breed$breeding_date))), to=max(as.vector(scale(breed$breeding_date))), by=0.01)
) 
data


# inits <- list(
#   list(
#     alpha=1,
#     b1=0.1,
#     b2=0.1,
#     b_int=0.1
#   ))

inits <- list(
  list(
    #alpha=1,
    beta_r=0.1,
    #beta_l=0.1,
    sigma=0.1
  ),
  list(
    #alpha=10,
    beta_r=0.005,
    #beta_l=0.005,
    sigma=0.1
  ),
  list(
    #alpha=-1,
    beta_r=-0.01,
    #beta_l=-0.01,
    sigma=0.1
  )
)


model <- "JAGS_legsmodel.R"

jm <- jags.model(model,
                 data=data, 
                 n.adapt=5000, 
                 inits=inits, 
                 #n.chains=1
                 n.chains=3
) 


burn.in <- 100000

update(jm, n.iter=burn.in) 


samples <- 10000
n.thin <- 20

#sample the MCMC chains using CODA

zc <- coda.samples(jm,
                   variable.names=c("beta_r"), 
                   n.iter=samples, 
                   thin=n.thin)

summary(zc)

#first time values
#           Mean      SD Naive SE Time-series SE
# alpha  99.3443 10.6060  0.86598        0.97405
# beta_r  0.9341  0.1394  0.01138        0.01294

# in this case alpha represented the height of someone with no legs

#second time values
#            Mean     SD Naive SE Time-series SE
# alpha  170.0333 0.7658 0.019774       0.021575
# beta_r   0.9312 0.1345 0.003472       0.003472

#my intercept is now a representation of the height at mean leg length (roughly mean of the population)

#third time values

#            Mean    SD Naive SE Time-series SE
# alpha  169.9404 0.804  0.06565        0.07523
# beta_l  -0.8339 1.975  0.16123        0.16477
# beta_r   1.7661 1.963  0.16024        0.16351

#now my beta parameters have huge standard deviations

#fourth time values

#            Mean     SD Naive SE Time-series SE
# alpha  169.9582 0.8087 0.020880       0.020867
# beta_l  -0.8670 1.8389 0.047479       0.046129
# beta_r   1.8003 1.8409 0.047532       0.046137
# combo    0.9333 0.1377 0.003557       0.003557

#combo has a way smaller SD than the two betas, comparable to the SD from when we only had beta_r. The mean value is also comparable between combo and beta_r when we only had beta_r.

#last one without intercept
#          Mean             SD       Naive SE Time-series SE 
#     2.2390755      0.0142829      0.0003688      0.0003300



plot(zc) 
#first time plots
#they look like they are mirrors of each other

#second time plots
#they don't look mirrored anymore

#third time plots
#the plots for beta_r and beta_l are highly mirror-like

#fourth time plots
#combo looks like it converged, most likely because the two chains that make it up are mostly mirrors of each other so when you sum them up you will get valuse very small every time. Also, they will be centered close to 0 because the left over to explain is almost 0


coda.matrix <- as.matrix(zc[[1]]) #collect the chains as a matrix to save or plot
#bivariate plot
plot(coda.matrix[,1], coda.matrix[,2])
#this suggests they are highly correlated, which makes sense considering the (the first time)

plot(coda.matrix[,2], coda.matrix[,3]) #plot the two betas
plot(coda.matrix[,1], coda.matrix[,2])
plot(coda.matrix[,1], coda.matrix[,3])
#the two betas are highly correlated, but the alpha does not look to be correlated with any of the betas


head(coda.matrix)



#MCMC convergence diagnostics (from coda.samples())
raftery.diag(zc)
heidel.diag(zc)
gelman.diag(zc) #needs at least 2 chains
