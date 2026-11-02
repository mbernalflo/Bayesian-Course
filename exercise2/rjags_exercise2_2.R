rm(list=ls())

library(boot)
library(rjags)
library(coda)
library(scales)

load.module("glm")

#file.choose() #use this to find your file and create your working directory pathway

setwd("C:/Users/mabe2494/Documents/PhD/Bayesian Course/exercise2/") #set your working directory to your working folder
breed<-read.csv("bird_data.csv", header=T) #bring in the data

names(breed)
#[1] "year"           "nest_success"   "breeding_date"  "fem_age"        "clutch_size"    "chicks_fledged"
str(breed)
summary(breed) 

data <- NULL
data <- list(
  y=breed$nest_success, #the response variable
  y.obs=nrow(breed), #the number of observations
  fem_age=breed$fem_age, #explanatory variable
  breed_date=breed$breeding_date #explanatory variable
) 
data

inits <- list(
  list(
    alpha=1,
    b1=0.1,
    b2=0.1
  ),
  list(
    alpha=5,
    b1=0.5,
    b2=0.5
  ),
  list(
    alpha=2,
    b1=0.0005,
    b2=0.0001
  )
)

# inits <- list(
#      list(
#        alpha=5,
#        b1=0.5,
#        b2=0.5
#     ))

model <- "JAGS_nestsuccmodel_predict.R"

jm <- jags.model(model,
                 data=data, 
                 n.adapt=5000, 
                 inits=inits, 
                 n.chains=3) 


burn.in <- 10000

update(jm, n.iter=burn.in) 


samples <- 10000
n.thin <- 5

#sample the MCMC chains using CODA

zc <- coda.samples(jm,
                   variable.names=c("alpha", "b1", "b2","nestsucc_young", "nestsucc_exp", "diff_nestsucc"), 
                   n.iter=samples, 
                   thin=n.thin)

summary(zc)
plot(zc) 

HPDinterval(zc, prob=0.95) #give the 95% CIs
HPDinterval(zc, prob=0.99) #give the 99% CIs

zc_matrix<-as.matrix(zc[[1]]) #collect the chains as a matrix to save or plot
head(zc_matrix)

plot(density(zc_matrix[,4]))

young <- density(zc_matrix[,6])
exp <- density(zc_matrix[,5])



plot(NA, NA, xlim = c(0.6, 1), ylim = c(0,20), type = "n")
lines(young$x, young$y)
polygon(young$x, young$y, col = alpha("red", alpha = 0.5), border = "red")
lines(exp$x, exp$y, col = "blue")
polygon(exp$x, exp$y, col = alpha("blue", alpha = 0.5), border = "blue")


n <- 100
xx <- c(0:n, n:0)
yy <- c(c(0, cumsum(stats::rnorm(n))), rev(c(0, cumsum(stats::rnorm(n)))))
plot   (-0.2:0.5, type = "n", xlab = "Time", ylab = "Distance")
polygon(xx, yy, col = "gray", border = "red")
title("Distance Between Brownian Motions")

#sample the MCMC chains using JAGS.samples

zj <- jags.samples(jm, 
                   variable.names=c("b1", "b2", "nestsucc_young", "nestsucc_exp", "diff_nestsucc"), 
                   n.iter=samples, 
                   thin=n.thin)

ecdf(zj$b2)(0) #see what proportion of the posterior is below 0 

pred_b1<-summary(zj$b1, quantile, c(.025,.5,.975))$stat #get the 95% CIs and median from a jags.samples object
pred_b2<-summary(zj$b2, quantile, c(.025,.5,.975))$stat #get the 95% CIs and median from a jags.samples object

pred_b2<-summary(zj$nestsucc_young, quantile, c(.025,.5,.975))$stat #get the 95% CIs and median from a jags.samples object

pred_b2<-summary(zj$nestsucc_exp, quantile, c(.025,.5,.975))$stat #get the 95% CIs and median from a jags.samples object

pred_b2<-summary(zj$diff_nestsucc, quantile, c(.025,.5,.975))$stat #get the 95% CIs and median from a jags.samples object

1 - ecdf(zj$diff_nestsucc)(0) #0.93, which means that there is a 93% probability that more experienced breeders will have higher nest success than young breeders


#MCMC convergence diagnostics (from coda.samples())
raftery.diag(zc)
heidel.diag(zc)
gelman.diag(zc) #needs at least 2 chains
