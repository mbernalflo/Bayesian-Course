#rm(list=ls())

library(boot)
library(rjags)
library(coda)
library(scales)

load.module("glm")

#file.choose() #use this to find your file and create your working directory pathway

setwd("C:/Users/mabe2494/Documents/PhD/Bayesian Course/exercise1/") #set your working directory to your working folder
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

model <- "JAGS_nestsuccmodel.R"

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
                  variable.names=c("alpha", "b1", "b2"), 
                  n.iter=samples, 
                  thin=n.thin)

summary(zc)
plot(zc) 

HPDinterval(zc, prob=0.95) #give the 95% CIs
HPDinterval(zc, prob=0.99) #give the 99% CIs

coda.matrix<-as.matrix(zc[[1]]) #collect the chains as a matrix to save or plot
head(coda.matrix)



#sample the MCMC chains using JAGS.samples

zj <- jags.samples(jm, 
                  variable.names=c("b1", "b2"), 
                  n.iter=samples, 
                  thin=n.thin)

ecdf(zj$b2)(0) #see what proportion of the posterior is below 0 

pred_b1<-summary(zj$b1, quantile, c(.025,.5,.975))$stat #get the 95% CIs and median from a jags.samples object
pred_b2<-summary(zj$b2, quantile, c(.025,.5,.975))$stat #get the 95% CIs and median from a jags.samples object



#MCMC convergence diagnostics (from coda.samples())
raftery.diag(zc)
heidel.diag(zc)
gelman.diag(zc) #needs at least 2 chains
