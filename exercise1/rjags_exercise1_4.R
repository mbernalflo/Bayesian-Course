rm(list=ls())

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
  y=breed$breeding_date, #the response variable
  y.obs=nrow(breed), #the number of observations
  year=breed$year #explanatory variable
) 
data

# inits <- list(
#   list(
#     alpha=1,
#     b1=0.1,
#     sigma=1
#   ))

inits <- list(
  list(
    alpha=1,
    b1=-0.1,
    sigma=1
  ),
  list(
    alpha=20,
    b1=-0.7,
    sigma=5
  ),
  list(
    alpha=10,
    b1=-1,
    sigma=10
  )
)


model <- "JAGS_breeddatemodel.R"

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
                   variable.names=c("alpha", "b1", "sigma"), 
                   n.iter=samples, 
                   thin=n.thin)

summary(zc)
plot(zc) 

HPDinterval(zc, prob=0.95) #give the 95% CIs
HPDinterval(zc, prob=0.99) #give the 99% CIs
#         lower      upper
# alpha 19.5662388 22.7803957
# b1    -0.8871015 -0.5165767
# sigma  5.0475979  6.1660635
# attr(,"Probability")
# [1] 0.99

coda.matrix <- as.matrix(zc[[1]]) #collect the chains as a matrix to save or plot
head(coda.matrix)



#sample the MCMC chains using JAGS.samples

zj <- jags.samples(jm, 
                   variable.names=c("b1"), 
                   n.iter=samples, 
                   thin=n.thin)

ecdf(zj$b1)(0) #see what proportion of the posterior is below 0 

pred <- summary(zj$b1, quantile, c(.025,.5,.975))$stat #get the 95% CIs and median from a jags.samples object
#1


#MCMC convergence diagnostics (from coda.samples())
raftery.diag(zc)
heidel.diag(zc)
gelman.diag(zc) #needs at least 2 chains

