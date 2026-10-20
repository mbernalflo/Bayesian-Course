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
  y=breed$clutch_size, #the response variable
  y.obs=nrow(breed), #the number of observations
  fem_age=breed$fem_age, #explanatory variable
  breed_date=breed$breeding_date
) 
data

inits <- list(
  list(
    alpha=1,
    b1=-0.03,
    b2=0.1,
    sigma=1
  ),
  list(
    alpha=10,
    b1=0.005,
    b2=0.5,
    sigma=5
  ),
  list(
    alpha=20,
    b1=-0.02,
    b2=0.001,
    sigma=0.1
  )
)


model <- "JAGS_clutchsizemodel.R"

jm <- jags.model(model,
                 data=data, 
                 n.adapt=5000, 
                 inits=inits, 
                 n.chains=3) 


burn.in <- 10000

update(jm, n.iter=burn.in) 


samples=15000
n.thin=5

#sample the MCMC chains using CODA

zc = coda.samples(jm,
                  variable.names=c("alpha", "b1", "b2", "sigma"), 
                  n.iter=samples, 
                  thin=n.thin)

summary(zc)
plot(zc) 

HPDinterval(zc, prob=0.95) #give the 95% CIs
HPDinterval(zc, prob=0.99) #give the 99% CIs

# [[1]]
# lower       upper
# alpha  6.65867772  7.30537085
# b1    -0.24605908  0.18012112
# b2    -0.06650024 -0.03525849
# sigma  0.61529323  0.75054809
# attr(,"Probability")
# [1] 0.99
# 
# [[2]]
# lower       upper
# alpha  6.61748884  7.28929228
# b1    -0.23950176  0.17919796
# b2    -0.06534112 -0.03288761
# sigma  0.61262426  0.74518142
# attr(,"Probability")
# [1] 0.99
# 
# [[3]]
# lower       upper
# alpha  6.57273237  7.28630592
# b1    -0.24607417  0.20670436
# b2    -0.06506855 -0.03267997
# sigma  0.62145255  0.75789615
# attr(,"Probability")
# [1] 0.99

coda.matrix<-as.matrix(zc[[1]]) #collect the chains as a matrix to save or plot
head(coda.matrix)



#sample the MCMC chains using JAGS.samples

zj = jags.samples(jm, 
                  variable.names=c("b1", "b2"), 
                  n.iter=samples, 
                  thin=n.thin)

ecdf(zj$b1)(0) #see what proportion of the posterior is below 0 
#0.6448889
ecdf(zj$b2)(0) #see what proportion of the posterior is below 0 
#1
pred <- summary(zj$b1, quantile, c(.025,.5,.975))$stat #get the 95% CIs and median from a jags.samples object


#MCMC convergence diagnostics (from coda.samples())
raftery.diag(zc)
heidel.diag(zc)
gelman.diag(zc) #needs at least 2 chains
