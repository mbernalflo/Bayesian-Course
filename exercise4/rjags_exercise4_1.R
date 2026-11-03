rm(list=ls())

library(boot)
library(rjags)
library(coda)
library(scales)
library(corrgram)

load.module("glm")

#file.choose() #use this to find your file and create your working directory pathway

setwd("C:/Users/mabe2494/Documents/PhD/Bayesian Course/exercise4/") #set your working directory to your working folder
milk<-read.csv("milk.csv", header=T) #bring in the data

names(milk)
#"clade"          
#"species"        
#"kcal.per.g" kilocalories of energy per gram of milk as the response variable  
#"perc.fat"   the percent of fat in the milk (predictor)
#"perc.protein"  the percentage of protein in the milk (predictor)
#"perc.lactose"   the percentage of lactose in the milk (predictor)
#"mass"           the average female mass in kg (predictor), we eill use log(mass)
#"neocortex.perc" the percent of total brain mass that is cortex (predictor)

milk <- na.omit(milk)

str(milk)
summary(milk) 

plot(milk)
corrgram(milk, lower.panel = panel.pie, upper.panel = panel.conf)


data <- NULL
data <- list(
  y=milk$kcal.per.g, #the response variable
  y.obs=nrow(milk), #the number of observations
  mass=log(milk$mass), #explanatory variable
  #breed_date=as.vector(scale(breed$breeding_date)), #explanatory variable
  neocortex=milk$neocortex.perc
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
    alpha=1,
    beta_nc=0.1,
    beta_mass=0.1,
    sigma=0.1
  ),
  list(
    alpha=10,
    beta_nc=0.005,
    beta_mass=0.005,
    sigma=0.1
  ),
  list(
    alpha=-1,
    beta_nc=-0.01,
    beta_mass=-0.01,
    sigma=0.1
  )
)


model <- "JAGS_milkmodel1.R"

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
                   variable.names=c("alpha",
                                    "beta_nc",
                                    "beta_mass"
                                    ), 
                   n.iter=samples, 
                   thin=n.thin)

summary(zc)

#first time only with neocortex
#           2.5%       25%      50%      75%   97.5%
#alpha   -0.8363 -0.002899 0.349617 0.678566 1.51880
#beta_nc -0.0127 -0.000225 0.004756 0.009692 0.02152

#second time only with mass
#              2.5%      25%      50%      75%  97.5%
#alpha      0.59514  0.66798  0.70732  0.74341 0.8119
#beta_mass -0.07871 -0.04765 -0.03214 -0.01612 0.0166

#third time with both mass and neocortex
#               2.5%      25%      50%      75%    97.5%
#alpha     -2.166168 -1.45683 -1.08351 -0.70243  0.10505
#beta_mass -0.151767 -0.11511 -0.09693 -0.07849 -0.04277
#beta_nc    0.009473  0.02211  0.02770  0.03372  0.04480


plot(zc) 
#only neocortex plots
#chains don't look too good and it fails the heidel test. Also they look quite wide and centered in zero, so it seems non informative

#only mass plots
#looks fine, but also centered in zero quite a lot. Passes the tests

#plots with both mass and neocortex
#the plots look better now, and the variables seem to explain more our data, for example they are not overlapping zero anymore




coda.matrix <- as.matrix(zc[[1]]) #collect the chains as a matrix to save or plot
#bivariate plot
plot(coda.matrix[,2], coda.matrix[,3])
#this suggests they are correlated, which reflects the 0.61 correlation coefficient we saw in the plot at the beginning


head(coda.matrix)


#MCMC convergence diagnostics (from coda.samples())
raftery.diag(zc)
heidel.diag(zc)
gelman.diag(zc) #needs at least 2 chains

