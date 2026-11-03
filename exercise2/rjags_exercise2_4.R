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
  y=breed$chicks_fledged, #the response variable
  y.obs=nrow(breed), #the number of observations
  fem_age=breed$fem_age, #explanatory variable
  breed_date=breed$breeding_date, #explanatory variable
  clutch_size=breed$clutch_size,
  x.pred=seq(from=min(breed$breeding_date), to=max(breed$breeding_date), 1)
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
    b_age=0.1,
    b_date=0.1
  ),
  list(
    alpha=10,
    b_age=0.005,
    b_date=-0.5
  ),
  list(
    alpha=-1,
    b_age=-0.01,
    b_date=-0.001
  )
)


model <- "JAGS_fledgemodel_predict.R"

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
                   variable.names=c("alpha", "b_age", "b_date"), 
                   n.iter=samples, 
                   thin=n.thin)

summary(zc)
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
                   variable.names=c("b_age", "b_date", "pred_out"), 
                   n.iter=samples, 
                   thin=n.thin)

ecdf(zj$b_age)(0) #see what proportion of the posterior is below 0 
ecdf(zj$b_date)(0) #see what proportion of the posterior is below 0 


pred_age <- summary(zj$b_age, quantile, c(.025,.5,.975))$stat #get the 95% CIs and median from a jags.samples object
pred_age <- summary(zj$b_date, quantile, c(.025,.5,.975))$stat #get the 95% CIs and median from a jags.samples object

#plotting prediction & 95%CIs using polygon
pred <- summary(zj$pred_out, quantile, c(.025,.5,.975))$stat
x <- data$x.pred
y <- pred


#plot the second row (median expectation)
plot(NA,NA, xlim=c(-1.5,30.5) ,ylim=c(0.6,0.9), typ="n")
#plot(x,y[2,], col="blue", xlab="Date", ylab="Number of chicks fledged", cex=1.4, typ="l", tck=0.03, bty="l", ylim=c(0,10)) #plot the median

lines(x,y[2,], col="black")

#plot the 1st and 3rd rows (95% CIs) as a polygon or lines

lines(x,y[1,], lty="dashed", col="blue") # add lower CI (if wanted)
lines(x,y[3,], lty="dashed", col="blue") #add upper CI (if wanted)

#if you want to make the colour of the polygon 'see through' use the scales package
#and the alpha() function within any col argumnent in R plotting (adjust alpha to make it more or less transparent)

polygon(c(x,rev(x)), c(y[1,], rev(y[3,])), col=alpha("blue", alpha=0.1), border=alpha("blue", alpha=0.1)) 




#MCMC convergence diagnostics (from coda.samples())
raftery.diag(zc)
heidel.diag(zc)
gelman.diag(zc) #needs at least 2 chains

