rm(list=ls())

library(boot)
library(rjags)
library(coda)
library(scales)

load.module("glm")

#file.choose() #use this to find your file and create your working directory pathway

setwd("C:/Users/mabe2494/Documents/PhD/Bayesian Course/exercise5/") #set your working directory to your working folder
lichen<-read.csv("lichen_edge.csv", header=T) #bring in the data

names(lichen)
#"transect.distance"       "tree.age"   "count"  
str(lichen)
summary(lichen) 

plot(lichen$transect.distance, lichen$count)

data <- NULL
data <- list(
  n_lichens=lichen$count, #the response variable
  y.obs=nrow(lichen), #the number of observations
  trans_dist=lichen$transect.distance, #explanatory variable
  x.pred=seq(from=min(lichen$transect.distance), to=max(lichen$transect.distance), .1)
) 
data

# inits <- list(
#   list(
#     Vmax=3,
#     h=80,
#     n=-50,
#     d=0.1
#   ))

inits <- list(
  list(
    Vmax=30,
    h=80,
    n=-50,
    d=0.1
  ),
  list(
    Vmax=1,
    h=5,
    n=2,
    d=0.99
  ),
  list(
    Vmax=200,
    h=0.1,
    n=10,
    d=0.001
  )
)


model <- "JAGS_lichenmodel.R"

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
                   variable.names=c("Vmax", "h", "n", "d"), 
                   n.iter=samples, 
                   thin=n.thin)

summary(zc)
#          2.5%      25%      50%      75%    97.5%
# Vmax    2.3685   2.6323   2.7798   2.9280   3.2393
# d       0.1706   0.2487   0.2936   0.3445   0.4532
# h      77.4471  80.2479  81.9852  83.5975  85.0410
# n    -154.0656 -94.5300 -59.6528 -37.5880 -18.5726
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
                   variable.names=c("Vmax", "h","n", "output_lichen"), 
                   n.iter=samples, 
                   thin=n.thin)



pred_lichen <- summary(zj$output_lichen, quantile, c(.025,.5,.975))$stat #get the 95% CIs and median from a jags.samples object

#plotting prediction & 95%CIs using polygon
pred <- summary(zj$output_lichen, quantile, c(.025,.5,.975))$stat
x <- data$x.pred
y <- pred


#plot the second row (median expectation)
plot(NA,NA, xlim=c(-0.5,180) ,ylim=c(-0.5,5.5), typ="n")
#plot(x,y[2,], col="blue", xlab="Date", ylab="Number of chicks fledged", cex=1.4, typ="l", tck=0.03, bty="l", ylim=c(0,10)) #plot the median

points(jitter(lichen$transect.distance, factor = 1), jitter(lichen$count, factor = 1))

lines(x,y[2,], col="blue")

#plot the 1st and 3rd rows (95% CIs) as a polygon or lines

#lines(x,y[1,], lty="dashed", col="blue") # add lower CI (if wanted)
#lines(x,y[3,], lty="dashed", col="blue") #add upper CI (if wanted)

#if you want to make the colour of the polygon 'see through' use the scales package
#and the alpha() function within any col argumnent in R plotting (adjust alpha to make it more or less transparent)

polygon(c(x,rev(x)), c(y[1,], rev(y[3,])), col=alpha("blue", alpha=0.1), border=alpha("blue", alpha=0.1)) 




#MCMC convergence diagnostics (from coda.samples())
raftery.diag(zc)
heidel.diag(zc)
gelman.diag(zc) #needs at least 2 chains

