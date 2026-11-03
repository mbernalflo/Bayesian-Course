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
  #breed_date=breed$breeding_date, #explanatory variable
  breed_date=as.vector(scale(breed$breeding_date)), #explanatory variable
  clutch_size=breed$clutch_size,
  x.pred=seq(from=min(as.vector(scale(breed$breeding_date))), to=max(as.vector(scale(breed$breeding_date))), by=0.01)
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
    b_age=0.1,
    b_date=0.1,
    b_int=0.1
  ),
  list(
    alpha=10,
    b_age=0.005,
    b_date=-0.5,
    b_int=0.1
  ),
  list(
    alpha=-1,
    b_age=-0.01,
    b_date=-0.001,
    b_int=0.1
  )
)


model <- "JAGS_interaction.R"

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
n.thin <- 20

#sample the MCMC chains using CODA

zc <- coda.samples(jm,
                    variable.names=c("alpha", "b_age", "b_date", "b_int", "pred_out_young", "pred_out_exp"), 
                    n.iter=samples, 
                    thin=n.thin)

summary(zc)
plot(zc) 



coda.matrix <- as.matrix(zc[[1]]) #collect the chains as a matrix to save or plot
head(coda.matrix)

diff_early <- coda.matrix[,5] - coda.matrix[,496]
mean(diff_early) #0.05076915
diff_late <- coda.matrix[,495] - coda.matrix[,986]
mean(diff_late) #0.06403525



#sample the MCMC chains using JAGS.samples

zj <- jags.samples(jm, 
                    variable.names=c("b_age", "b_date", "b_int", "pred_out_young", "pred_out_exp", "diff1", "diff30"), 
                    n.iter=samples, 
                    thin=n.thin)

#ecdf(zj5$b1)(0) #see what proportion of the posterior is below 0 
#ecdf(zj$diff[1, ,])(0) #see what proportion of the posterior is below 0 

head(zj$diff)
pred_young <- summary(zj$pred_out_young, quantile, c(.025,.5,.975))$stat #get the 95% CIs and median from a jags.samples object
pred_exp <- summary(zj$pred_out_exp, quantile, c(.025,.5,.975))$stat #get the 95% CIs and median from a jags.samples object

pred_diff1 <- summary(zj$diff1, quantile, c(.025,.5,.975))$stat #get the 95% CIs and median from a jags.samples object
pred_diff30 <- summary(zj$diff30, quantile, c(.025,.5,.975))$stat #get the 95% CIs and median from a jags.samples object

dim(zj$diff)
x_young <- data$x.pred
y_young <- pred_young

x_exp <- data$x.pred
y_exp <- pred_exp

#plot the second row (median expectation)
plot(NA,NA, xlim=c(-2.7,1.5) ,ylim=c(0.6,0.9), typ="n", xlab="Breeding date",ylab="p(Egg fledging)")
#plot(x,y[2,], col="blue", xlab="Date", ylab="Number of chicks fledged", cex=1.4, typ="l", tck=0.03, bty="l", ylim=c(0,10)) #plot the median
#median for inexperienced birds
lines(x_young,y_young[2,], col="black")

#plot the 1st and 3rd rows (95% CIs) as a polygon or lines
#95% CI for inexperienced birds
#lines(x_young,y_young[1,], lty="dashed", col="blue") # add lower CI (if wanted)
#lines(x_young,y_young[3,], lty="dashed", col="blue") #add upper CI (if wanted)

#if you want to make the colour of the polygon 'see through' use the scales package
#and the alpha() function within any col argumnent in R plotting (adjust alpha to make it more or less transparent)

polygon(c(x_young,rev(x_young)), c(y_young[1,], rev(y_young[3,])), col=alpha("blue", alpha=0.1), border=alpha("blue", alpha=0.1)) 

#line with 95% CI for experienced birds
lines(x_exp,y_exp[2,], col="black")

#lines(x_young,y_young[1,], lty="dashed", col="blue") # add lower CI (if wanted)
#lines(x_young,y_young[3,], lty="dashed", col="blue") #add upper CI (if wanted)

polygon(c(x_exp,rev(x_exp)), c(y_exp[1,], rev(y_exp[3,])), col=alpha("red", alpha=0.1), border=alpha("red", alpha=0.1))

diff_early <- coda.matrix
diff_late


#MCMC convergence diagnostics (from coda.samples())
raftery.diag(zc)
heidel.diag(zc)
gelman.diag(zc) #needs at least 2 chains