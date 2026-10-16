#R JAGS template for Bayes course - Matt Low & Malin Aronsson April 2022

#this is the R side of the analysis where you run everything
#the JAGS model code is saved into a file, you run it from in here

#rm(list=ls()) #clear the workspace (if wanted)

#1. load the libraries
library(boot)
library(rjags)
library(coda)
library(scales)

load.module("glm")


#2. get the data

#file.choose()
setwd("") #need to set working directory
df<-read.csv("data.csv", header=T) #obviously use your data name here

names(df) #and if you called your data something other than 'df', then use that name here
str(df)
summary(df) 


#3. create the data statement for JAGS

data=NULL
data=list(

) 
data


#4. create the intital values for the MCMC chain

inits=list(
list(

))

#5. specify which model to run
model = "name_of_JAGS_model_file.R" #obviously use your model file name here

#6.compile the model in JAGS
jm = jags.model(model,
data=data, 
n.adapt=5000, 
inits=inits, 
n.chains=1) 

#7. run the MCMC chains to create stability
burn.in=10000

update(jm, n.iter=burn.in) 

#8. sample the MCMC chains to create the posterior distributions
samples=10000
n.thin=5

#CODA object MCMC sampling
zc = coda.samples(jm,
variable.names=c(""), #include your parameter names here
n.iter=samples, 
thin=n.thin)

summary(zc)
plot(zc) 

#JAGS object MCMC sampling
zj = jags.samples(jm, 
variable.names=c(""), #include your parameter names here
n.iter=samples, 
thin=n.thin)

#summarise a JAGS object to create a 95% CI for a single or multiple points
pred<-summary(zj$output, quantile, c(.025,.5,.975))$stat #where 'output' is the name of the parameter from your model (you need to change it to what YOU called it)




#####--------------------------------------------########

#9. other useful code you might use (will need to be adjusted for your models)
##we will get to these in turn during the lectures and exercises
##please don't panic if you don't know what they are used for yet...leave them until we get to them


#diagnostics (see the JAGS Primer)
raftery.diag(zc)
heidel.diag(zc)
gelman.diag(zc) #needs at least 2 chains


#useful functions for doing predictions
ecdf(zj$output)(0) #obviously substitute the name of your parameter for 'output'
HPDinterval(zc, prob=0.95)
pred<-summary(zj$output, quantile, c(.025,.5,.975))$stat

#extract mcmc chain data from coda object
coda.matrix<-as.matrix(zc[[1]])
head(coda.matrix)

#plotting prediction & 95%CIs using polygon
pred<-summary(zj$output, quantile, c(.025,.5,.975))$stat
x=data$x.pred #if you created an 'x.pred' object in your data statement
y=pred
plot(x,y[2,], col="blue", xlab="Number of trials", ylab="Probability of correct choice", cex=1.4, typ="l", tck=0.03, bty="l") 
polygon(c(x,rev(x)), c(y[1,], rev(y[3,])), col=alpha("blue", alpha=0.4), border=alpha("blue", alpha=0.4))
lines(x,y[1,], lty="dashed", col="blue")
lines(x,y[3,], lty="dashed", col="blue")

abline(h=0.5, lty=2, col="grey80")



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




## Model Selection (we will get to this in week 2 in the model selection lecture)

##ON THE JAGS MODEL SIDE...NOT IN RJAGS

#if this is your likelihood     
y[i] ~ dpois(lambda[i])

#now add this
#this gives predictive probability density for WAIC
pd[i] <- dpois(y[i],lambda[i])

#this calculates the log PPD
log_pd[i] <- log(dpois(y[i],lambda[i]))

##THEN ADD THE FOLLOWING CODE IN THE RJAGS CODE AFTER YOU'VE RUN THE MODEL

zj = jags.samples(jm, 
variable.names=c("pd" ,"log_pd"), 
n.iter=samples, 
thin=n.thin)

lppd=-2*sum(log(summary(zj$pd,mean)$stat)) #fit
pd.WAIC=sum((summary(zj$log_pd,sd)$stat)^2) #penalty
WAIC=lppd+2*pd.WAIC                 #fit + 2*penalty
WAIC
