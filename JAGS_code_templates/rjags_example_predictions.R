#R JAGS template for Bayes course - Matt Low & Malin Aronsson Jan 2018

#rm(list=ls())

library(boot)
library(rjags)
library(coda)
library(scales)

load.module("glm")

#file.choose()
setwd("/Users/matlow/Dropbox/Matt Work/Teaching courses/Bayes Course/Bayes 2023/exercises & data 2020/JAGS templates/") #set your working directory to your working folder
breed<-read.csv("example_data.csv", header=T) #bring in the data

names(breed)
#[1] "year"           "nest_success"   "breeding_date"  "fem_age"        "clutch_size"    "chicks_fledged"
str(breed)
summary(breed) 

data=NULL
data=list(
	y=breed$chicks_fledged, #the response variable
	y.obs=length(breed$chicks_fledged), #the number of observations
	age=breed$fem_age, #explanatory variable
	date=breed$breeding_date, #explanatory variable
	x.pred=1:10 #10 date values you want to predict from the model
) 
data

inits=list(
list(
alpha=1,
b1=0.1,
b2=0.1,
sigma=1
))


model = "JAGS model example predictions.R"

jm = jags.model(model,
data=data, 
n.adapt=5000, 
inits=inits, 
n.chains=1) 


burn.in=10000

update(jm, n.iter=burn.in) 


samples=10000
n.thin=5


#sampling the MCMC chains using CODA

zc = coda.samples(jm,
variable.names=c("alpha", "b1", "b2", "sigma"), 
n.iter=samples, 
thin=n.thin)

summary(zc)
plot(zc) 


#sampling 

zj = jags.samples(jm, 
variable.names=c("output"), #here we ask for the multiple predictions in the object 'output' 
n.iter=samples, 
thin=n.thin)

#the summary function allows you to extract all the numbers you need to present or plot from jags.samples objects
pred<-summary(zj$output, quantile, c(.025,.5,.975))$stat

pred #look inside the pred object

          # [,1]     [,2]     [,3]     [,4]     [,5]     [,6]     [,7]     [,8]     [,9]    [,10]
# 2.5%  4.852756 4.780311 4.692499 4.625103 4.549130 4.492152 4.418953 4.355581 4.277363 4.195472
# 50%   7.046092 6.986655 6.929948 6.876078 6.816325 6.757562 6.692709 6.624404 6.560448 6.496217
# 97.5% 9.240907 9.183434 9.137580 9.080383 9.015112 8.968522 8.915203 8.829779 8.777517 8.746369

#here you have the y-coordinates for your plot. The top row is the lower 95% CI, the middle row the median expectation, and the lower row the upper 95% CI
#your x co-ordinates are your x.pred object you created in the data statement


#plotting prediction & 95%CIs using polygon
pred<-summary(zj$output, quantile, c(.025,.5,.975))$stat
x=data$x.pred
y=pred

#plot the second row (median expectation)
plot(x,y[2,], col="blue", xlab="Date", ylab="Number of chicks fledged", cex=1.4, typ="l", tck=0.03, bty="l", ylim=c(0,10)) #plot the median

#plot the 1st and 3rd rows (95% CIs) as a polygon or lines

lines(x,y[1,], lty="dashed", col="blue") # add lower CI (if wanted)
lines(x,y[3,], lty="dashed", col="blue") #add upper CI (if wanted)

#if you want to make the colour of the polygon 'see through' use the scales package
#and the alpha() function within any col argumnent in R plotting (adjust alpha to make it more or less transparent)

library(scales)

polygon(c(x,rev(x)), c(y[1,], rev(y[3,])), col=alpha("blue", alpha=0.1), border=alpha("blue", alpha=0.1)) 

