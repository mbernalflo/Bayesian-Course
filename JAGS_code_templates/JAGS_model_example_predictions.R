#for the Bayes course 2024 - code by Matt Low
#JAGS model code for simple Gaussian linear model
#with vague priors
#from data where y is observed response variable
#and age and date are the explanatory variables 
#assume homogeneity of variances (i.e. variance/tau is the same ragardless of the mean estimate)

#asking for prediction for dates from 1 to 10 (called 'x.pred' in the data statement)

model{

#likelihood

	for(i in 1:y.obs){
	
		y[i] ~ dnorm(mu[i], tau)
		mu[i] <- alpha + b1*age[i] + b2*date[i] #add additional terms as needed
		
		y.sim[i] ~ likelihood(parameter1, parameter2)  #here use the SAME likelihood and parameters as above but it is called y.sim[i] instead of y[i]
		
	}#close likelihood loop i
	
#priors	
	
	b1 ~ dnorm(0,0.00001) #dnorm has 2 arguments (mean, precision)
	b2 ~ dnorm(0,0.00001)
	alpha ~ dnorm(0,0.00001)
	sigma ~ dunif(0,50) #dunif has 2 argument (lower, upper) 
	tau <- 1/sigma^2

#predictions
#here we use a LOOP to generate multiple predictions
#ONLY use a loop for multiple prediction generation
#if you only need to estimate 1 or a few derived variables...don't use a loop, just ask for what you want directly 

for(j in 1:length(x.pred)){
	
output[j]<-alpha + b1*5 + b2*x.pred[j] #expected value of y when age=5 and date=ranges from 1 to 10

} #close prediction loop j

	#model fit calculations
	#means
	mean.y <-mean(y[]) #y is the name of your response variable
	mean.y.sim <-mean(y.sim[]) #y.sim is name of your sim variable
	p.mean <-step(mean.y.sim - mean.y) 
	
	#coeff variation
	cv.y <- sd(y[ ])/mean(y[ ]) #y is the name of your response variable
	cv.y.sim <- sd(y.sim[])/mean(y.sim[ ]) #y.sim is name of your sim variable
	p.cv <- step(cv.y.sim-cv.y)
	
	#sum of squares
	for(j in 1:nobs){
	  sq[j] <- (y[j]-        )^2 #insert expected outcome from likelihood
	  sq.sim[j] <- (y.sim[j]-         )^2 #insert expected outcome from likelihood
	}
	fit <- sum(sq[])
	fit.sim <- sum(sq.sim[])
	p.fit <- step(fit.sim-fit)

} #close model

## MUST have at least 1 blank line after model closes

