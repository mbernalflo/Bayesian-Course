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


} #close model

## MUST have at least 1 blank line after model closes

