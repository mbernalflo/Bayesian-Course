#for the Bayes course 2024 - code by Matt Low
#JAGS model code for simple Gaussian linear model
#with vague priors
#from data where y is observed response variable
#and age and date are the explanatory variables 
#assume homogeneity of variances (i.e. variance/tau is the same regardless of the mean estimate)


model{

#likelihood

	for(i in 1:y.obs){
	
		y[i] ~ dnorm(mu[i], tau)
		mu[i] <- alpha + b1*age[i] + b2*date[i] #add additional terms as needed
		
	}#close loop i
	
#priors	
	
	b1 ~ dnorm(0,0.00001) #dnorm has 2 arguments (mean, precision)
	b2 ~ dnorm(0,0.00001)
	alpha ~ dnorm(0,0.00001)
	sigma ~ dunif(0,50) #dunif has 2 argument (lower, upper) 
	tau <- 1/sigma^2



} #close model

## MUST have at least 1 blank line after model closes

