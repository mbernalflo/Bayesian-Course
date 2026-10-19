
model{
  
  #likelihood
  
  for(i in 1:y.obs){
    
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha + b1*fem_age[i] + b2*breed_date[i] #add additional terms as needed
    
  }#close loop i
  
  #priors	
  
  b1 ~ dnorm(0,0.00001) #dnorm has 2 arguments (mean, precision)
  b2 ~ dnorm(0,0.00001)
  alpha ~ dnorm(0,0.00001)
  sigma ~ dgamma(.001,.001) #dunif has 2 argument (lower, upper) 
  tau <- 1/sigma^2
  
  
  
} #close model

## MUST have at least 1 blank line after model closes