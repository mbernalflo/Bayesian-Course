model{
  
  #likelihood
  
  for(i in 1:y.obs){
    
    y[i] ~ dbern(p[i])
    logit(mu[i]) <- alpha + b1*fem_age[i] + b2*breed_date[i] #add additional terms as needed
    p[i] <- mu[i]
    
  }#close loop i
  
  #priors	
  
  b1 ~ dnorm(0,1E-6) #dnorm has 2 arguments (mean, precision)
  b2 ~ dnorm(0,1E-6)
  alpha ~ dnorm(0,1E-6)

  
  
  
} #close model

## MUST have at least 1 blank line after model closes
