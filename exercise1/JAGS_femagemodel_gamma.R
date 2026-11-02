model{
  
  #likelihood
  
  for(i in 1:y.obs){
    
    y[i] ~ dgamma(a[i], b[i]) #likelihood model
    mu[i] <- alpha + b1*fem_age[i] #prediction model
    a[i] <- (mu[i])^2/(sigma)^2
    b[i] <- mu[i]/(sigma)^2
    
  }#close loop i
  
  #priors	
  
  b1 ~ dnorm(0,0.00001) #dnorm has 2 arguments (mean, precision)
  alpha ~ dnorm(0,0.00001)
  sigma ~ dgamma(.001,.001) #dunif has 2 argument (lower, upper) 
  
  
  
} #close model

## MUST have at least 1 blank line after model closes
