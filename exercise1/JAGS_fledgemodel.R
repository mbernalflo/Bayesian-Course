
model{
  
  #likelihood
  
  for(i in 1:y.obs){
    
    y[i] ~ dbin(p[i], n[i])
    logit(p[i]) <- alpha + b1*fem_age[i] + b2*breed_date[i] #add additional terms as needed
    n[i] <- clutch_size[i]
    
  }#close loop i
  
  #priors	
  
  b1 ~ dnorm(0,0.00001) #dnorm has 2 arguments (mean, precision)
  b2 ~ dnorm(0,0.00001)
  alpha ~ dnorm(0,0.00001)
  
  
  
} #close model

## MUST have at least 1 blank line after model closes
