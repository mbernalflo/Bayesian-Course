model{
  
  #likelihood
  
  for(i in 1:y.obs){
    
    y[i] ~ dbern(p[i])
    logit(p[i]) <- alpha + b1*fem_age[i] + b2*breed_date[i] #add additional terms as needed
    
    
  }#close loop i
  
  #priors	
  
  b1 ~ dnorm(0,1E-6) #dnorm has 2 arguments (mean, precision)
  b2 ~ dnorm(0,1E-6)
  alpha ~ dnorm(0,1E-6)
  
  #predictions
  logit(nestsucc_young) <- alpha + b1*0 + b2*mean(breed_date) #you can use logit to do the inverse of the logit function
  logit(nestsucc_exp) <- alpha + b1*1 + b2*mean(breed_date)
  diff_nestsucc <- nestsucc_exp - nestsucc_young  #if positive more experienced will have higher probability of nest success than non experienced. If negative the opposite
  
  
} #close model

