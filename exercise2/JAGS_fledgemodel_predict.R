model{
  
  #likelihood
  
  for(i in 1:y.obs){
    
    y[i] ~ dbin(p[i], n[i])
    logit(p[i]) <- alpha + b_age*fem_age[i] + b_date*breed_date[i] #add additional terms as needed
    n[i] <- clutch_size[i]
    
  }#close loop i
  
  #priors	
  
  b_age ~ dnorm(0,0.00001) #dnorm has 2 arguments (mean, precision)
  b_date ~ dnorm(0,0.00001)
  alpha ~ dnorm(0,0.00001)
  
  
  #predictions
  for(k in 1:length(x.pred)){
    logit(pred_out[k]) <- alpha + b_date*x.pred[k] + b_age*mean(fem_age)
  }
  
} #close model

## MUST have at least 1 blank line after model closes
