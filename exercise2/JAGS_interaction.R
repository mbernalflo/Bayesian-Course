model{
  
  #likelihood
  
  for(i in 1:y.obs){
    
    y[i] ~ dbin(p[i], n[i])
    logit(p[i]) <- alpha + b_age*fem_age[i] + b_date*breed_date[i] + b_int*fem_age[i]*breed_date[i] #add additional terms as needed
    n[i] <- clutch_size[i]
    
  }#close loop i
  
  #priors	
  
  b_age ~ dnorm(0,0.00001) #dnorm has 2 arguments (mean, precision)
  b_date ~ dnorm(0,0.00001)
  b_int ~ dnorm(0,0.00001)
  alpha ~ dnorm(0,0.00001)
  
  #predictions
  for(k in 1:length(x.pred)){
    logit(pred_out_young[k]) <- alpha + b_date*x.pred[k] + b_age*0 + b_int*b_date*0
    logit(pred_out_exp[k]) <- alpha + b_date*x.pred[k] + b_age*1 + b_int*b_date*1
    #diff[k] <- pred_out_exp[k] - pred_out_young[k]
  }
  diff1 <- pred_out_exp[1] - pred_out_young[1] 
  diff30 <- pred_out_exp[491] - pred_out_young[491]
  
  
} #close model

## MUST have at least 1 blank line after model closes
