model{
  
  #likelihood
  for(i in 1:y.obs){
    
    y[i] ~ dnorm(mu[i], tau)
    #mu[i] <- alpha + beta_lact*lactose[i]
    #mu[i] <- alpha + beta_fat*fat[i]
    mu[i] <- alpha + beta_lact*lactose[i] + beta_fat*fat[i]
    
    
  }
  
  #priors
  alpha ~ dnorm(0,0.00001)
  beta_lact ~ dnorm(0,0.00001)
  beta_fat ~ dnorm(0,0.00001)
  sigma ~ dunif(0,50) #dunif has 2 argument (lower, upper) 
  tau <- 1/sigma^2
  
  #predictions 
  
  # for(k in 1:length(x.pred)){
  # 
  #   
  #   
  # 	 } #only needed if doing multiple predictions along an x-gradient
  
  
  
}
