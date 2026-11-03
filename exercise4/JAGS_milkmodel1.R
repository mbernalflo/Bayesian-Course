model{
  
  #likelihood
  for(i in 1:y.obs){
    
    y[i] ~ dnorm(mu[i], tau)
    #mu[i] <- alpha + beta_nc*neocortex[i]
    #mu[i] <- alpha + beta_mass*mass[i]
    mu[i] <- alpha + beta_nc*neocortex[i] + beta_mass*mass[i]
    
    
  }
  
  #priors
  alpha ~ dnorm(0,0.00001)
  beta_nc ~ dnorm(0,0.00001)
  beta_mass ~ dnorm(0,0.00001)
  sigma ~ dunif(0,50) #dunif has 2 argument (lower, upper) 
  tau <- 1/sigma^2
  
  #predictions 
  
  # for(k in 1:length(x.pred)){
  # 
  #   
  #   
  # 	 } #only needed if doing multiple predictions along an x-gradient
  
  
  
}
