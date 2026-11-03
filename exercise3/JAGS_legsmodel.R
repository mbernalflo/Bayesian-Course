#JAGS model template

model{
  
  #likelihood
  for(i in 1:y.obs){
    
    y[i] ~ dnorm(mu[i], tau)
    #mu[i] <- alpha + beta_r*right_leg[i]  question 1
    #mu[i] <- alpha + beta_r*(right_leg[i] - mean(right_leg))  question 5
    #mu[i] <- alpha + beta_r*(right_leg[i] - mean(right_leg)) + beta_l*(left_leg[i] - mean(left_leg))  #question 6
    mu[i] <- beta_r*right_leg[i]
    
    
  }
  
  #priors
  #alpha ~ dnorm(0,0.00001)
  beta_r ~ dnorm(0,0.00001)
  #beta_l ~ dnorm(0,0.00001)
  sigma ~ dunif(0,50) #dunif has 2 argument (lower, upper) 
  tau <- 1/sigma^2
  
  #combo <- beta_r + beta_l
  
  #predictions 
  
  # for(k in 1:length(x.pred)){
  # 
  #   
  #   
  # 	 } #only needed if doing multiple predictions along an x-gradient
  
  
  
}
