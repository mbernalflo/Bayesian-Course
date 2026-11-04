model{
  
  #likelihood
  for(i in 1:y.obs){
    
    storm_severity[i] ~ dnorm(mu[i], tau[i])
    mu[i] <- alpha + b_year*year[i]
    log(sigma[i]) <- alpha_sigma + b_sigma*year[i]
    tau[i] <- 1/sigma[i]^2
    
    
    
  }
  
  #priors
  alpha ~ dnorm(0,0.00001)
  alpha_sigma ~ dnorm(0,0.0001)
  b_year ~ dnorm(0,0.00001)
  b_sigma ~ dnorm(0,0.00001)
  #sigma ~ dunif(0,50)
  #tau = 1/sigma^2
  
  #predictions 
  
  for(k in 1:length(x.pred)){
    output_storm[k]<-alpha + b_year*x.pred[k]
    log(output_sigma[k]) <- alpha_sigma + b_sigma*x.pred[k]
    output_tau[k] <- 1/output_sigma[k]^2
    sim_out[k] ~ dnorm(output_storm[k], output_tau[k])


  } #only needed if doing multiple predictions along an x-gradient
  
  
  
}
