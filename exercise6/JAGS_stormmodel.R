model{
  
  #likelihood
  for(i in 1:y.obs){
    
    storm_severity[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha + b_year*year[i]
    
    
    
  }
  
  #priors
  alpha ~ dnorm(0,0.00001)
  b_year ~ dnorm(0,0.00001)
  sigma ~ dunif(0,50)
  tau = 1/sigma^2
  
  #predictions 
  
  # for(k in 1:length(x.pred)){
  #   output_storm[k]<-(( Vmax * x.pred[k]^n )/( h^n + x.pred[k]^n )) + d
  #   
  #   
  # } #only needed if doing multiple predictions along an x-gradient
  
  
  
}
