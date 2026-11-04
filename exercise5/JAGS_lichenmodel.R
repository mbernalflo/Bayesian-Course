model{
  
  #likelihood
  for(i in 1:y.obs){
    
    n_lichens[i] ~ dpois(lambda[i])
    lambda[i] <- ( ( Vmax * trans_dist[i]^n )/( h^n + trans_dist[i]^n ) ) + d
    
    
    
  }
  
  #priors
  Vmax ~ dgamma(0.001,0.001)
  h ~ dnorm(0,0.0001)
  n ~ dnorm(-1,0.0001)
  d ~ dunif(0, 1)
  
  #predictions 
  
  for(k in 1:length(x.pred)){
    output_lichen[k]<-(( Vmax * x.pred[k]^n )/( h^n + x.pred[k]^n )) + d


  	 } #only needed if doing multiple predictions along an x-gradient
  
  
  
}
