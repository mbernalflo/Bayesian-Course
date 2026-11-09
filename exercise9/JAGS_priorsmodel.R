
model{
  
  #likelihood
  
  for(i in 1:y.obs){
    
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha + treat_b*b[i] + treat_c*c[i] #add additional terms as needed
    
    y.sim[i] ~ dnorm(mu[i], tau)  #here use the SAME likelihood and parameters as above but it is called y.sim[i] instead of y[i]
    
  }#close likelihood loop i
  
  #priors	
  
  treat_b ~ dnorm(0,0.00001) #dnorm has 2 arguments (mean, precision)
  treat_c ~ dnorm(0,0.00001)
  alpha ~ dnorm(0,0.00001)
  sigma ~ dunif(10,60) #dunif has 2 argument (lower, upper) 
  tau <- 1/sigma^2
  
  #for plotting the priors
  treat_b_prior ~ dnorm(0,0.00001) #dnorm has 2 arguments (mean, precision)
  treat_c_prior ~ dnorm(0,0.00001)
  alpha_prior ~ dnorm(0,0.00001)
  sigma_prior ~ dunif(10,60) #dunif has 2 argument (lower, upper) 

  
  #predictions
  #here we use a LOOP to generate multiple predictions
  #ONLY use a loop for multiple prediction generation
  #if you only need to estimate 1 or a few derived variables...don't use a loop, just ask for what you want directly 
  
  output_a <- alpha
  output_b <- alpha + treat_b
  output_c <- alpha + treat_c
  diff_ba <- output_b - output_a
  diff_ca <- output_c - output_a
  diff_bc <- output_b - output_c
  
  
#   
#   for(j in 1:length(x.pred)){
#     
#     output[j]<-alpha + b1*5 + b2*x.pred[j] #expected value of y when age=5 and date=ranges from 1 to 10
#     
#   } #close prediction loop j
#   
  #model fit calculations
  #means
  mean.y <-mean(y[]) #y is the name of your response variable
  mean.y.sim <-mean(y.sim[]) #y.sim is name of your sim variable
  p.mean <-step(mean.y.sim - mean.y)

  #coeff variation
  cv.y <- sd(y[ ])/mean(y[ ]) #y is the name of your response variable
  cv.y.sim <- sd(y.sim[])/mean(y.sim[ ]) #y.sim is name of your sim variable
  p.cv <- step(cv.y.sim-cv.y)

  #sum of squares
  for(j in 1:y.obs){
    sq[j] <- (y[j]-mu[j])^2 #insert expected outcome from likelihood
    sq.sim[j] <- (y.sim[j]-mu[j])^2 #insert expected outcome from likelihood
  }
  fit <- sum(sq[])
  fit.sim <- sum(sq.sim[])
  p.fit <- step(fit.sim-fit)

 } #close model

## MUST have at least 1 blank line after model closes
