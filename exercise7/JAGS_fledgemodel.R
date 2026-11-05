
model{
  
  #likelihood
  
  for(i in 1:y.obs){
    
    y[i] ~ dbin(p[i], n[i])
    logit(p[i]) <- alpha + b1*fem_age[i] + b2*breed_date[i] #add additional terms as needed
    n[i] <- clutch_size[i]
    y.sim[i] ~ dbin(p[i], n[i]) 
    
  }#close loop i
  
  #priors	
  
  b1 ~ dnorm(0,0.00001) #dnorm has 2 arguments (mean, precision)
  b2 ~ dnorm(0,0.00001)
  alpha ~ dnorm(0,0.00001)
  
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
    sq[j] <- (y[j]-p[j]*n[j])^2 #insert expected outcome from likelihood
    sq.sim[j] <- (y.sim[j]-p[j]*n[j])^2 #insert expected outcome from likelihood
  }
  fit <- sum(sq[])
  fit.sim <- sum(sq.sim[])
  p.fit <- step(fit.sim-fit)
  
} #close model

## MUST have at least 1 blank line after model closes
