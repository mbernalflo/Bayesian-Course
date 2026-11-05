
model{
  
  #likelihood
  
  for(i in 1:y.obs){
    
    number.exotics[i] ~ dpois(lambda[i])
    log(lambda[i]) <- alpha[study.area[i]] + b_dist*disturbance[i]
    
    #y.sim[i] ~ likelihood(parameter1, parameter2)  #here use the SAME likelihood and parameters as above but it is called y.sim[i] instead of y[i]
    
  }#close likelihood loop i
  
  for(j in 1:n.groups){
    alpha[j] ~ dnorm(mu_alpha[j], tau_alpha)
    mu_alpha[j] <- (a_alpha * road.density[j])/(b_alpha + road.density[j])
  }
  
  #priors	
  
  b_dist ~ dnorm(0,0.00001) #dnorm has 2 arguments (mean, precision)
  #alpha ~ dnorm(0,0.00001)
  #mu_alpha ~ dnorm(0,0.0001)
  a_alpha ~ dunif(0,5)
  b_alpha ~ dunif(0,2)
  sigma_alpha ~ dunif(0,50) #dunif has 2 argument (lower, upper) 
  tau_alpha <- 1/sigma_alpha^2
  
  #predictions
  #here we use a LOOP to generate multiple predictions
  #ONLY use a loop for multiple prediction generation
  #if you only need to estimate 1 or a few derived variables...don't use a loop, just ask for what you want directly 
  
  for(k in 1:length(x.pred)){

    log(out_lambda[k]) <- (a_alpha * x.pred[k])/(b_alpha + x.pred[k]) + b_dist*mean(disturbance) #expected value of y when age=5 and date=ranges from 1 to 10

  } #close prediction loop k
  
  # for(k in 1:length(x.pred)){
  # 
  #   log(out_lambda[k]) <- mu_alpha + b_dist*x.pred[k] #expected value of y when age=5 and date=ranges from 1 to 10
  # 
  # } #close prediction loop k
  
  #for fun
  # for(j in 1:n.groups){
  #   for(k in 1:length(x.pred)){
  #     log(out_lambda[k,j]) <- alpha[j] + b_dist*x.pred[k]
  #   }
  # }
  # 
  #model fit calculations
  #means
  # mean.y <-mean(y[]) #y is the name of your response variable
  # mean.y.sim <-mean(y.sim[]) #y.sim is name of your sim variable
  # p.mean <-step(mean.y.sim - mean.y) 
  # 
  # #coeff variation
  # cv.y <- sd(y[ ])/mean(y[ ]) #y is the name of your response variable
  # cv.y.sim <- sd(y.sim[])/mean(y.sim[ ]) #y.sim is name of your sim variable
  # p.cv <- step(cv.y.sim-cv.y)
  # 
  # #sum of squares
  # for(j in 1:nobs){
  #   sq[j] <- (y[j]-        )^2 #insert expected outcome from likelihood
  #   sq.sim[j] <- (y.sim[j]-         )^2 #insert expected outcome from likelihood
  # }
  # fit <- sum(sq[])
  # fit.sim <- sum(sq.sim[])
  # p.fit <- step(fit.sim-fit)
  # 
} #close model

## MUST have at least 1 blank line after model closes
