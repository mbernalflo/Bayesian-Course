
model{
  
  #likelihood
  
  for(i in 1:y.obs){
    
    z[i] ~ dbern(p_z[i])
    logit(p_z[i]) <- intercept + b_forest*forest[i] + b_elevation1*elevation[i] + b_elevation2*(elevation[i])^2 
    n_detection[i] ~ dbinom(p*z[i], n_surveys[i])
    
    #y.sim[i] ~ dnorm(mu[i], tau)  #here use the SAME likelihood and parameters as above but it is called y.sim[i] instead of y[i]
    
  }#close likelihood loop i
  
  #priors	
  
  intercept ~ dnorm(0,0.00001) #dnorm has 2 arguments (mean, precision)
  b_forest ~ dnorm(0,0.00001)
  b_elevation1 ~ dnorm(0,0.00001)
  b_elevation2 ~ dnorm(0,0.00001)
  p ~ dunif(0,1)

  # 
  # #for plotting the priors
  # treat_b_prior ~ dnorm(0,0.00001) #dnorm has 2 arguments (mean, precision)
  # treat_c_prior ~ dnorm(0,0.00001)
  # alpha_prior ~ dnorm(0,0.00001)
  # sigma_prior ~ dunif(10,60) #dunif has 2 argument (lower, upper) 
  
  
  #predictions
  #here we use a LOOP to generate multiple predictions
  #ONLY use a loop for multiple prediction generation
  #if you only need to estimate 1 or a few derived variables...don't use a loop, just ask for what you want directly 
  
    logit(max_e_st) <- (-1*b_elevation1)/(2*b_elevation2)
    max_e <- max_e_st*sd_elev + mean_elev #back transform from scaling
  

    for(k in 1:length(x.pred_e_scaled)){

      logit(output_pz_elev[k])<-intercept + b_forest*mean(forest) + b_elevation1*x.pred_e_scaled[k] + b_elevation2*(x.pred_e_scaled[k])^2 #expected value of y when age=5 and date=ranges from 1 to 10

    } #close prediction loop j
  
  for(t in 1:length(x.pred_f_scaled)){
    logit(output_pz_forest[t]) <- intercept + b_forest*x.pred_f_scaled[t] + b_elevation1*mean(elevation) + b_elevation2*(mean(elevation))^2
  }
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
  # for(j in 1:y.obs){
  #   sq[j] <- (y[j]-mu[j])^2 #insert expected outcome from likelihood
  #   sq.sim[j] <- (y.sim[j]-mu[j])^2 #insert expected outcome from likelihood
  # }
  # fit <- sum(sq[])
  # fit.sim <- sum(sq.sim[])
  # p.fit <- step(fit.sim-fit)
  
} #close model

## MUST have at least 1 blank line after model closes
