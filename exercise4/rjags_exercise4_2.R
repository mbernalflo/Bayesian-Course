rm(list=ls())

library(boot)
library(rjags)
library(coda)
library(scales)
library(corrgram)

load.module("glm")

#file.choose() #use this to find your file and create your working directory pathway

setwd("C:/Users/mabe2494/Documents/PhD/Bayesian Course/exercise4/") #set your working directory to your working folder
milk<-read.csv("milk.csv", header=T) #bring in the data

names(milk)
#"clade"          
#"species"        
#"kcal.per.g" kilocalories of energy per gram of milk as the response variable  
#"perc.fat"   the percent of fat in the milk (predictor)
#"perc.protein"  the percentage of protein in the milk (predictor)
#"perc.lactose"   the percentage of lactose in the milk (predictor)
#"mass"           the average female mass in kg (predictor), we eill use log(mass)
#"neocortex.perc" the percent of total brain mass that is cortex (predictor)

milk <- na.omit(milk)

str(milk)
summary(milk) 

plot(milk)
corrgram(milk, lower.panel = panel.pie, upper.panel = panel.conf)


data <- NULL
data <- list(
  y=milk$kcal.per.g, #the response variable
  y.obs=nrow(milk), #the number of observations
  lactose=as.vector(scale(log(milk$perc.lactose))), #explanatory variable
  #breed_date=as.vector(scale(breed$breeding_date)), #explanatory variable
  fat=as.vector(scale(milk$perc.fat))
  #x.pred=seq(from=min(as.vector(scale(breed$breeding_date))), to=max(as.vector(scale(breed$breeding_date))), by=0.01)
) 
data


# inits <- list(
#   list(
#     alpha=1,
#     b1=0.1,
#     b2=0.1,
#     b_int=0.1
#   ))

inits <- list(
  list(
    alpha=1,
    beta_fat=0.1,
    beta_lact=0.1,
    sigma=0.1
  ),
  list(
    alpha=10,
    beta_fat=0.005,
    beta_lact=0.005,
    sigma=0.1
  ),
  list(
    alpha=-1,
    beta_fat=-0.01,
    beta_lact=-0.01,
    sigma=0.1
  )
)


model <- "JAGS_milkmodel2.R"

jm <- jags.model(model,
                 data=data, 
                 n.adapt=5000, 
                 inits=inits, 
                 #n.chains=1
                 n.chains=3
) 


burn.in <- 100000

update(jm, n.iter=burn.in) 


samples <- 10000
n.thin <- 200

#sample the MCMC chains using CODA

zc <- coda.samples(jm,
                   variable.names=c("alpha",
                                    "beta_fat",
                                    "beta_lact"
                   ), 
                   n.iter=samples, 
                   thin=n.thin)

summary(zc)

#first time only with lactose
#             2.5%     25%     50%     75%   97.5%
#alpha      2.4319  2.7040  2.8144  2.9421  3.1955
#beta_lact -0.6648 -0.5973 -0.5644 -0.5347 -0.4635

#the effect of lactose is clearly negative, and it clearly has an effect

#second time only with fat
#             2.5%      25%     50%    75%   97.5%
#alpha    0.153685 0.247952 0.28742 0.3252 0.41412
#beta_fat 0.007195 0.009353 0.01032 0.0113 0.01364

#the effect of fat seems very small, and very close to zero, but consistently positive nevertheless?

#third time with both lactose and fat
#               2.5%      25%        50%       75%     97.5%
# alpha      1.546481  2.28680  2.6270983  2.980470  3.630407
# beta_fat  -0.003361 -0.00064  0.0009255  0.002469  0.005505
# beta_lact -0.750340 -0.60105 -0.5238700 -0.447179 -0.274473

#fourth time with both scaled
#               2.5%       25%      50%      75%    97.5%
# alpha      0.62450  0.649320  0.65911  0.66748  0.68309
# beta_fat  -0.05474 -0.005513  0.01894  0.04109  0.07042
# beta_lact -0.22379 -0.173961 -0.14965 -0.13177 -0.09204

#you would not put them together because the amount of calories will depend either on the fat or the sugar, but if one goes up the other one should will probably go down and they will cancel each other out (in real life). Also, larger animals might have less fat and more sugar because they can consume more food, while smaller animals will have more fat and less sugar because they need the calories to be more concentrated


plot(zc) 
#only lactose plots
#it passes the diagnostic tests, the chains look fine and the curves ar not extremely flat

#only fat plots
#looks fine, very slightly positive but consistently positive, passes the tests

#plots with both lactose and fat
#the plots look bad. Alpha is the mirror image of lactose and fat, which seem to be very very similar. The density plots are very wide

#plots with both variables scaled
#The chains still look bad, and the density plots have not changed




coda.matrix <- as.matrix(zc[[1]]) #collect the chains as a matrix to save or plot
#bivariate plot for lactose alone without centering
plot(coda.matrix[,1], coda.matrix[,2])
#this suggests they are correlated, but the estimates seem to still be reliable, as they are consistent, the sd is not super wide and they pass the diagnostics

#bivariate plot for fat alone
plot(coda.matrix[,1], coda.matrix[,2])
#this suggests they are correlated, although not as strongly as the other one. It still passes the tests

#bivariate plot for both
plot(coda.matrix[,2], coda.matrix[,3])
plot(coda.matrix[,1], coda.matrix[,3])
plot(coda.matrix[,2], coda.matrix[,1])

#this suggests they are correlated, which reflects the 0.61 correlation coefficient we saw in the plot at the beginning

#bivariate plot for both when scaled
plot(coda.matrix[,2], coda.matrix[,3])
#these 2 are still correlated
plot(coda.matrix[,1], coda.matrix[,3])
plot(coda.matrix[,2], coda.matrix[,1])
#there is no correlation with the intercept anymore

head(coda.matrix)


#MCMC convergence diagnostics (from coda.samples())
raftery.diag(zc)
heidel.diag(zc)
gelman.diag(zc) #needs at least 2 chains
