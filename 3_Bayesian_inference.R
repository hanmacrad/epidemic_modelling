##To do
#1. Download JAGs
#2. Inspect results

#Packages
library(R2OpenBUGS)
library(rjags)
library(coda)
library(MCMCvis)

source("1_simulation.R")
source("3_bayesian_inference.R")

#Params
r0 = 3.5
num_days = 45
shape_gamma = 6
scale_gamma = 1

#Data
y = simulate_branching(num_days, r0, shape_gamma, scale_gamma)
  
#Model
model_bayesian = function(){
  
  #Priors
  r0 ~ exp(1)
  
  #Likelihood
  #Params
  num_days = length(y)
  
  #Infectiousness (Discrete gamma)
  prob_infect = pgamma(c(1:num_days), shape = shape_gamma, scale = scale_gamma) - pgamma(c(0:(num_days-1)), shape = shape_gamma, scale = scale_gamma)
  likel = 1
  
  for (t in 2:num_days) {
    
    lambda = r0*sum(y[1:t-1]*rev(prob_infect[1:t-1]))
    likel = likel*lambda^(y[t])*exp(-lambda)
    
  }
  
}

#Write Model
model.file = "model.file"
write.model(model_bayesian, model.file)

#Initial values
inits <- NULL

#Parameters to track
params = c('r0')

#Hyperparameters
# number of iterations
ni = 10000
# burn in interval
nb = 1000
# thinning interval
nt = 1
# number of chains
nc = 3

# compile model
jmod = jags.model(file = model.file, data = y, n.chains = nc, inits = inits, n.adapt = 1000)

