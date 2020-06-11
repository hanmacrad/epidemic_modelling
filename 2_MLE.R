#Maximum Likelihood Estimation**
source("simulation/1_simulation.R")
source("2_mle.R")

#Data
data = simulate_branching(num_days, r0, shape_gamma, scale_gamma)

#log likelihood 
poi_log_like <- function(r0_opt, y){
  
  #Params
  num_days = length(y)
  shape_gamma = 6
  scale_gamma = 1
  
  #Infectiousness (Discrete gamma)
  prob_infect = pgamma(c(1:num_days), shape = shape_gamma, scale = scale_gamma) - pgamma(c(0:(num_days-1)), shape = shape_gamma, scale = scale_gamma)
  logl = 0
  
  for (t in 2:num_days) {
    
    lambda = r0_opt*sum(y[1:t-1]*rev(prob_infect[1:t-1]))
    logl = logl + y[t]*log(lambda) - lambda
    
  }
  
  return(-logl)
  
}

#*********
#Optim
optim(1, poi_log_like, y = data)



#**************************************************
#Optim Test
fr <- function(x) {   ## Rosenbrock Banana function
  x1 <- x[1]
  x2 <- x[2]
  100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}

optim(c(-1.2,1), fr)
