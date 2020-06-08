#Simulate Branching Process 

#Version 0
#Parameters
r0 = 1.1
num_days = 30

#Function
simulate_branching = function(num_days, r0){
  
  #Set up
  vec_num_infecteds = vector('numeric', num_days)
  vec_num_infecteds[1] = 1
  
  for (t in 2: num_days){
    #print(t)
    cat("time t: ", t)
    num_new_infecteds = 0
    for (i in 1: vec_num_infecteds[t-1]){
      cat("individual i: ", i)
      num_new_infecteds = num_new_infecteds + rpois(1, r0)
      cat("num_new_infecteds: ", num_new_infecteds)
    }
    vec_num_infecteds[t] = num_new_infecteds
  }
  
  vec_num_infecteds
}

#Implement
x = simulate_branching(30, r0)

#Plots
plot.ts(x, ylab="Num Daily infections")
cum_data <-cumsum(vec_num_infecteds)
plot.ts(cum_data)


#Version 0.1
#Simulate Branching Process

#Parameters
num_days = 30
r0 = 1.1
shape_gamma = 6
scale_gamma = 1


#Function
simulate_branching = function(num_days, r0, shape_gamma, scale_gamma) {
  #Set up
  vec_infecteds = c() #vector('numeric', num_days)
  vec_infecteds[1] = 1
  
  for (t in 2:num_days) {
    num_new_infecteds = 0
    for (t_inf in 1:length(vec_infecteds)) { #Loop through each past time index
      
      for (i in 1:vec_infecteds[t_inf]) { #Get infectiousness of each individual at each tinf
        num_new_infecteds = num_new_infecteds + rpois(1, r0 * pgamma(t - t_inf, shape = shape_gamma, scale = scale_gamma))
      }
    }
    vec_infecteds[t] = num_new_infecteds
  }
  
  vec_infecteds
}

#Implement
x = simulate_branching(num_days, r0, shape_gamma, scale_gamma)

#Plots
plot.ts(x, ylab = "Num Daily infections")
cum_data <- cumsum(vec_infecteds)
plot.ts(cum_data)


#Rough work
#Inspect gamma density
a = seq(0.0, 10, by = 1)
b = pgamma(a, shape = 6, scale = 1)
plot(a, b)

pgamma(2, shape = 6, scale = 1)

#Version 0.2
#Function
simulate_branching = function(num_days, r0, shape_gamma, scale_gamma) {
  #Set up
  vec_infecteds = c() #vector('numeric', num_days)
  vec_infecteds[1] = 1
  
  for (t in 2:num_days) {
    num_new_infecteds = 0
    
    for (t_inf in 1:length(vec_infecteds)) { #Loop through each past time index
      
      for (i in 1:vec_infecteds[t_inf]) { #Get infectiousness of each individual at each tinf
        
        #Infectiousness (Discrete gamma)
        ti2 = t - t_inf
        ti1 = ti2 - 1
        prob_infect = pgamma(ti2, shape = shape_gamma, scale = scale_gamma) - pgamma(ti1, shape = shape_gamma, scale = scale_gamma)
        
        num_new_infecteds = num_new_infecteds + rpois(1, r0 * prob_infect)
      }
    }
    vec_infecteds[t] = num_new_infecteds
  }
  
  vec_infecteds
}

#Implement
x = simulate_branching(num_days, r0, shape_gamma, scale_gamma)

#Version 0.3
simulate_branching = function(num_days, r0, shape_gamma, scale_gamma) {
  #Set up
  vec_infecteds = c() #vector('numeric', num_days)
  vec_infecteds[1] = 1
  
  for (t in 2:num_days) {
    
    #num_new_infecteds = 0
    t_inf = 1:length(vec_infecteds)
    
    #Infectiousness (Discrete gamma)
    ti2 = t - t_inf #Vectorwise 
    ti1 = ti2 - 1
    prob_infect = pgamma(ti2, shape = shape_gamma, scale = scale_gamma) - pgamma(ti1, shape = shape_gamma, scale = scale_gamma)
    #print(prob_infect)
    
    #New infections (Poisson)
    #vec_new_infecteds = rpois(vec_infecteds, r0 * prob_infect)
    print()
    #avec_infecteds[t] = length(rpois(vec_infecteds, r0 * prob_infect))
  }
  
  vec_infecteds
}

#Vector indexing
for(j in (n:L)){
  sel <- (j-n+1):j
  signal[j] <- sum(abs(V_s[sel] - V_b[sel])) / (n*V)
}