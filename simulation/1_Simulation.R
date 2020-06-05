#Simulate Branching Process

#Parameters
num_days = 30
r0 = 2.2
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

#Plots
plot.ts(x, ylab = "Num Daily infections")
cum_data <- cumsum(vec_infecteds)
plot.ts(cum_data)


#Rough work
#Inspect gamma density
a = seq(0.0, 10, by = 1)
b = pgamma(a, shape = 6, scale = 1)
plot(a, b)

pgamma(8, shape = 6, scale = 1) - pgamma(6, shape = 6, scale = 1)
