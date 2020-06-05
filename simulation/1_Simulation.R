#Simulate Branching Process

#Parameters
num_days = 30
r0 = 2.5
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
        num_new_infecteds = num_new_infecteds + round(rpois(1, r0) * dgamma(t - t_inf, shape = shape_gamma, scale = scale_gamma))
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
x = seq(0.0, 10, by = 1)
y = dgamma(x = x, shape = 6, scale = 1)
plot(x, y)
