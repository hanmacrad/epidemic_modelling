#Simulate Branching Process

#Parameters
num_days = 20
r0 = 2.2
shape_gamma = 6
scale_gamma = 1


#Function
simulate_branching = function(num_days, r0, shape_gamma, scale_gamma) {
  #Set up
  vec_infecteds = vector('numeric', num_days)
  vec_infecteds[1] = 1
  
  #Infectiousness (Discrete gamma)
  prob_infect = pgamma(c(1:num_days), shape = shape_gamma, scale = scale_gamma) - pgamma(c(0:(num_days-1)), shape = shape_gamma, scale = scale_gamma)
  
  for (t in 2:num_days) {
    
    #Total rate
    tot_rate = r0*sum(vec_infecteds[1:t-1]*rev(prob_infect[1:t-1]))
    vec_infecteds[t] = rpois(1, tot_rate)
  }
  
  vec_infecteds
}

#Implement
start_time = Sys.time()
x = simulate_branching(num_days, r0, shape_gamma, scale_gamma)
end_time = Sys.time()
time_elap = end_time - start_time
#print(time_elap)
x

#Plots
plot.ts(x, ylab = "N Daily infections")
cum_data <- cumsum(x)
plot.ts(cum_data)


#Rough work
#Inspect gamma density
a = seq(0.0, 10, by = 1)
b = pgamma(a, shape = 6, scale = 1)
plot(a, b)



