#Simulate Branching Process 

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
