# Define parameters
beta <- 0.3  # Infection rate
gamma <- 0.0001  # Recovery rate
initial_susceptible <- 990  # Initial number of susceptible
initial_infected <- 10  # Initial number of infected
total_population <- 1000  # Total population size
time_steps <- 100  # Number of time steps

# Initialize compartments
S <- c(initial_susceptible)
I <- c(initial_infected)
R <- c(0)

for (t in 2:time_steps) {
  new_infections <- round(beta * S[t-1] * I[t-1] / total_population)
  new_recoveries <- round(gamma * I[t-1])
  
  S <- c(S, S[t-1] - new_infections)
  I <- c(I, I[t-1] + new_infections - new_recoveries)
  R <- c(R, R[t-1] + new_recoveries)
}

plot(1:time_steps, S, type="l", col="blue", xlab="Time", ylab="Number of Individuals", main="SIR Model Simulation", ylim=c(0,1000))
lines(1:time_steps, I, type="l", col="red")
lines(1:time_steps, R, type="l", col="green")
legend("topright", legend=c("Susceptible", "Infected", "Recovered"), col=c("blue", "red", "green"), lty=1)

