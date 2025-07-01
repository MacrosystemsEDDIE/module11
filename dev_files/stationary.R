# Stationary vs not stationary figures
# Author: Mary Lofton
# Date: 01JUL25

# Simulate a random walk with drift
set.seed(456)
RW_drift <- arima.sim(model = list(order = c(0, 1, 0)), n = 200, mean = 1, sd = 5)
plot(RW_drift, main = "Non-stationary", ylab = "value")

# Generate 100 observations of Gaussian white noise with mean 0 and standard deviation 1
white_noise <- rnorm(200) 

# You can specify mean and standard deviation:
white_noise_custom <- rnorm(200, mean = 1, sd = 5)

# Plot the white noise series
plot.ts(white_noise_custom, main = "Stationary", ylab = "value")
