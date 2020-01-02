library(mvtnorm)
sigma <- matrix(c(4, 2, 2, 6), nrow = 2)
mu <- c(0,0)
test_data <- rmvnorm(n = 1000, mean = mu, sigma = sigma)