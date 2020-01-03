library(mvtnorm)
sigma <- matrix(c(4, 2, 2, 6), nrow = 2)
mu <- c(0,0)
training_data <- rmvnorm(n = 1000, mean = mu, sigma = sigma)
test_data <- rmvnorm(n = 200, mean = mu, sigma = sigma)

test_data_outlier <- rbind(test_data, c(10, 20))

halfspaces_training <- train_depth(data = training_data, n_halfspace = 200)
halfspace_masses_test <- evaluate_depth(data = test_data_outlier, 
  halfspaces = halfspaces_training)
