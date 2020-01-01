### subfunctions for train_depth ###

### check_inputs
## 
check_train_depth <- function(data, n_halfspace, subsample, scope, seed) {
  checkmate::assert(
    # passt das hier mit dem mode so??? was genau ist das jetzt eigentlich 
    # und wieso spezifizieren wir beim data frame den type
    # und bei der Matrix den mode ????
    checkmate::check_data_frame(data, types = "numeric", null.ok = FALSE), 
    checkmate::check_matrix(data, mode = "numeric", null.ok = FALSE), 
    combine = "or"
  )
  
  index_missings <- apply(data, MARGIN = 2, FUN = function(x) any(is.na(x)))
  
  if (sum(index_missings) > 0) warning("rows with missing values were removed")
  
  data_matrix <- as.matrix(data[-index_missings, , drop = FALSE])
  checkmate::assert_true(all(dim(data_matrix) > 0))
  n_complete_obs <- NROW(data_matrix)
  
  checkmate::assert_count(n_halfspace, positive = TRUE)
  checkmate::assert_numeric(subsample, lower = 1/n_complete_obs, 
    upper = 1, len = 1, any.missing = FALSE)
  checkmate::assert_numeric(scope, lower = 0, finite = TRUE, any.missing = FALSE, 
    len = 1)
  
  if (is.null(seed)) warning("specifying a seed is recommended for reproducibility")
  checkmate::assert_integer(seed, any.missing = FALSE, len = 1)
  
  n_subsample <- round(n_complete_obs * subsample)
  
  list(
    "data_matrix" = data_matrix, 
    "n_complete_obs" = n_complete_obs, 
    "n_sumbsample" = n_subsample
  )
}

check_evaluate_depth <- function(data, halfspaces) {
  
}

sample_directions <- function(dimension, n) {
  x <- matrix(rnorm(dimension * n), nrow = dimension, ncol = n)
  # I guess we should set the variance not to 1 but higher in order
  # to avoid getting to small points
  
  ### here we write some functions that ensure that the norm of each vector
  # is large enough (how large no idea) to ensure numeric stabiltiy
  # of dividing by the norm 
  
  
  # why can we use the norm function here (which calculates the matrix-norm)
  # v = v/|v| * |v| * 1
  # dabe ist v/|v| unitär, 1 die adjungierte einer unitären Matrix und Sigma
  # eine Diagonalmatrix, hat also insbesondere die korrekte Form für die SVD
  x <- apply(X = x, MARGIN = 2, FUN = function(x) x/norm(x, type = "2"))
  
  x
}

sample_halfspace_positions <- function(projection_matrix, scope, n_halfspace) {
  minimums <- apply(X = projection_matrix, MARGIN = 2, FUN = min)
  maximums <- apply(X = projection_matrix, MARGIN = 2, FUN = max)
  ranges <- maximums - minimums

  random_numbers <- runif(n_halfspace)
  halfspace_positions <- random_numbers * ranges - minimums # utilize vectorization
  # and inverse transform sampling 
}