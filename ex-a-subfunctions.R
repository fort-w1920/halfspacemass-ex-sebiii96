### subfunctions for train_depth ###

### check_inputs

### check_data: 
## this is used for more buth functions of ex a
check_data <- function(data) {
  checkmate::assert(
    # why do we have to specify the type for data frame and the mode for matrix?
    checkmate::check_data_frame(data, types = "numeric", null.ok = FALSE), 
    checkmate::check_matrix(data, mode = "numeric", null.ok = FALSE), 
    combine = "or"
  )
  
  index_missings <- apply(data, MARGIN = 1, FUN = function(x) any(is.na(x)))
  if (sum(index_missings) > 0) warning("rows with missing values were removed")
  as.matrix(data[!index_missings, , drop = FALSE])
}

### check train_depth
check_train_depth <- function(data, n_halfspace, subsample, scope, seed) {

  data_matrix <- check_data(data)
  checkmate::assert_true(all(dim(data_matrix) > 0))
  n_complete_obs <- NROW(data_matrix)
  
  checkmate::assert_count(n_halfspace, positive = TRUE)

  checkmate::assert_numeric(subsample, lower = 1/n_complete_obs, 
    upper = 1, len = 1, any.missing = FALSE)
  checkmate::assert_numeric(scope, lower = 0, finite = TRUE, any.missing = FALSE, 
    len = 1)
  
  if (is.null(seed)) warning("specifying a seed is recommended for reproducibility")
  checkmate::assert_integerish(seed, any.missing = FALSE, len = 1, null.ok = TRUE)
  
  n_subsample <- round(n_complete_obs * subsample)
  
  list(
    "data_matrix" = data_matrix, 
    "n_complete_obs" = n_complete_obs, 
    "n_subsample" = n_subsample
  )
}


check_evaluate_depth <- function(data, halfspaces) {
  # checl the data
  data_matrix <- check_data(data)
  n_complete_obs <- NROW(data_matrix)
  
  
  ## check halfspaces
  # check names
  # we also have to check the names
  checkmate::assert(
    checkmate::check_list(halfspaces, any.missing = FALSE, len = 4), 
    checkmate::check_true(all(names(halfspaces) == 
        c("halfspace_directions", "halfspace_positions", "mass_below", "mass_above"))), 
    combine = "and")
  # basic tests regarding the type of the data
  checkmate::assert(
    checkmate::check_matrix(halfspaces[["halfspace_directions"]],
      any.missing = FALSE),
    checkmate::check_numeric(halfspaces[["halfspace_positions"]],
      any.missing = FALSE),
    checkmate::check_numeric(halfspaces[["mass_below"]], 
      any.missing = FALSE),
    checkmate::check_numeric(halfspaces[["mass_above"]], 
      any.missing = FALSE)
  )
  n_halfspace <- length(halfspaces[["halfspace_positions"]])
  
  # check that the dimesions of the objects fit together: 
  # mass above gives the proportion of halfspaces for which a point of the 
  # sample is on above/below the halfspace. this means there are as many
  # points as observation
  # however the halfspace positions have as many columns as the halfspace_positions
  # as there are as many directions as positions
  checkmate::assert(
    checkmate::check_true(
      length(halfspaces[["mass_above"]]) == length(halfspaces[["mass_below"]])), 
    checkmate::check_true(n_halfspace == NCOL(halfspaces[["halfspace_directions"]])), 
    combine = "and"
  )


  list(
    "data_matrix" = data_matrix, 
    "n_complete_obs" = n_complete_obs, 
    "n_halfspace" = n_halfspace
  )
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

sample_index_matrix <- function(x, size, times) {
  replicate(
    n = times, 
    expr = sample(x, size = size)
  )
}

sample_halfspace_positions <- function(projection_matrix, scope, n_halfspace) {
  minimums <- apply(X = projection_matrix, MARGIN = 1, FUN = min)
  maximums <- apply(X = projection_matrix, MARGIN = 1, FUN = max)
  ranges <- maximums - minimums

  random_numbers <- runif(n_halfspace)
  halfspace_positions <- random_numbers * ranges + minimums # utilize vectorization
  # and inverse transform sampling 
}