# Pseudocode 

sample_index_matrix <- function()
  
### ALGO 1: train_depth ###
## what it does: 
# we sample halfspaces and get the proportions of points on each side of 
# the half-space
  
  
  
train_depth <- function(data, n_halfspace, subsample = 1, scope = 1, seed = NULL) {
  checked_inputs <- check_train_depth(data = data, n_halfspace = n_halfspace, 
    subsample = subsample, scope = scope, seed = seed)
  data_matrix <- checked_inputs[["data_matrix"]]
  n_complete_obs <- checked_inputs[["n_complete_obs"]]
  n_subsample <- round(n_complete_obs * subsample) # transforms the proportion into the size
  dimension <- NCOL(data_matrix)
  
  
  if (!is.null(seed)) set.seed(seed)
  
  # index matrix has dim = c(sample_size, n_halfspace)
  index_matrix <- sample_index_matrix(n = n_complete_obs, size = n_subsample, 
    n = n_halfspace) # index samples are stored in the columns and by 
  # indices and not logical to save memory
  
  # we simply sample from a circle with radius 1, as we can choose lambda this
  # does not restrict us in any way
  # direction_matrix has dim = c(dimension, n_halfspace)
  halfspace_directions <- sample_directions(dimension = dimension, n
  
  # we loop over a vector 1:n_halfspace and pass the index matrix as well as the
  # data-matrix seperately and let the function itself get the corresponding column
  # projection matrix has dim = c(sample_size, n_halfspace)
  projection_matrix <- vapply(X = 1:n_halfspace, FUN.VALUE = numeric(n_subsample), 
    FUN = function(x) data_matrix[x,] %*% halfspace_directions[,x])
  
  halfspace_positions <- sample_halfspace_positions(projection_matrix)
  # this gives back the s_i
  
  # now we have to count for each iteratio how many projections are < s_i and >= s_i
  
  projection_matrix < matrix(halfspace_positions, ncol = n_halfspace, byrow = T)
  
  mass_below = colMeans(matrix(projection_matrix)) # we add the matrix just that 
  # we avoid any conversion to vectors in case n_halfspace = 1 or n_subsample = 1
  
  mass_above = 1 - mass_below
  
  list(
    "halfspace_directions" = halfspace_directions, 
    "halfspace_positions" = halfspace_positions, 
    "mass_below" = mass_below,
    "mass_above" = mass_above
  )
}


### ALGO 2: evaluate_deth ###
## inputs: 
#   - data: validation-/ testdata
#   - halfspaces: a list of the form specified in the function above
## outpt: a vector that contains the half-space for each point in the validation set 



evaluate_depth <- function(data, halfspaces) {
  
  
}