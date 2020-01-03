library(testthat)

context("exercise a works")

### how can we test that it works ???
# we should add outliers to the data and check whether they are identified
# by the method. In addition to that can maybe test the distribution 
# of the halfspace-masses in case all points are sampled from a normal 

halfspaces 

test_that("train_depth basically works", {
  expect(evaluate_depth(data = tra))
  
})