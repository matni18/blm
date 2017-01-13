context("responselessModelMatrix, internal function")

test_that("We can construct a responseless model matrix", {
  #Simulate data:
  beta = 0.2
  w0 = 0.2; w1 = 1.2; w2 = 0.5
  d = data.frame(x=rnorm(5), z=rnorm(5))
  d$y = rnorm(5, w0+w1*d$x+w2*d$z, 1/beta)
  #The model:
  m = y ~ x+z

  #Construct a responseless model matrix:
  myRMM=responselessModelMatrix(m, d)
  #TESTS:
  #Check that the model matrix has the right dimensions:
  expect_true(all(dim(myRMM[1])==c(5,3)))
  #Check that the class is correct:
  expect_true(class(myRMM)=='matrix')
})