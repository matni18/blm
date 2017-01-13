context("make prior constructor")

test_that("We can construct a prior distribution", {
  #The model:
  m = y ~ x+z

  #Construct a blm:
  myPrior=make_prior(m, 1)
  #TESTS:
  #No values should be NULL in the output list:
  expect_true(all(!sapply(myPrior, is.null)))
  #Check that the covariance matrix has the right dimensions:
  expect_true(all(dim(myPrior$sigma)==3))
  #Check that the class is correct:
  expect_true(class(myPrior)=='list')
  #Check that we get an error if we try to input a negative value of alpha:
  expect_error(make_prior(m, -1))
})