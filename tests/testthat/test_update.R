context("update, internal function")

test_that("We can construct posterior distribution", {
  #Simulate data:
  beta = 0.2
  w0 = 0.2; w1 = 1.2; w2 = 0.5
  d = data.frame(x=rnorm(5), z=rnorm(5))
  d$y = rnorm(5, w0+w1*d$x+w2*d$z, 1/beta)
  #The model:
  m = y ~ x+z

  #Construct a posterior distribution:
  myPost=update(m, make_prior(m, 1), beta, d)
  #TESTS:
  #No values should be NULL in the output list:
  expect_true(all(!sapply(myPost, is.null)))
  #Check that the covariance matrix has the right dimensions:
  expect_true(all(dim(myPost$sigma)==3))
  #Check that the mean has the right dimensions:
  expect_true(all(dim(myPost$mean)==c(3,1)))
  #Check that the class is correct:
  expect_true(class(myPost)=='list')
  #Check that we get an error if we try to input a negative value of beta:
  expect_error(update(m, make_prior(m, 1), -0.2, d))
  #Check that the data has the right dimensions:
  expect_true(all(dim(myPost$data)==dim(d)))
})