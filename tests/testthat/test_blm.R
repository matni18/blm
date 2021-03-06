context("blm constructor")

test_that("We can construct a blm object", {
  #Simulate data:
  beta = 0.2
  w0 = 0.2; w1 = 1.2; w2 = 0.5
  d = data.frame(x=rnorm(5), z=rnorm(5))
  d$y = rnorm(5, w0+w1*d$x+w2*d$z, 1/beta)
  #The model:
  m = y ~ x+z

  #Construct a blm:
  myBlm=blm(m, make_prior(m, 1), beta, d)
  #TESTS:
  #No values should be NULL in the output list:
  expect_true(all(!sapply(myBlm, is.null)))
  #Check that the covariance matrix has the right dimensions:
  expect_true(all(dim(myBlm$sigma)==3))
  #Check that the mean has the right dimensions:
  expect_true(all(dim(myBlm$mean)==c(3,1)))
  #Check that the class is correct:
  expect_true(class(myBlm)=='blm')
  #Check that we get an error if we try to input a negative value of beta:
  expect_error(blm(m, make_prior(m, 1), -0.2, d))
})