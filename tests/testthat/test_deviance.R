context("deviance")

test_that("We can get the deviance", {
  #Simulate data:
  beta = 0.2
  w0 = 0.2; w1 = 1.2; w2 = 0.5
  d = data.frame(x=rnorm(5), z=rnorm(5))
  d$y = rnorm(5, w0+w1*d$x+w2*d$z, 1/beta)
  #The model:
  m = y ~ x+z

  #Construct a blm:
  myBlm=blm(m, make_prior(m, 1), beta, d)
  #Get coefficients:
  myDev = deviance(myBlm)
  #TESTS:
  #Deviance should not be NULL:
  expect_false(is.null(myDev))
  #Check that deviance is >=0:
  expect_true(myDev >= 0)
  #Check that the class is correct:
  expect_true(class(myDev)=='numeric')

  #Construct a bad blm:
  myBlm$beta = NULL
  #Try to get deviance of bad blm:
  expect_error(deviance(myBlm))
})