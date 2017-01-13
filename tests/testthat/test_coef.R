context("coef")

test_that("We can get the coefficients", {
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
  myCoefs = coef(myBlm)
  #TESTS:
  #No values should be NULL in the output list:
  expect_true(all(!sapply(myCoefs, is.null)))
  #Check that the covariance matrix has the right dimensions:
  expect_true(all(dim(myCoefs$sigma)==3))
  #Check that the mean has the right dimensions:
  expect_true(all(dim(myCoefs$mean)==c(3,1)))
  #Check that the class is correct:
  expect_true(class(myCoefs)=='list')

  #Construct a bad blm:
  myBlm$beta = NULL
  #Try to get coef of bad blm:
  expect_error(coef(myBlm))
})