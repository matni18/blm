context("residuals")

test_that("We can find the residuals", {
  #Simulate data:
  beta = 0.2
  w0 = 0.2; w1 = 1.2; w2 = 0.5
  d = data.frame(x=rnorm(5), z=rnorm(5))
  d$y = rnorm(5, w0+w1*d$x+w2*d$z, 1/beta)
  #The model:
  m = y ~ x+z

  #Construct a blm:
  myBlm=blm(m, make_prior(m, 1), beta, d)
  #Get the residuals:
  myRes = residuals(myBlm)
  #TESTS:
  #Check that the output has the right dimensions:
  expect_true(all(dim(myRes)==c(5,1)))
  #Check that the class is correct:
  expect_true(class(myRes)=='matrix')

  #Construct a bad blm:
  myBlm$beta = NULL
  #Try to get residuals of bad blm:
  expect_error(residuals(myBlm))
})