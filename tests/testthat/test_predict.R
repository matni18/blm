context("predict")

test_that("We can predict the response of new data", {
  #Simulate data:
  beta = 0.2
  w0 = 0.2; w1 = 1.2; w2 = 0.5
  d = data.frame(x=rnorm(5), z=rnorm(5))
  d$y = rnorm(5, w0+w1*d$x+w2*d$z, 1/beta)
  #The model:
  m = y ~ x+z

  #new data:
  d2 = data.frame(x=rnorm(5), z=rnorm(5))
  d2$y = rnorm(5, w0+w1*d2$x+w2*d2$z, 1/beta)

  #Construct a blm:
  myBlm=blm(m, make_prior(m, 1), beta, d)
  #Get the fit:
  myPred = predict(myBlm, d2)
  #TESTS:
  #Check that the output matrix has the right dimensions:
  expect_true(all(dim(myPred)==c(5,2)))
  #Check that the class is correct:
  expect_true(class(myPred)=='data.frame')

  #Construct a bad blm:
  myBlm$beta = NULL
  #Try to predict using bad blm:
  expect_error(predict(myBlm, d2))
})

