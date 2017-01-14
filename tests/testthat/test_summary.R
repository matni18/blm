context("summary")

test_that("We can print a summary of the blm", {
  #Simulate data:
  beta = 0.2
  w0 = 0.2; w1 = 1.2; w2 = 0.5
  d = data.frame(x=rnorm(5), z=rnorm(5))
  d$y = rnorm(5, w0+w1*d$x+w2*d$z, 1/beta)
  #The model:
  m = y ~ x+z

  #Construct a blm:
  myBlm=blm(m, make_prior(m, 1), beta, d)
  #Store print in a variable:
  mySummary = capture.output(summary(myBlm))
  #TESTS:
  #Check that the output has the right length:
  expect_true(length(mySummary)==23)

  #Check with larger dataset:
  d = data.frame(x=rnorm(20), z=rnorm(20))
  d$y = rnorm(20, w0+w1*d$x+w2*d$z, 1/beta)
  myBlm2 = blm(m, make_prior(m, 1), beta, d)
  mySummary2 = capture.output(summary(myBlm2))
  #Check that the output has the right length:
  expect_true(length(mySummary2)==23)
})