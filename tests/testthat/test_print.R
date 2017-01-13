context("print")

test_that("We can print the blm", {
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
  myPrint = capture.output(print(myBlm))
  #TESTS:
  #Check that the output has the right length:
  expect_true(length(myPrint)==16)

})