context("plot")

test_that("We can plot the blm", {
  #Simulate data:
  beta = 0.2
  w0 = 0.2; w1 = 1.2; w2 = 0.5
  d = data.frame(x=rnorm(5), z=rnorm(5))
  d$y = rnorm(5, w0+w1*d$x+w2*d$z, 1/beta)
  #The model:
  m = y ~ x+z

  #Construct a bad blm:
  myBlm=blm(m, make_prior(m, 1), beta, d)
  myBlm$beta = NULL
  #Try to plot bad blm:
  expect_error(plot(myBlm))
})