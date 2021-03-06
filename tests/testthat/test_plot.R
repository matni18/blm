context("plot")

test_that("We can plot the blm", {
  #Simulate data:
  beta = 0.2
  w0 = 0.2; w1 = 1.2; w2 = 0.5
  d = data.frame(x=rnorm(5), z=rnorm(5))
  d$y = rnorm(5, w0+w1*d$x+w2*d$z, 1/beta)
  #The model:
  m = y ~ x+z

  #Construct blm:
  myBlm=blm(m, make_prior(m, 1), beta, d)
  expect_error(plot(myBlm), NA)

  #Try with larger dataset:
  d = data.frame(x=rnorm(50), z=rnorm(50))
  d$y = rnorm(50, w0+w1*d$x+w2*d$z, 1/beta)
  myBlm=blm(m, make_prior(m, 1), beta, d)
  expect_error(plot(myBlm), NA)

  #Construct a bad blm:
  myBlm$beta = NULL

  #Try to plot bad blm:
  expect_error(plot(myBlm))
})