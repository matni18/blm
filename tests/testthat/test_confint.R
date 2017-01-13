context("confint")

test_that("We can get the confidence intervals", {
  #Simulate data:
  beta = 0.2
  w0 = 0.2; w1 = 1.2; w2 = 0.5
  d = data.frame(x=rnorm(5), z=rnorm(5))
  d$y = rnorm(5, w0+w1*d$x+w2*d$z, 1/beta)
  #The model:
  m = y ~ x+z

  #Construct a blm:
  myBlm=blm(m, make_prior(m, 1), beta, d)
  #Get confidence interval:
  myConf = confint(myBlm, c("(Intercept)", "x", "z"), level=0.90)
  #TESTS:
  #Check that the output has the right dimensions:
  expect_true(all(dim(myConf$mean)==c(3,1)))
  #Check that the class is correct:
  expect_true(class(myConf)=='data.frame')
  #Throw error if level is out of bounds:
  expect_error(confint(myBlm, c("(Intercept)", "x", "z"), level=2))
  #Check that parm matches parameter names (if not, it results in NA entries in dataframe):
  myConf2 = confint(myBlm, c("(Intercept)", "foo", "bar"))
  expect_true(all(!sapply(myConf, is.na)))

  #Construct a bad blm:
  myBlm$beta = NULL
  #Try to get confint of bad blm:
  expect_error(confint(myBlm, c("(Intercept)", "x", "z")))
})