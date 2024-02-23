set.seed(0)
n <- 500
p <- 3
x <- matrix(rnorm(n * p), n, p)
eta <- 1 * x[, 1] + 2 * x[, 2] + 3 * x[, 3]
y <- eta + rnorm(n)


temp1_AIC <- RAMP::RAMP(X = x, y = y, penalty = "LASSO", tune = "AIC")
temp1_BIC <- RAMP::RAMP(X = x, y = y, penalty = "LASSO", tune = "BIC")

temp2_AIC <- tune.fit(x = x, y = y, penalty = "lasso", tune = "aic", lambda = 0) %>% suppressWarnings()
temp2_BIC <- tune.fit(x = x, y = y, penalty = "lasso", tune = "bic", lambda = 0) %>% suppressWarnings()


temp3 <- lm(y ~ x[, 1] + x[, 2] + x[, 3])

temp_mimic_quadVAR_AIC <- structure(
  list(
    AR_model = list(temp3),
    VAR_model = list(temp2_AIC),
    VAR_model_full = list(temp3),
    quadVAR_model = list(temp1_AIC),
    quadVAR_model_full = list(temp3),
    tune = "AIC"
  ),
  class = "quadVAR"
)


temp_mimic_quadVAR_BIC <- structure(
  list(
    AR_model = list(temp3),
    VAR_model = list(temp2_BIC),
    VAR_model_full = list(temp3),
    quadVAR_model = list(temp1_BIC),
    quadVAR_model_full = list(temp3),
    tune = "BIC"
  ),
  class = "quadVAR"
)

test_that("AIC and BIC measures are unified across packages", {
  expect_equal(summary(temp_mimic_quadVAR_AIC)$DiffIC, rep(0, 5))
  expect_equal(summary(temp_mimic_quadVAR_BIC)$DiffIC, rep(0, 5))
})
