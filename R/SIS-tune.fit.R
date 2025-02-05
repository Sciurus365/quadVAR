#' Using the **glmnet** and **ncvreg** packages, fits a Generalized Linear Model or Cox Proportional Hazards Model using various methods for choosing the regularization parameter \eqn{\lambda}
#'
#' This function is modified from [SIS::tune.fit()]. It is used to tune the regularization parameter for the regularized VAR models. This wrapper is used because of the following reasons.
#' 1. The original [SIS::tune.fit()] function does not return the value of the information criteria that we would like to use.
#' 2. We use the ncvreg package exclusively (so we removed the code using the glmnet package). This is to make the result more consistent, and also because the ncvreg package has better support for the calculation of information criteria.
#' 3. We also removed the generalized linear model (GLM) option, and the cross-validation option because we do not use them.
#' 4. We use stats::AIC() and stats::BIC() instead of the ones using SIS:::loglik() to make the calculation methods more consistent.
#' 5. We added `...` to allow the user to pass additional arguments to the ncvreg::ncvreg() function.
#'
#' Original description from [SIS::tune.fit()]:
#'
#' This function fits a generalized linear model or a Cox proportional hazards model via penalized maximum likelihood, with available penalties as indicated in the **glmnet** and **ncvreg** packages. Instead of providing the whole regularization solution path, the function returns the solution at a unique value of \eqn{\lambda}, the one optimizing the criterion specified in tune.
#' @examples
#' \dontrun{
#' set.seed(0)
#' data("leukemia.train", package = "SIS")
#' y.train <- leukemia.train[, dim(leukemia.train)[2]]
#' x.train <- as.matrix(leukemia.train[, -dim(leukemia.train)[2]])
#' x.train <- standardize(x.train)
#' model <- tune.fit(x.train[, 1:3500], y.train, family = "binomial", tune = "bic")
#' model$ix
#' model$a0
#' model$beta
#' }
#'
#' @inherit SIS::tune.fit
#' @param ... additional arguments to be passed to the ncvreg::ncvreg() function.
tune.fit <- function(x, y, family = "gaussian", penalty = c("SCAD", "MCP", "lasso"), concavity.parameter = switch(penalty,
                       SCAD = 3.7,
                       3
                     ), tune = c("aic", "bic", "ebic"),
                     type.measure = c("deviance", "class", "auc", "mse", "mae"),
                     gamma.ebic = 1, ...) {
  ## SIS:::getdf()
  getdf <- function(coef.beta) {
    apply(abs(coef.beta) > 1e-10, 2, sum)
  }

  if (is.null(x) || is.null(y)) {
    stop("The data is missing!")
  }
  this.call <- match.call()
  penalty <- match.arg(penalty)
  if (!is.numeric(concavity.parameter)) {
    stop("concavity.parameter must be numeric!")
  }
  tune <- match.arg(tune)
  type.measure <- match.arg(type.measure)
  n <- nrow(x)
  reg.fit <- ncvreg::ncvreg(x, y,
    family = family, penalty = penalty,
    gamma = concavity.parameter, ...
  )
  coef.beta <- reg.fit$beta
  reg.df <- getdf(coef.beta[-1, , drop = FALSE])

  if (tune == "aic") {
    obj <- stats::AIC(reg.fit)
  }
  if (tune == "bic") {
    obj <- stats::BIC(reg.fit)
  }
  if (tune == "ebic") {
    obj <- stats::BIC(reg.fit) + 2 * gamma.ebic * log(choose(
      dim(x)[2],
      reg.df
    ))
  }
  lambda.ind <- which.min(obj)
  coef.beta <- coef.beta[, lambda.ind]
  lambda <- reg.fit$lambda[lambda.ind]
  ic <- obj[lambda.ind] %>% as.numeric()
  a0 <- coef.beta[1]
  coef.beta <- coef.beta[-1]

  ix <- which(coef.beta != 0)
  beta <- coef.beta[ix]
  return(list(
    ix = ix, a0 = a0, beta = beta, fit = reg.fit,
    lambda = lambda, lambda.ind = lambda.ind, ic = ic
  ))
}
