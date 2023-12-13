#' @inherit SIS::tune.fit
#' This function is modified from \code{\link{SIS::tune.fit}}. It is used to tune the regularization parameter for the regularized VAR models. This wrapper is used because the original \code{\link{SIS::tune.fit}} function does not return the value of the information criteria that we would like to use.
#' Also,
tune.fit <- function (x, y, family = c("gaussian", "binomial", "poisson",
                           "cox"), penalty = c("SCAD", "MCP", "lasso"), concavity.parameter = switch(penalty,
                                                                                                     SCAD = 3.7, 3), tune = c("cv", "aic", "bic", "ebic"), nfolds = 10,
          type.measure = c("deviance", "class", "auc", "mse", "mae"),
          gamma.ebic = 1)
{
  if (is.null(x) || is.null(y))
    stop("The data is missing!")
  this.call = match.call()
  family = match.arg(family)
  penalty = match.arg(penalty)
  if (class(concavity.parameter) != "numeric")
    stop("concavity.parameter must be numeric!")
  tune = match.arg(tune)
  if (class(nfolds) != "numeric")
    stop("nfolds must be numeric!")
  type.measure = match.arg(type.measure)
  if (tune == "cv") {
    if (penalty == "lasso") {
      cv.fit = glmnet::cv.glmnet(x, y, family = family, type.measure = type.measure,
                         nfolds = nfolds)
      coef.beta = coef(cv.fit, s = "lambda.1se")
      reg.fit = cv.fit$glmnet.fit
      lambda = cv.fit$lambda.1se
      lambda.ind = which(cv.fit$lambda == cv.fit$lambda.1se)
    }
    else if (family != "cox") {
      cv.fit = ncvreg::cv.ncvreg(x, y, family = family, penalty = penalty,
                         gamma = concavity.parameter, nfolds = nfolds)
      cv.1se.ind = min(which(cv.fit$cve < cv.fit$cve[cv.fit$min] +
                               cv.fit$cvse[cv.fit$min]))
      coef.beta = cv.fit$fit$beta[, cv.1se.ind]
      reg.fit = cv.fit$fit
      lambda = cv.fit$lambda[cv.1se.ind]
      lambda.ind = cv.1se.ind
    }
    else {
      cv.fit = ncvreg::cv.ncvsurv(x, y, family = family, penalty = penalty,
                          gamma = concavity.parameter, nfolds = nfolds)
      cv.1se.ind = min(which(cv.fit$cve < cv.fit$cve[cv.fit$min] +
                               cv.fit$cvse[cv.fit$min]))
      coef.beta = cv.fit$fit$beta[, cv.1se.ind]
      reg.fit = cv.fit$fit
      lambda = cv.fit$lambda[cv.1se.ind]
      lambda.ind = cv.1se.ind
    }
  }
  else {
    n = nrow(x)
    if (penalty == "lasso") {
      reg.fit = glmnet::glmnet(x, y, family = family)
      coef.beta = rbind(reg.fit$a0, as.matrix(reg.fit$beta))
      dev = deviance(reg.fit)
      reg.df = reg.fit$df
    }
    else {
      if (family != "cox") {
        reg.fit = ncvreg::ncvreg(x, y, family = family, penalty = penalty,
                         gamma = concavity.parameter)
        coef.beta = reg.fit$beta
        dev = loglik(x, y, coef.beta, family = family)
        reg.df = getdf(coef.beta[-1, , drop = FALSE])
      }
      else {
        reg.fit = ncvreg::ncvsurv(x, y, family = family, penalty = penalty,
                          gamma = concavity.parameter)
        coef.beta = reg.fit$beta
        dev = 2 * reg.fit$loss
        reg.df = getdf(coef.beta)
      }
    }
    if (tune == "aic") {
      obj = dev + 2 * reg.df
    }
    if (tune == "bic") {
      obj = dev + log(n) * reg.df
    }
    if (tune == "ebic") {
      obj = dev + log(n) * reg.df + 2 * gamma.ebic * log(choose(dim(x)[2],
                                                                reg.df))
    }
    lambda.ind = which.min(obj)
    coef.beta = coef.beta[, lambda.ind]
    lambda = reg.fit$lambda[lambda.ind]
    ic <- obj[lambda.ind]
  }
  if (family != "cox") {
    a0 = coef.beta[1]
    coef.beta = coef.beta[-1]
  }
  else {
    a0 = NULL
    coef.beta = as.vector(coef.beta)
  }
  ix = which(coef.beta != 0)
  beta = coef.beta[ix]
  return(list(ix = ix, a0 = a0, beta = beta, fit = reg.fit,
              lambda = lambda, lambda.ind = lambda.ind, ic = ic))
}
