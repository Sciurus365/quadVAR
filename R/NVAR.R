#' Estimate Lag-1 Nonlinear Vector Autoregressive Models
#'
#' This function estimate regularized nonlinear quadratic vector autoregressive models with the [RAMP::RAMP()] and compare it with the linear AR and regularized VAR models.
#'
#' @param data A `tibble`, data.frame, or matrix that represents a time series of vectors, with each row as a time step.
#' @param vars A character vector of the variable names used in the model.
#' @param penalty The penalty used for the linear and regularized VAR models. Possible options include "LASSO", "SCAD", "MCP", with "LASSO" as the default.
#' @param tune Tuning parameter selection method. Possible options include "AIC", "BIC", "EBIC", with "EBIC" as the default.
#' @param SIS_options A list of other parameters for the [SIS::tune.fit()] function. This is used for the regularized  VAR models.
#' @param SIS_options A list of other parameters for the [RAMP::RAMP()] function. This is used for the nonlinear quadratic VAR model.
#'
#' @return An `NVAR2` object that contains `data`, `data_td` (a tidy form of `tibble` that contains the training data), `W_out` (the fitted coefficients), and `parameters`.
#'
#' @export
NVAR <- function(data, vars, penalty = "LASSO", tune = "EBIC", ...) {
  # check arguments
  penalty <- toupper(penalty)
  tune <- toupper(tune)

  data <- tibble::as_tibble(data[, vars, drop = FALSE])
  data_x <- data[-nrow(data), ]
  data_y <- data[-1, ]
  d <- ncol(data)

  # intercept-only models
  Intercept_model <- lapply(vars, function(a_var) {
    lm(data_y %>% dplyr::pull(a_var) ~ 1)
  })

  # Saturated model
  Saturated_model <- lapply(vars, function(a_var) {
    lm(data_y %>% dplyr::pull(a_var) ~ data_x %>% as.matrix())
  })

  # AR models
  AR_model <- lapply(vars, function(a_var) {
    # SIS::tune.fit(x = data_x[, a_var, drop = FALSE] %>% as.matrix(), y = data_y %>% dplyr::pull(a_var), penalty = ifelse(penalty == "LASSO", "lasso", penalty), tune = tolower(tune), ...)
    lm(data_y %>% dplyr::pull(a_var) ~ data_x[, a_var, drop = FALSE] %>% as.matrix())
  })

  # VAR models
  VAR_model <- lapply(vars, function(a_var) {
    tune.fit(x = data_x %>% as.matrix(), y = data_y %>% dplyr::pull(a_var), penalty = ifelse(penalty == "LASSO", "lasso", penalty), tune = tolower(tune), lm_null = Intercept_model[[1]], ...)
    # [1] is wrong here. just for testing.
  })
  # why don't it return the value, i.e., EBIC?

  # NVAR models
  NVAR_model <- lapply(vars, function(a_var) {
    RAMP::RAMP(X = data_x %>% as.matrix(), y = data_y %>% dplyr::pull(a_var), penalty = penalty, tune = tune, ...)
  })

  return(list(Intercept_model = Intercept_model, Saturated_model = Saturated_model, AR_model = AR_model, VAR_model = VAR_model, NVAR_model = NVAR_model, data = data, vars = vars))
  # summary_all_models <-

  # best model

  return(structure(list(
    full_model = full_model,
    best_model = best_model, VAR_model = VAR_model, AR_model = AR_model,
    data = data, data_tidy = td, vars = vars, p = p, method = method, rank = rank, keep_linear = keep_linear
  ), class = "NVAR"))
}

#' @export
print.NVAR <- function(x, ...) {
  summary.NVAR(x, ...)
}

#' @export
summary.NVAR <- function(object, ...) {
  if (object$method == "stepAIC" & object$rank == "BIC") {
      k <- log(nrow(object$data_tidy))
  } else {
    k <- 2
  }
  full_model_AIC <- object$full_model %>%
    lapply(stats::extractAIC, k = k) %>%
    do.call(rbind, .) %>%
    colSums()
  best_model_AIC <- object$best_model %>%
    lapply(stats::extractAIC, k = k) %>%
    do.call(rbind, .) %>%
    colSums()
  VAR_AIC <- object$VAR_model %>%
    lapply(stats::extractAIC, k = k) %>%
    do.call(rbind, .) %>%
    colSums()
  AR_AIC <- object$AR_model %>%
    lapply(stats::extractAIC, k = k) %>%
    do.call(rbind, .) %>%
    colSums()
  tibble::tribble(
    ~`Model`, ~`Sumdf`, ~`SumIC`,
    "Full", full_model_AIC[1], full_model_AIC[2],
    "Best", best_model_AIC[1], best_model_AIC[2],
    "VAR", VAR_AIC[1], VAR_AIC[2],
    "AR", AR_AIC[1], AR_AIC[2]
  )
}
