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

  # AR models
  AR_model <- lapply(vars, function(a_var) {
    lm(data_y %>% dplyr::pull(a_var) ~ data_x[, a_var, drop = FALSE] %>% as.matrix())
  })

  # VAR models
  VAR_model <- lapply(vars, function(a_var) {
    tune.fit(x = data_x %>% as.matrix(), y = data_y %>% dplyr::pull(a_var), penalty = ifelse(penalty == "LASSO", "lasso", penalty), tune = tolower(tune), ...)
  })

  # NVAR models
  NVAR_model <- lapply(vars, function(a_var) {
    RAMP::RAMP(X = data_x %>% as.matrix(), y = data_y %>% dplyr::pull(a_var), penalty = penalty, tune = tune, ...)
  })

  return(structure(list(
    AR_model = AR_model, VAR_model = VAR_model, NVAR_model = NVAR_model, data = data, vars = vars, penalty = penalty, tune = tune, others = list(...)
  ), class = "NVAR"))
}

#' @export
print.NVAR <- function(x, ...) {
  summary.NVAR(x, ...)
}

#' @export
summary.NVAR <- function(object, ...) {
  if (object$tune == "BIC" | object$tune == "EBIC") {
      AR_IC <- object$AR_model %>%
        lapply(function(x) stats::BIC(x) %>% as.numeric()) %>%
        unlist() %>% sum()

      VAR_IC <- object$VAR_model %>%
        lapply(function(x) x$ic) %>%
        unlist() %>% sum() ### WARNING!! EBIC not implemented yet. this is BIC. also check EBIC for NVAR

      NVAR_IC <- object$NVAR_model %>%
        lapply(function(x) {
          n <- x$y %>% length()
          x$cri.list[x$cri.loc] + n + n*log(2*pi) + 2*log(n)
          }) %>%
        unlist() %>% sum()
  } else if (object$tune == "AIC") {
    AR_IC <- object$AR_model %>%
      lapply(function(x) stats::AIC(x) %>% as.numeric()) %>%
      unlist() %>% sum()

    VAR_IC <- object$VAR_model %>%
      lapply(function(x) x$ic) %>%
      unlist() %>% sum()

    NVAR_IC <- object$NVAR_model %>%
      lapply(function(x) {
        n <- x$y %>% length()
        x$cri.list[x$cri.loc] + n + n*log(2*pi) + 2*2
      }) %>%
      unlist() %>% sum()
  }

  AR_df <- object$AR_model %>%
    lapply(function(x) summary(x)$fstatistic["numdf"] %>% as.numeric()) %>%
    unlist() %>% sum()
  VAR_df <- object$VAR_model %>%
    lapply(function(x) length(x$beta)) %>%
    unlist() %>% sum()
  NVAR_df <- object$NVAR_model %>%
    lapply(function(x) x$df[x$cri.loc]) %>%
    unlist() %>% sum()

  tibble::tribble(
    ~`Model`, ~`Sumdf`, ~`SumIC`,
    "AR", AR_df, AR_IC,
    "VAR", VAR_df, VAR_IC,
    "NVAR", NVAR_df, NVAR_IC
  )
}
