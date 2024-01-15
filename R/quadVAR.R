#' Estimate Lag-1 Quadratic Vector Autoregression Models
#'
#' This function estimate regularized nonlinear quadratic vector autoregression models with strong hierarchy using the [RAMP::RAMP()] algorithm, and also compare it with the linear AR, regularized VAR, and unregularized (full) VAR and quadratic VAR models.
#'
#' @param data A `tibble`, data.frame, or matrix that represents a time series of vectors, with each row as a time step.
#' @param vars A character vector of the variable names used in the model.
#' @param penalty The penalty used for the linear and regularized VAR models. Possible options include "LASSO", "SCAD", "MCP", with "LASSO" as the default.
#' @param tune Tuning parameter selection method. Possible options include "AIC", "BIC", "EBIC", with "EBIC" as the default.
#' @param SIS_options A list of other parameters for the [SIS::tune.fit()] function. This is used for the regularized  VAR models.
#' @param RAMP_options A list of other parameters for the [RAMP::RAMP()] function. This is used for the nonlinear quadratic VAR model.
#' @param object An `quadVAR` object.
#' @param ... Not in use.
#'
#' @return An `quadVAR` object that contains the following elements:
#' \itemize{
#'  \item `AR`: A list of linear AR models for each variable.
#'  \item `VAR`: A list of regularized VAR models for each variable.
#'  \item `fullVAR`: A list of unregularized (full) VAR models for each variable.
#'  \item `quadVAR`: A list of regularized nonlinear quadratic VAR models for each variable.
#'  \item `fullquadVAR`: A list of unregularized (full) nonlinear quadratic VAR models for each variable.
#'  \item `data`,`vars`,`penalty`,`tune`,`SIS_options`,`RAMP_options`: The input arguments.
#'  }
#'
#'
#' @export
quadVAR <- function(data, vars, penalty = "LASSO", tune = "EBIC", SIS_options = list(), RAMP_options = list()) {
  # check arguments
  penalty <- toupper(penalty)
  tune <- toupper(tune)

  data <- tibble::as_tibble(data[, vars, drop = FALSE])
  data_x <- data[-nrow(data), ]
  data_y <- data[-1, ]
  d <- ncol(data)

  # AR models
  AR_model <- lapply(vars, function(a_var) {
    stats::lm(data_y %>% dplyr::pull(a_var) ~ data_x[, a_var] %>% as.matrix())
  })

  # VAR models
  VAR_model <- lapply(vars, function(a_var) {
    do.call(tune.fit, c(list(x = data_x %>% as.matrix(), y = data_y %>% dplyr::pull(a_var), penalty = ifelse(penalty == "LASSO", "lasso", penalty), tune = tolower(tune)), SIS_options))
  })

  # full VAR models
  VAR_model_full <- lapply(vars, function(a_var) {
    stats::lm(data_y %>% dplyr::pull(a_var) ~ ., data = data_x)
  })

  # quadVAR models
  quadVAR_model <- lapply(vars, function(a_var) {
    do.call(RAMP::RAMP, c(list(X = data_x %>% as.matrix(), y = data_y %>% dplyr::pull(a_var), penalty = penalty, tune = tune), RAMP_options))
  })

  # full quadVAR models
  quadVAR_model_full <- lapply(vars, function(a_var) {
    stats::lm(stats::as.formula(paste("data_y %>% dplyr::pull(a_var) ~ ", paste('poly(', paste(colnames(data_x), collapse = ","), ', degree = 2, raw = TRUE)', sep=''), collapse=" + ")), data = data_x)
  })

  return(structure(list(
    AR_model = AR_model, VAR_model = VAR_model, VAR_model_full = VAR_model_full, quadVAR_model = quadVAR_model, quadVAR_model_full = quadVAR_model_full, data = data, vars = vars, penalty = penalty, tune = tune, SIS_options = SIS_options, RAMP_options = RAMP_options), class = "quadVAR"))
}

#' @export
print.quadVAR <- function(x, ...) {
  print(summary.quadVAR(x, ...))
}

#' @describeIn quadVAR Summary of a quadVAR object. Different IC definitions used by different packages (which differ by a constant) are unified to make them comparable to each other.
#' @export
summary.quadVAR <- function(object, ...) {
  if (object$tune == "BIC" | object$tune == "EBIC") {
      AR_IC <- object$AR_model %>%
        lapply(function(x) stats::BIC(x) %>% as.numeric()) %>%
        unlist() %>% sum()

      VAR_IC <- object$VAR_model %>%
        lapply(function(x) x$ic) %>%
        unlist() %>% sum()

      VAR_full_IC <- object$VAR_model_full %>%
        lapply(function(x) stats::BIC(x) %>% as.numeric()) %>%
        unlist() %>% sum()

      quadVAR_IC <- object$quadVAR_model %>%
        lapply(function(x) {
          n <- x$y %>% length()
          x$cri.list[x$cri.loc] + n + n*log(2*pi) + 2*log(n)
          }) %>%
        unlist() %>% sum()

      quadVAR_full_IC <- object$quadVAR_model_full %>%
        lapply(function(x) stats::BIC(x) %>% as.numeric()) %>%
        unlist() %>% sum()
  } else if (object$tune == "AIC") {
    AR_IC <- object$AR_model %>%
      lapply(function(x) stats::AIC(x) %>% as.numeric()) %>%
      unlist() %>% sum()

    VAR_IC <- object$VAR_model %>%
      lapply(function(x) x$ic) %>%
      unlist() %>% sum()

    VAR_full_IC <- object$VAR_model_full %>%
      lapply(function(x) stats::AIC(x) %>% as.numeric()) %>%
      unlist() %>% sum()

    quadVAR_IC <- object$quadVAR_model %>%
      lapply(function(x) {
        n <- x$y %>% length()
        x$cri.list[x$cri.loc] + n + n*log(2*pi) + 2*2
      }) %>%
      unlist() %>% sum()

    quadVAR_full_IC <- object$quadVAR_model_full %>%
      lapply(function(x) stats::AIC(x) %>% as.numeric()) %>%
      unlist() %>% sum()
  }

  AR_df <- object$AR_model %>%
    lapply(function(x) summary(x)$fstatistic["numdf"] %>% as.numeric()) %>%
    unlist() %>% sum()
  VAR_df <- object$VAR_model %>%
    lapply(function(x) length(x$beta)) %>%
    unlist() %>% sum()
  VAR_full_df <- object$VAR_model_full %>%
    lapply(function(x) summary(x)$fstatistic["numdf"] %>% as.numeric()) %>%
    unlist() %>% sum()
  quadVAR_df <- object$quadVAR_model %>%
    lapply(function(x) x$df[x$cri.loc]) %>%
    unlist() %>% sum()
  quadVAR_full_df <- object$quadVAR_model_full %>%
    lapply(function(x) summary(x)$fstatistic["numdf"] %>% as.numeric()) %>%
    unlist() %>% sum()

  output <- tibble::tribble(
    ~`Model`, ~`Sumdf`, ~`SumIC`,
    "AR", AR_df, AR_IC,
    "VAR", VAR_df, VAR_IC,
    "VAR_full", VAR_full_df, VAR_full_IC,
    "quadVAR", quadVAR_df, quadVAR_IC,
    "quadVAR_full", quadVAR_full_df, quadVAR_full_IC
  )

  minIC <- output$SumIC %>% min()
  output$DiffIC <- output$SumIC - minIC
  output$Weight <- exp(-output$DiffIC/2)/sum(exp(-output$DiffIC/2))
  return(output)
}
