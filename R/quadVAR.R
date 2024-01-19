#' Estimate lag-1 quadratic vector autoregression models
#'
#' This function estimate regularized nonlinear quadratic vector autoregression models with strong hierarchy using the [RAMP::RAMP()] algorithm, and also compare it with the linear AR, regularized VAR, and unregularized (full) VAR and quadratic VAR models.
#'
#' @param data A `tibble`, data.frame, or matrix that represents a time series of vectors, with each row as a time step.
#' @param vars A character vector of the variable names used in the model.
#' @inheritParams mlVAR::mlVAR
#' @param donotestimate A character vector of the model names that are not estimated. Possible options include "AR", "VAR", "VAR_full", "quadVAR_full", "all_others", with NULL as the default. If set "all_others", then only a `quadVAR` model will be estimated. For datasets with large number of variables, you may set this parameter to "quadVAR_full" to save time.
#' @param penalty The penalty used for the linear and regularized VAR models. Possible options include "LASSO", "SCAD", "MCP", with "LASSO" as the default.
#' @param tune Tuning parameter selection method. Possible options include "AIC", "BIC", "EBIC", with "EBIC" as the default.
#' @param SIS_options A list of other parameters for the [SIS::tune.fit()] function. This is used for the regularized  VAR models.
#' @param RAMP_options A list of other parameters for the [RAMP::RAMP()] function. This is used for the nonlinear quadratic VAR model.
#' @param object,x An `quadVAR` object.
#' @param silent Whether to print out the results.
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
#' @examples
#' set.seed(1614)
#' data <- sim_4_emo(time = 200, sd = 1)
#' plot(data[,"x1"])
#' qV1 <- quadVAR(data, vars = c("x1", "x2", "x3", "x4"))
#' summary(qV1)
#' coef(qV1)
#' plot(qV1)
#' # Compare the estimation with the true model
#' plot(true_model_4_emo())
#' plot(qV1, value = 0, value_standardized = FALSE, layout = plot(true_model_4_emo())$layout)
#'
#' @export
#' @seealso [linear_quadVAR_network()]
quadVAR <- function(data, vars, dayvar = NULL, beepvar = NULL, penalty = "LASSO", tune = "EBIC", donotestimate = NULL, SIS_options = list(), RAMP_options = list()) {
  # check arguments
  penalty <- toupper(penalty)
  tune <- toupper(tune)

  data <- tibble::as_tibble(data[, c(vars, dayvar, beepvar), drop = FALSE])
  if(any(is.na(data))) {
    data <- stats::na.omit(data)
    cli::cli_warn("Missing values detected in either `vars`, `dayvar`, or `beepvar`. {.pkg quadVAR} does not support data imputation at the moment. Thus, all the time points with missing values have been removed.")
  }

  # remove data points according to the dayvar and beepvar
  index <- find_index(data, dayvar, beepvar)

  data_x <- data[index[[1]], vars]
  data_y <- data[index[[2]], vars]
  d <- ncol(data)

  # check if donotestimate only contains valid options
  if(!is.null(donotestimate)) {
    upper_donotestimate <- toupper(donotestimate)
    if(any(!(upper_donotestimate %in% c("AR", "VAR", "VAR_FULL", "QUADVAR_FULL", "ALL_OTHERS")))) {
      cli::cli_abort(c("Invalid options in donotestimate.", i = "Possible options include: AR, VAR, VAR_full, quadVAR_full, all_others (case insensitive).", "x" = "You've supplied {donotestimate}."))
    }
  }

  # AR models
  if(is.null(donotestimate) || !(("AR" %in% toupper(donotestimate)) || ("ALL_OTHERS" %in% toupper(donotestimate)))) {
    AR_model <- lapply(vars, function(a_var) {
      stats::lm(data_y %>% dplyr::pull(a_var) ~ data_x[, a_var] %>% as.matrix())
    })
  } else {
    AR_model <- NULL
  }

  # VAR models
  if(is.null(donotestimate) || !(("VAR" %in% toupper(donotestimate)) || ("ALL_OTHERS" %in% toupper(donotestimate)))) {
    VAR_model <- lapply(vars, function(a_var) {
      do.call(tune.fit, c(list(x = data_x %>% as.matrix(), y = data_y %>% dplyr::pull(a_var), penalty = ifelse(penalty == "LASSO", "lasso", penalty), tune = tolower(tune)), SIS_options))
    })
  } else {
    VAR_model <- NULL
  }

  # full VAR models
  if(is.null(donotestimate) || !(("VAR_FULL" %in% toupper(donotestimate)) || ("ALL_OTHERS" %in% toupper(donotestimate)))) {
    VAR_model_full <- lapply(vars, function(a_var) {
      stats::lm(data_y %>% dplyr::pull(a_var) ~ ., data = data_x)
    })
  } else {
    VAR_model_full <- NULL
  }

  # quadVAR models
  quadVAR_model <- lapply(vars, function(a_var) {
    do.call(RAMP::RAMP, c(list(X = data_x %>% as.matrix(), y = data_y %>% dplyr::pull(a_var), penalty = penalty, tune = tune), RAMP_options))
  })

  # full quadVAR models
  if(is.null(donotestimate) || !(("QUADVAR_FULL" %in% toupper(donotestimate)) || ("ALL_OTHERS" %in% toupper(donotestimate)))) {
    quadVAR_model_full <- lapply(vars, function(a_var) {
      stats::lm(stats::as.formula(paste("data_y %>% dplyr::pull(a_var) ~ ", paste('poly(', paste(colnames(data_x), collapse = ","), ', degree = 2, raw = TRUE)', sep=''), collapse=" + ")), data = data_x)
    })
  } else {
    quadVAR_model_full <- NULL
  }

  return(structure(list(
    AR_model = AR_model, VAR_model = VAR_model, VAR_model_full = VAR_model_full, quadVAR_model = quadVAR_model, quadVAR_model_full = quadVAR_model_full, data = data, vars = vars, penalty = penalty, tune = tune, SIS_options = SIS_options, RAMP_options = RAMP_options), class = "quadVAR"))
}

#' @describeIn quadVAR Print the summary for a quadVAR object. See [summary.quadVAR()] for details. Note that this function does not automatically print all the coefficients. Use [coef.quadVAR()] to extract the coefficients.
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

  if(is.null(object$AR_model)) {
    output <- output %>% dplyr::filter(`Model` != "AR")
  }
  if(is.null(object$VAR_model)) {
    output <- output %>% dplyr::filter(`Model` != "VAR")
  }
  if(is.null(object$VAR_model_full)) {
    output <- output %>% dplyr::filter(`Model` != "VAR_full")
  }
  if(is.null(object$quadVAR_model)) {
    output <- output %>% dplyr::filter(`Model` != "quadVAR")
  }
  if(is.null(object$quadVAR_model_full)) {
    output <- output %>% dplyr::filter(`Model` != "quadVAR_full")
  }

  minIC <- output$SumIC %>% min()
  output$DiffIC <- output$SumIC - minIC
  output$Weight <- exp(-output$DiffIC/2)/sum(exp(-output$DiffIC/2))
  return(output)
}

#' @describeIn quadVAR Extract the coefficients from a quadVAR object.
#' @export
coef.quadVAR <- function(object, silent = FALSE,  ...) {
  n_var <- object$data %>% ncol()

  output <- data.frame(
    model = rep(1:n_var, each = 2*n_var + choose(n_var, 2)),
    effect = rep(generate_effect_term(n_var), n_var),
    estimate = 0
  )

  for (i in 1:n_var) {
    # first fill in the main effects
    output$estimate[output$model == i & output$effect %in% paste0("X", object$quadVAR_model[[i]]$mainInd)] <- object$quadVAR_model[[i]]$beta.m
    # then fill in the interaction effects
    for (j in 1:length(object$quadVAR_model[[i]]$interInd)) {
      output$estimate[output$model == i & output$effect == object$quadVAR_model[[i]]$interInd[j]] <- object$quadVAR_model[[i]]$beta.i[j]
    }
  }

  output_toprint <- output
  output_toprint$estimate <- round(output_toprint$estimate, 2)

  if(!silent) print(output_toprint)

  invisible(output)
}

generate_effect_term <- function(n_var) {
  output_linear <- paste0("X", 1:n_var)
  output_quad <- tidyr::crossing(Var1 = 1:n_var, Var2 = 1:n_var) %>%
    dplyr::filter(Var1 <= Var2) %>%
    apply(c(1,2), function(x) paste0("X", x)) %>%
    apply(1, function(x) paste0(x, collapse = "")) %>%
    as.character()
  output <- c(output_linear, output_quad)
  return(output)
}

#' @describeIn quadVAR Produce a plot for the linearized quadVAR model. Equivalent to first produce a linear quadVAR network using [linear_quadVAR_network()], then use [plot.linear_quadVAR_network()].
#' @export
#' @inheritParams linear_quadVAR_network
plot.quadVAR <- function(x, value, value_standardized = TRUE, interactive = FALSE, ...) {
  if(rlang::is_missing(value)) {
    cli::cli_inform(c("i" = "The {.pkg quadVAR} model, being {.strong nonlinear}, generates a network {.strong meaningful only for specific variable values}. If values are unspecified, the plot defaults to the mean, but this may not be meaningful in all cases."), .frequency = "regularly", .frequency_id = "plot.quadVAR")
    value <- 0
  }
  linear_result <- linear_quadVAR_network(x, value = value, value_standardized = value_standardized)
  plot(linear_result, interactive = interactive, ...)
}
