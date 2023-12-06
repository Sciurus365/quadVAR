#' Estimate Lag-1 Nested Nonlinear Vector Autoregressive Models
#'
#' This function estimate a set of linear and nonlinear autoregressive models, find the best model according to an information criterion, and compare it with the linear AR and VAR models.
#'
#' @param data A `tibble`, data.frame, or matrix that represents a time series of vectors, with each row as a time step.
#' @param vars A character vector of the variable names used in the model.
#' @param p The maximum order of polynomial feature vector.
#' @param method One of "stepAIC" or "dredge". "stepAIC" uses [MASS::stepAIC()] and perform a backward stepwise selection, and "dredge" uses [MuMIn::dredge()] and search for all possible submodels.
#' @param keep_linear If `TRUE`, the `scope` parameter of [MASS::stepAIC()] or the `fixed` parameter of [MuMIn::dredge()] will be supplemented to indicate all linear terms are kept, so that the VAR model is guaranteed to be a sub-model of the best model selected.
#' @param rank When `method = "dredge"`, the same as the `rank` parameter in [MuMIn::dredge()]. Here the default is "AIC" to be consistent with [MASS::stepAIC()]. When `method = "stepAIC"`, it is also possible to use `BIC` here.
#' @param ... Other parameters passed to [MuMIn::dredge()] or [MASS::stepAIC()] for model selection.
#'
#' @return An `NVAR2` object that contains `data`, `data_td` (a tidy form of `tibble` that contains the training data), `W_out` (the fitted coefficients), and `parameters`.
#'
#' @export
NVAR <- function(data, vars, p, method = c("stepAIC", "dredge"), keep_linear = TRUE, rank = "AIC", ...) {
  data <- tibble::as_tibble(data[, vars, drop = FALSE])
  d <- ncol(data)

  # full models
  expressions <- make_I_expressions(vars, p)
  formula_all <- construct_formula(vars, expressions)
  td <- make_tidy_data(data, vars, make_e_linear(vars)) # tidy data directly used for model estimation

  method <- method[1]

  # full model
  full_model <- lapply(formula_all, function(one_formula) {
    stats::lm(one_formula, data = td, na.action = na.fail)
  })

  # VAR models
  expressions_VAR <- make_I_expressions(vars, p = 1)
  formula_all_VAR <- construct_formula(vars, expressions_VAR)
  VAR_model <- lapply(formula_all_VAR, function(one_formula) {
    stats::lm(one_formula, data = td, na.action = na.fail)
  })

  # AR models
  formula_all_AR <- construct_formula_AR(vars, expressions_VAR)
  AR_model <- lapply(formula_all_AR, function(one_formula) {
    stats::lm(one_formula, data = td, na.action = na.fail)
  })

  # find the best submodel
  best_model <- lapply(formula_all, function(one_formula) {
    if (method == "dredge") {
      if (keep_linear) {
        fixed <- make_I_expressions(vars, p = 1) %>% lapply(rlang::expr_deparse) %>% unlist()
        result2 <- MuMIn::dredge(
          stats::lm(one_formula, data = td, na.action = na.fail),
          rank = rank, fixed = fixed,
          ...
        )
      } else {
        result2 <- MuMIn::dredge(
          stats::lm(one_formula, data = td, na.action = na.fail),
          rank = rank,
          ...
        )
      }
      best_model <- eval(attr(result2, "model.calls")[[1]])
      attr(best_model, "raw") <- result2
      return(
        best_model
      )
    } else if (method == "stepAIC") {
      if(rank == "AIC") {
        k <- 2
      } else if(rank == "BIC") {
        k <- log(nrow(td))
      } else {
        stop("`rank` can only be 'AIC' or 'BIC' for `method = 'stepAIC'.")
      }

      if(keep_linear) {
        scope <- list(lower = construct_formula_linear_scope(vars, expressions_VAR))
        return(MASS::stepAIC(
          stats::lm(one_formula, data = td, na.action = na.fail),
          direction = "backward", trace = FALSE, scope = scope, k = k,
          ...
        ))
      } else {
        return(MASS::stepAIC(
          stats::lm(one_formula, data = td, na.action = na.fail),
          direction = "backward", trace = FALSE, k = k,
          ...
        ))
      }
    } else {
      stop("`method` should be either 'dredge' or 'stepAIC'.")
    }
    ## dredge not feasible for nvar > 4. Maybe find stepwise solution. It doesn't have to be the best, but relatively okay. I only want to know what's the best AIC. also need to detect AIC for linear VAR and AR.
  })



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
