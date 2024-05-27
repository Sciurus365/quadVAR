#' Estimate lag-1 quadratic vector autoregression models
#'
#' This function estimate regularized nonlinear quadratic vector autoregression models with strong hierarchy using the [RAMP::RAMP()] algorithm, and also compare it with the linear AR, regularized VAR, and unregularized (full) VAR and quadratic VAR models.
#'
#' @param data A `tibble`, data.frame, or matrix that represents a time series of vectors, with each row as a time step.
#' @param vars A character vector of the variable names used in the model.
#' @inheritParams mlVAR::mlVAR
#' @param donotestimate A character vector of the model names that are not estimated. Possible options include, "NULL_model", "AR", "VAR", "VAR_full", "quadVAR_full", "all_others", with NULL as the default. If set "all_others", then only a `quadVAR` model will be estimated. For datasets with large number of variables, you may set this parameter to "quadVAR_full" to save time.
#' @param penalty The penalty used for the linear and regularized VAR models. Possible options include "LASSO", "SCAD", "MCP", with "LASSO" as the default.
#' @param tune Tuning parameter selection method. Possible options include "AIC", "BIC", "EBIC", with "EBIC" as the default.
#' @param SIS_options A list of other parameters for the [SIS::tune.fit()] function. This is used for the regularized  VAR models.
#' @param RAMP_options A list of other parameters for the [RAMP::RAMP()] function. This is used for the nonlinear quadratic VAR model.
#' @param object,x An `quadVAR` object. (For `print.coef_quadVAR`, an `coef_quadVAR` object returned by [coef.quadVAR()].)
#' @param use_actual_names Logical. If `TRUE`, the actual variable names are used in the output. If `FALSE`, the names "X1", "X2", etc., are used in the output. Default is `TRUE`.
#' @param abbr Logical. If `TRUE`, the output is abbreviated. Default is `FALSE`.
#' @param omit_zero Logical. If `TRUE`, the coefficients that are zero are omitted. Default is `FALSE`.
#' @inheritParams base::abbreviate
#' @inheritParams base::print.data.frame
#' @param ... For `print.quadVAR`, additional arguments passed to [print.coef_quadVAR()]. For `print.coef_quadVAR`, additional arguments passed to [print.data.frame()].
#'
#' @return An `quadVAR` object that contains the following elements:
#' \itemize{
#'  \item `NULL_model`: A list of NULL models for each variable.
#'  \item `AR_model`: A list of linear AR models for each variable.
#'  \item `VAR_model`: A list of regularized VAR models for each variable.
#'  \item `VAR_full_model`: A list of unregularized (full) VAR models for each variable.
#'  \item `quadVAR_model`: A list of regularized nonlinear quadratic VAR models for each variable.
#'  \item `quadVAR_full_model`: A list of unregularized (full) nonlinear quadratic VAR models for each variable.
#'  \item `data`,`vars`,`penalty`,`tune`,`SIS_options`,`RAMP_options`: The input arguments.
#'  \item `data_x`,`data_y`: The data directly used for modeling.
#'  }
#'
#' @examples
#' set.seed(1614)
#' data <- sim_4_emo(time = 200, sd = 1)
#' plot(data[, "x1"])
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
  # check if the package version of RAMP is at least 2.0.3.
  ## this is a temperary solution before the next official release of RAMP
  if (!("RAMP" %in% utils::installed.packages())) {
    cli::cli_abort("Please install the RAMP package from GitHub with the following command: `remotes::install_github('Sciurus365/RAMP')`.")
  } else {
    if (utils::packageVersion("RAMP") < "2.0.3") {
      cli::cli_abort("Please update the RAMP package to at least version 2.0.3 with the following command: `remotes::install_github('Sciurus365/RAMP')`. This is a temporary solution before the next official release of RAMP.")
    }
  }

  # check arguments
  penalty <- toupper(penalty)
  tune <- toupper(tune)

  ## check if penalty is valid
  if (!(penalty %in% c("LASSO", "SCAD", "MCP"))) {
    cli::cli_abort(c("Invalid penalty.", i = "Possible options include: LASSO, SCAD, MCP (case insensitive).", "x" = "You've supplied {penalty}."))
  }

  ## check if tune is valid
  if (!(tune %in% c("AIC", "BIC", "EBIC"))) {
    cli::cli_abort(c("Invalid tune.", i = "Possible options include: AIC, BIC, EBIC (case insensitive).", "x" = "You've supplied {tune}."))
  }

  # select useful variables from the data

  data <- tibble::as_tibble(data[, c(vars, dayvar, beepvar), drop = FALSE])
  if (any(is.na(data))) {
    data <- stats::na.omit(data)
    cli::cli_warn("Missing values detected in either `vars`, `dayvar`, or `beepvar`. {.pkg quadVAR} does not support data imputation at the moment. Thus, all the time points with missing values have been removed.")
  }

  # remove data points according to the dayvar and beepvar
  index <- find_index(data, dayvar, beepvar)

  data_x <- data[index[[1]], vars]
  data_y <- data[index[[2]], vars]
  d <- ncol(data_x)

  # check if there are constant variables
  if (any(apply(data_x, 2, function(x) length(unique(x)) == 1) | apply(data_y, 2, function(x) length(unique(x)) == 1))) {
    cli::cli_warn(c("There are constant variable(s) in the data. The constant variable(s) include {paste(vars[apply(data_y, 2, function(x) length(unique(x)) == 1)], collapse = ', ')}.", "i" = "Those variable(s) will be automatically excluded."))
    const_vars_index <- which(apply(data_x, 2, function(x) length(unique(x)) == 1) | apply(data_y, 2, function(x) length(unique(x)) == 1))
    const_vars <- vars[const_vars_index]
    const_vars_value <- data_x[1, const_vars] %>% unlist()
    original_vars <- vars
    vars <- vars[-const_vars_index]

    data_x <- data[index[[1]], vars]
    data_y <- data[index[[2]], vars]
    d <- ncol(data_x)
  } else {
    original_vars <- vars
    const_vars <- NULL
    const_vars_value <- NULL
    const_vars_index <- NULL
  }

  if (length(vars) <= 1) {
    cli::cli_warn("There is only one variable in the data, which is {vars}. This may cause errors in the RAMP package when estimating the quadVAR model.")
  }

  # check if donotestimate only contains valid options
  if (!is.null(donotestimate)) {
    upper_donotestimate <- toupper(donotestimate)
    if (any(!(upper_donotestimate %in% c("AR", "VAR", "VAR_FULL", "QUADVAR_FULL", "ALL_OTHERS")))) {
      cli::cli_abort(c("Invalid options in donotestimate.", i = "Possible options include: AR, VAR, VAR_full, quadVAR_full, all_others (case insensitive).", "x" = "You've supplied {donotestimate}."))
    }
  }

  # NULL models
  if (is.null(donotestimate) || !(("NULL_MODEL" %in% toupper(donotestimate)) || ("ALL_OTHERS" %in% toupper(donotestimate)))) {
    NULL_model <- lapply(vars, function(a_var) {
      stats::lm(data_y %>% dplyr::pull(a_var) ~ 1)
    })
  } else {
    NULL_model <- NULL
  }

  # AR models
  if (is.null(donotestimate) || !(("AR" %in% toupper(donotestimate)) || ("ALL_OTHERS" %in% toupper(donotestimate)))) {
    AR_model <- lapply(vars, function(a_var) {
      stats::lm(data_y %>% dplyr::pull(a_var) ~ data_x[, a_var] %>% as.matrix())
    })
  } else {
    AR_model <- NULL
  }

  # VAR models
  if (is.null(donotestimate) || !(("VAR" %in% toupper(donotestimate)) || ("ALL_OTHERS" %in% toupper(donotestimate)))) {
    VAR_model <- lapply(vars, function(a_var) {
      do.call(tune.fit, c(list(x = data_x %>% as.matrix(), y = data_y %>% dplyr::pull(a_var), penalty = ifelse(penalty == "LASSO", "lasso", penalty), tune = tolower(tune)), SIS_options))
    })
  } else {
    VAR_model <- NULL
  }

  # full VAR models
  if (is.null(donotestimate) || !(("VAR_FULL" %in% toupper(donotestimate)) || ("ALL_OTHERS" %in% toupper(donotestimate)))) {
    VAR_full_model <- lapply(vars, function(a_var) {
      stats::lm(data_y %>% dplyr::pull(a_var) ~ ., data = data_x)
    })
  } else {
    VAR_full_model <- NULL
  }

  # quadVAR models
  quadVAR_model <- tryCatch(lapply(vars, function(a_var) {
    do.call(RAMP::RAMP, c(list(X = data_x %>% as.matrix(), y = data_y %>% dplyr::pull(a_var), penalty = penalty, tune = tune), RAMP_options))
  }), error = function(e) {
    # find possible interaction terms that are constant from data_x
    constant_interaction_terms <- c()
    for (i in 1:d) {
      for (j in i:d) {
        if (length(unique((data_x %>% dplyr::pull(i)) * (data_x %>% dplyr::pull(j)))) == 1) {
          constant_interaction_terms <- c(constant_interaction_terms, paste(colnames(data_x)[i], colnames(data_x)[j], sep = "*"))
        }
      }
    }
    if (length(constant_interaction_terms) == 0) {
      cli::cli_abort(c("Error in the estimation of quadVAR models.",
        "x" = "Original message from RAMP::RAMP: {e$message}"
      ))
    } else {
      cli::cli_abort(c("Error in the estimation of quadVAR models.",
        "x" = "Original message from RAMP::RAMP: {e$message}",
        "i" = "One possible reason is that some of the interaction terms are constant, although non of the variables are constant. Possible constant interaction term(s) are: {paste(constant_interaction_terms, collapse = ', ')}. Try to add a constant value to the variables in the interaction terms."
      ))
    }
  })

  # full quadVAR models
  if (is.null(donotestimate) || !(("QUADVAR_FULL" %in% toupper(donotestimate)) || ("ALL_OTHERS" %in% toupper(donotestimate)))) {
    quadVAR_full_model <- lapply(vars, function(a_var) {
      stats::lm(stats::as.formula(paste("data_y %>% dplyr::pull(a_var) ~ ", paste("poly(", paste(colnames(data_x), collapse = ","), ", degree = 2, raw = TRUE)", sep = ""), collapse = " + ")), data = data_x)
    })
  } else {
    quadVAR_full_model <- NULL
  }

  return(structure(list(
    NULL_model = NULL_model, AR_model = AR_model, VAR_model = VAR_model, VAR_full_model = VAR_full_model, quadVAR_model = quadVAR_model, quadVAR_full_model = quadVAR_full_model, data = data, data_x = data_x, data_y = data_y, vars = vars, original_vars = original_vars, const_vars = const_vars, const_vars_index = const_vars_index, const_vars_value = const_vars_value,
    dayvar = dayvar, beepvar = beepvar, penalty = penalty, tune = tune, SIS_options = SIS_options, RAMP_options = RAMP_options
  ), class = "quadVAR"))
}

#' @describeIn quadVAR Print the coefficients for a quadVAR object. See [coef.quadVAR()] and [print.coef_quadVAR()] for details.
#' @export
print.quadVAR <- function(x, ...) {
  print(stats::coef(x), ...)
}

#' @describeIn quadVAR Summary of a quadVAR object. Different IC definitions used by different packages (which differ by a constant) are unified to make them comparable to each other.
#' @export
summary.quadVAR <- function(object, ...) {
  if (object$tune == "BIC" | object$tune == "EBIC") {
    NULL_IC <- object$NULL_model %>%
      lapply(function(x) stats::BIC(x) %>% as.numeric()) %>%
      unlist() %>%
      sum()

    AR_IC <- object$AR_model %>%
      lapply(function(x) stats::BIC(x) %>% as.numeric()) %>%
      unlist() %>%
      sum()

    VAR_IC <- object$VAR_model %>%
      lapply(function(x) x$ic) %>%
      unlist() %>%
      sum()

    VAR_full_IC <- object$VAR_full_model %>%
      lapply(function(x) stats::BIC(x) %>% as.numeric()) %>%
      unlist() %>%
      sum()

    quadVAR_IC <- object$quadVAR_model %>%
      lapply(function(x) {
        n <- x$y %>% length()
        x$cri.list[x$cri.loc] + n + n * log(2 * pi) + 2 * log(n)
      }) %>%
      unlist() %>%
      sum()

    quadVAR_full_IC <- object$quadVAR_full_model %>%
      lapply(function(x) stats::BIC(x) %>% as.numeric()) %>%
      unlist() %>%
      sum()
  } else if (object$tune == "AIC") {
    NULL_IC <- object$NULL_model %>%
      lapply(function(x) stats::AIC(x) %>% as.numeric()) %>%
      unlist() %>%
      sum()

    AR_IC <- object$AR_model %>%
      lapply(function(x) stats::AIC(x) %>% as.numeric()) %>%
      unlist() %>%
      sum()

    VAR_IC <- object$VAR_model %>%
      lapply(function(x) x$ic) %>%
      unlist() %>%
      sum()

    VAR_full_IC <- object$VAR_full_model %>%
      lapply(function(x) stats::AIC(x) %>% as.numeric()) %>%
      unlist() %>%
      sum()

    quadVAR_IC <- object$quadVAR_model %>%
      lapply(function(x) {
        n <- x$y %>% length()
        x$cri.list[x$cri.loc] + n + n * log(2 * pi) + 2 * 2
      }) %>%
      unlist() %>%
      sum()

    quadVAR_full_IC <- object$quadVAR_full_model %>%
      lapply(function(x) stats::AIC(x) %>% as.numeric()) %>%
      unlist() %>%
      sum()
  }

  NULL_df <- object$NULL_model %>%
    lapply(function(x) summary(x)$fstatistic["numdf"] %>% as.numeric()) %>%
    unlist() %>%
    sum()
  AR_df <- object$AR_model %>%
    lapply(function(x) summary(x)$fstatistic["numdf"] %>% as.numeric()) %>%
    unlist() %>%
    sum()
  VAR_df <- object$VAR_model %>%
    lapply(function(x) length(x$beta)) %>%
    unlist() %>%
    sum()
  VAR_full_df <- object$VAR_full_model %>%
    lapply(function(x) summary(x)$fstatistic["numdf"] %>% as.numeric()) %>%
    unlist() %>%
    sum()
  quadVAR_df <- object$quadVAR_model %>%
    lapply(function(x) x$df[x$cri.loc]) %>%
    unlist() %>%
    sum()
  quadVAR_full_df <- object$quadVAR_full_model %>%
    lapply(function(x) summary(x)$fstatistic["numdf"] %>% as.numeric()) %>%
    unlist() %>%
    sum()

  output <- tibble::tribble(
    ~`Model`, ~`Sumdf`, ~`SumIC`,
    "NULL_model", NULL_df, NULL_IC,
    "AR", AR_df, AR_IC,
    "VAR", VAR_df, VAR_IC,
    "VAR_full", VAR_full_df, VAR_full_IC,
    "quadVAR", quadVAR_df, quadVAR_IC,
    "quadVAR_full", quadVAR_full_df, quadVAR_full_IC
  )

  if (is.null(object$NULL_model)) {
    output <- output %>% dplyr::filter(`Model` != "NULL_model")
  }
  if (is.null(object$AR_model)) {
    output <- output %>% dplyr::filter(`Model` != "AR")
  }
  if (is.null(object$VAR_model)) {
    output <- output %>% dplyr::filter(`Model` != "VAR")
  }
  if (is.null(object$VAR_full_model)) {
    output <- output %>% dplyr::filter(`Model` != "VAR_full")
  }
  if (is.null(object$quadVAR_model)) {
    output <- output %>% dplyr::filter(`Model` != "quadVAR")
  }
  if (is.null(object$quadVAR_full_model)) {
    output <- output %>% dplyr::filter(`Model` != "quadVAR_full")
  }

  minIC <- output$SumIC %>% min()
  output$DiffIC <- output$SumIC - minIC
  output$Weight <- exp(-output$DiffIC / 2) / sum(exp(-output$DiffIC / 2))
  return(output)
}

#' @describeIn quadVAR Extract the coefficients from a quadVAR object.
#' @export
coef.quadVAR <- function(object, ...) {
  n_var <- length(object$vars)

  output <- data.frame(
    model = rep(1:n_var, each = 2 * n_var + choose(n_var, 2)),
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

  attr(output, "vars") <- object$vars

  # add the class "coef_quadVAR" to the output
  class(output) <- c("coef_quadVAR", class(output))

  return(output)
}

#' @describeIn quadVAR Print the coefficients from a quadVAR object.
#' @export
print.coef_quadVAR <- function(x, use_actual_names = TRUE, abbr = FALSE, minlength = 3, omit_zero = TRUE, digits = 2, row.names = FALSE, ...) {
  if (use_actual_names) {
    name_to_use <- attr(x, "vars")
    if (abbr) {
      name_to_use <- abbreviate(name_to_use, minlength = minlength)
    }
    x$model <- name_to_use[x$model]

    # x$effect may contain one or two terms, named as X1, X2, X1X2, etc. replace them with name_to_use[1], name_to_use[2], name_to_use[1] : name_to_use[2], etc.
    x <- x %>%
      dplyr::mutate(effect = stringr::str_replace(effect, "^X([0-9]+)$", "\\1"),
                    effect = stringr::str_replace(effect, "^X([0-9]+)X([0-9]+)$", "\\1:\\2")) %>%
      # replace all occurrence of numbers to the corresponding variable names
      dplyr::mutate(effect = stringr::str_replace_all(effect, "[0-9]+", function(x) name_to_use[as.numeric(x)]))
  }

  if (omit_zero) {
    x <- x %>% dplyr::filter(estimate != 0)
  }

  print.data.frame(x, digits = digits, row.names = row.names, ...)
}

generate_effect_term <- function(n_var) {
  output_linear <- paste0("X", 1:n_var)
  output_quad <- tidyr::crossing(Var1 = 1:n_var, Var2 = 1:n_var) %>%
    dplyr::filter(Var1 <= Var2) %>%
    apply(c(1, 2), function(x) paste0("X", x)) %>%
    apply(1, function(x) paste0(x, collapse = "")) %>%
    as.character()
  output <- c(output_linear, output_quad)
  return(output)
}

#' @describeIn quadVAR Produce a plot for the linearized quadVAR model. Equivalent to first produce a linear quadVAR network using [linear_quadVAR_network()], then use [plot.linear_quadVAR_network()].
#' @export
#' @inheritParams linear_quadVAR_network
plot.quadVAR <- function(x, value = NULL, value_standardized = TRUE, interactive = FALSE, ...) {
  linear_result <- linear_quadVAR_network(x, value = value, value_standardized = value_standardized)
  plot(linear_result, interactive = interactive, ...)
}
