#' Predict the values of the dependent variables using the quadVAR model
#' @param object A quadVAR object.
#' @param newdata A data frame or tibble containing at least the values of the independent variables, dayvar, and beepvar (if used in model estimation). If NULL, the original data used to fit the model will be used.
#' @param donotpredict NOT IMPLEMENTED YET! A character vector of the model names that are not used for prediction. Possible options include "AR", "VAR", "VAR_full", "quadVAR_full", "all_others", with NULL as the default. If set "all_others", then only a `quadVAR` model will be estimated. For datasets with large number of variables, you may set this parameter to "quadVAR_full" to save time.
#' @param lowerbound A numeric value or a vector with the same length as the number of variables that specifies the lower bound of the predicted values. If the predicted value is less than this value, it will be replaced by this value. The default value is -Inf.
#' @param upperbound A numeric value or a vector with the same length as the number of variables that specifies  the upper bound of the predicted values. If the predicted value is greater than this value, it will be replaced by this value. The default value is Inf.
#' @param with_const A logical value indicating whether to include the constant variables in the prediction. Those variables were automatically excluded in the estimation procedure. The default value is FALSE. When set to TRUE, the lowerbound and upperbound should be a vector with the same length as the number of variables in the model, including the constant variables. The values of the constant variables will be ignored though because their predicted values are always the same, which is the constant value in the input data.
#' @param ... Other arguments passed to the [RAMP::predict.RAMP()] function.
#' @return A data frame or tibble containing the predicted values of the dependent variables. If a value cannot be predicted (e.g., because the corresponding previous time point is not in the data), it will be NA.
#' @export
predict.quadVAR <- function(object, newdata = NULL, donotpredict = NULL, lowerbound = -Inf, upperbound = Inf, with_const = FALSE, ...) {
  if (is.null(newdata)) {
    newdata <- object$data
  }
  data <- tibble::as_tibble(newdata[, c(object$vars, object$dayvar, object$beepvar), drop = FALSE])
  if (any(is.na(data))) {
    data <- stats::na.omit(data)
    cli::cli_warn("Missing values detected in either `vars`, `dayvar`, or `beepvar`. {.pkg quadVAR} does not support data imputation at the moment. Thus, all the time points with missing values have been removed.")
  }

  # check lowerbound and upperbound
  if (length(lowerbound) == 1) lowerbound <- rep(lowerbound, length(object$vars))
  if (length(upperbound) == 1) upperbound <- rep(upperbound, length(object$vars))
  if (with_const & !is.null(object$const_index)){
    lowerbound <- lowerbound[-object$const_index]
    upperbound <- upperbound[-object$const_index]
  }
  if (length(lowerbound) != length(object$vars) | length(upperbound) != length(object$vars)) cli::cli_abort("The length of `lowerbound` and `upperbound` should be either 1 or the same as the number of variables in the model.")

  # remove data points according to the dayvar and beepvar
  index <- find_index(data, object$dayvar, object$beepvar)

  data_x <- data[index[[1]], object$vars]
  data_y <- data[index[[2]], object$vars]
  # data_y_out has the same shape as data_y, but all values are NA
  data_y_out <- apply(data_y, c(1, 2), function(x) NA)

  data_y_out_all <- rep(list(data_y_out), 6)
  names(data_y_out_all) <- c("VAR", "VAR_full", "quadVAR", "quadVAR_full", "AR", "NULL_model")

  # make predictions
  ## for the i-th row in data_x, use model$quadVAR_model[[j]] to predict data_y[i,j]

  for (i in 1:nrow(data_y_out)) {
    for (j in 1:ncol(data_y_out)) {
      if (!is.null(object$VAR_model)) data_y_out_all$VAR[i, j] <- stats::predict(object$VAR_model[[j]]$fit, X = as.matrix(data_x[c(i), ]), lambda = object$VAR_model[[j]]$lambda) %>% constrain(lowerbound[j], upperbound[j])

      if (!is.null(object$VAR_model_full)) data_y_out_all$VAR_full[i, j] <- stats::predict(object$VAR_model_full[[j]], newdata = data_x[c(i), ]) %>% constrain(lowerbound[j], upperbound[j])

      if (!is.null(object$quadVAR_model)) data_y_out_all$quadVAR[i, j] <- stats::predict(object$quadVAR_model[[j]], newdata = as.matrix(data_x[c(i, i), ]))[1] %>% constrain(lowerbound[j], upperbound[j]) # It seems that the predict function in RAMP package does not support the argument `newdata` to be a matrix with a single row. So I duplicate the input data and only use the first output.

      if (!is.null(object$quadVAR_model_full)) data_y_out_all$quadVAR_full[i, j] <- stats::predict(object$quadVAR_model_full[[j]], newdata = data_x[i, ]) %>% constrain(lowerbound[j], upperbound[j])

      if (!is.null(object$AR_model)) {
        lm_coef <- object$AR_model[[j]]$coef
        data_y_out_all$AR[i, j] <- as.numeric(lm_coef[1] + lm_coef[2] * data_x[i, j]) %>% constrain(lowerbound[j], upperbound[j])
      }

      data_y_out_all$NULL_model[i, j] <- mean(object$data_y[, j] %>% unlist() %>% mean(), na.rm = TRUE) %>% constrain(lowerbound[j], upperbound[j])
    }
  }

  data_out <- apply(data[, object$vars], c(1, 2), function(x) NA)



  data_out_all <- rep(list(data_out), 6)
  names(data_out_all) <- c("VAR", "VAR_full", "quadVAR", "quadVAR_full", "AR", "NULL_model")
  for (i in 1:6) {
    data_out_all[[i]][index[[2]], ] <- data_y_out_all[[i]]
  }

  if (with_const) {
    n_vars_total <- length(object$original_vars)
    vars_index <- setdiff(1:n_vars_total, object$const_vars_index)
    data_out_all_vars <- data_out_all
    # add columns to data_out, column names are object$const_vars, those columns all have NAs
    data_out <- matrix(NA, nrow = nrow(data), ncol = n_vars_total)
    colnames(data_out) <- object$original_vars

    data_out_all <- rep(list(data_out), 6)
    names(data_out_all) <- c("VAR", "VAR_full", "quadVAR", "quadVAR_full", "AR", "NULL_model")
    for (i in 1:6) {
      data_out_all[[i]][index[[2]], vars_index] <- data_y_out_all[[i]]
      data_out_all[[i]][index[[2]], object$const_vars_index] <- object$const_vars_value
    }
  }
  return(data_out_all)
}

constrain <- function(x, lower, upper) {
  if (x < lower) {
    return(lower)
  } else if (x > upper) {
    return(upper)
  } else {
    return(x)
  }
}
