#' Predict the values of the dependent variables using the quadVAR model
#' @param object A quadVAR object.
#' @param newdata A data frame or tibble containing at least the values of the independent variables, dayvar, and beepvar (if used in model estimation). If NULL, the original data used to fit the model will be used.
#' @param donotpredict NOT IMPLEMENTED YET! A character vector of the model names that are not used for prediction. Possible options include "AR", "VAR", "VAR_full", "quadVAR_full", "all_others", with NULL as the default. If set "all_others", then only a `quadVAR` model will be estimated. For datasets with large number of variables, you may set this parameter to "quadVAR_full" to save time.
#' @param ... Other arguments passed to the [RAMP::predict.RAMP()] function.
#' @return A data frame or tibble containing the predicted values of the dependent variables. If a value cannot be predicted (e.g., because the corresponding previous time point is not in the data), it will be NA.
#' @export
predict.quadVAR <- function(object, newdata = NULL, donotpredict = NULL, ...) {
  if (is.null(newdata)) {
    newdata <- object$data
  }
  data <- tibble::as_tibble(newdata[, c(object$vars, object$dayvar, object$beepvar), drop = FALSE])
  if (any(is.na(data))) {
    data <- stats::na.omit(data)
    cli::cli_warn("Missing values detected in either `vars`, `dayvar`, or `beepvar`. {.pkg quadVAR} does not support data imputation at the moment. Thus, all the time points with missing values have been removed.")
  }

  # remove data points according to the dayvar and beepvar
  index <- find_index(data, object$dayvar, object$beepvar)

  data_x <- data[index[[1]], object$vars]
  data_y <- data[index[[2]], object$vars]
  # data_y_out has the same shape as data_y, but all values are NA
  data_y_out <- apply(data_y, c(1, 2), function(x) NA)

  data_y_out_all <- rep(list(data_y_out), 5)
  names(data_y_out_all) <- c("VAR", "VAR_full", "quadVAR", "quadVAR_full", "AR")

  # make predictions
  ## for the i-th row in data_x, use model$quadVAR_model[[j]] to predict data_y[i,j]

  for (i in 1:nrow(data_y_out)) {
    for (j in 1:ncol(data_y_out)) {
      if (!is.null(object$VAR_model)) data_y_out_all$VAR[i, j] <- stats::predict(object$VAR_model[[j]]$fit, X = as.matrix(data_x[c(i), ]), lambda = object$VAR_model[[j]]$lambda)

      if (!is.null(object$VAR_model_full)) data_y_out_all$VAR_full[i, j] <- stats::predict(object$VAR_model_full[[j]], newdata = data_x[c(i), ])

      if (!is.null(object$quadVAR_model)) data_y_out_all$quadVAR[i, j] <- stats::predict(object$quadVAR_model[[j]], newdata = as.matrix(data_x[c(i, i), ]))[1] # It seems that the predict function in RAMP package does not support the argument `newdata` to be a matrix with a single row. So I duplicate the input data and only use the first output.

      if (!is.null(object$quadVAR_model_full)) data_y_out_all$quadVAR_full[i, j] <- stats::predict(object$quadVAR_model_full[[j]], newdata = data_x[i, ])

      if (!is.null(object$AR_model)) {
        lm_coef <- object$AR_model[[j]]$coef
        data_y_out_all$AR[i, j] <- as.numeric(lm_coef[1] + lm_coef[2] * data_y[i, j])
      }
    }
  }

  data_out <- apply(data[, object$vars], c(1, 2), function(x) NA)

  data_out_all <- rep(list(data_out), 5)
  names(data_out_all) <- c("VAR", "VAR_full", "quadVAR", "quadVAR_full", "AR")
  for(i in 1:5){
    data_out_all[[i]][index[[2]],] <- data_y_out_all[[i]]
  }

  return(data_out_all)
}
