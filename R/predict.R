#' Predict the values of the dependent variables using the quadVAR model
#' @param object A quadVAR object.
#' @param newdata A data frame or tibble containing at least the values of the independent variables, dayvar, and beepvar (if used in model estimation). If NULL, the original data used to fit the model will be used.
#' @param ... Other arguments passed to the [RAMP::predict.RAMP()] function.
#' @return A data frame or tibble containing the predicted values of the dependent variables. If a value cannot be predicted (e.g., because the corresponding previous time point is not in the data), it will be NA.
#' @export
predict.quadVAR <- function(object, newdata = NULL, ...){
  if(is.null(newdata)) {
    newdata <- object$data
  }
  data <- tibble::as_tibble(newdata[, c(object$vars, object$dayvar, object$beepvar), drop = FALSE])
  if(any(is.na(data))) {
    data <- stats::na.omit(data)
    cli::cli_warn("Missing values detected in either `vars`, `dayvar`, or `beepvar`. {.pkg quadVAR} does not support data imputation at the moment. Thus, all the time points with missing values have been removed.")
  }

  # remove data points according to the dayvar and beepvar
  index <- find_index(data, object$dayvar, object$beepvar)

  data_x <- data[index[[1]], object$vars]
  data_y <- data[index[[2]], object$vars]
  # data_y_out has the same shape as data_y, but all values are NA
  data_y_out <- apply(data_y, c(1,2), function(x) NA)

  # make predictions
  ## for the i-th row in data_x, use model$quadVAR_model[[j]] to predict data_y[i,j]

  for(i in 1:nrow(data_x)){
    for(j in 1:ncol(data_x)){
      data_y_out[i,j] <- stats::predict(object$quadVAR_model[[j]], newdata = as.matrix(data_x[c(i,i),]), ...)[1] # It seems that the predict function in RAMP package does not support the argument `newdata` to be a matrix with a single row. So I duplicate the input data and only use the first output.
    }
  }

  data_out <- apply(data[, object$vars], c(1,2), function(x) NA)
  data_out[index[[2]], object$vars] <- data_y_out

  return(data_out)
}
