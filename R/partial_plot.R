#' Make a partial plot of a variable in a model
#' This function takes a quadVAR model as input, and returns a plot of the partial effect of a variable on the dependent variable (controlling all other variables and the intercept), for higher and lower levels of the moderator variable split by the median.
#' @param model A quadVAR model
#' @param y The dependent variable
#' @param x The variable for which the partial effect is plotted
#' @param moderator The moderator variable
#' @return A ggplot object
#' @export
partial_plot <- function(model, y, x, moderator) {
  # check if x, y, moderator are characters (length 1) and are in model$vars
  if(!all(c(x, y, moderator) %in% model$vars) || length(x) != 1 || length(y) != 1 || length(moderator) != 1) {
    cli::cli_abort("x, y, and moderator must be single characters and in model$vars. Possible variables are: {paste(model$vars, collapse = ', ')}")
  }

  index_x <- which(model$vars == x)
  index_y <- which(model$vars == y)
  index_moderator <- which(model$vars == moderator)

  y_value <- dplyr::pull(model$data_y, index_y)
  x_value <- dplyr::pull(model$data_x, index_x)
  x_others <- model$data_x
  x_others[, index_x] <- 0
  y_others <- stats::predict(model$quadVAR_model[[index_y]], as.matrix(x_others))
  y_residuals <- y_value - y_others

  # split the moderator variable by the median
  median_moderator <- stats::median(dplyr::pull(model$data_x, index_moderator))
  moderator_split <- ifelse(dplyr::pull(model$data_x, index_moderator) > median_moderator, "high", "low")

  # create a new data frame with the moderator variable split by the median
  data <- data.frame(x = x_value, moderator = moderator_split, y = y_residuals)

  ggplot2::ggplot(data, ggplot2::aes(x = x, y = y, color = moderator)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm", se = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = x, y = paste0(y, " controled on all other variables"), color = moderator)
}
