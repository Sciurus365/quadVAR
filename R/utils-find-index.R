#' Find index of data that satisfies certain conditions
#'
#' @param data A data frame.
#' @inheritParams mlVAR::mlVAR
#' @return A list of two vectors of indices.
#'
find_index <- function(data, dayvar, beepvar) {
  data_index <- 1:nrow(data)
  all_index <- data.frame(data_index = data_index, data_x_index = data_index, data_y_index = data_index) %>%
    dplyr::mutate(data_y_index = dplyr::lead(data_y_index, 1)) %>%
    stats::na.omit()

  if(!is.null(dayvar)) {
    all_index <- all_index %>%
      dplyr::mutate(same_day = data_index %in% which(diff(data %>% pull(dayvar)) == 0)) %>%
      dplyr::filter(same_day)
  }
  if(!is.null(beepvar)) {
    all_index <- all_index %>%
      dplyr::mutate(consecutive_beep = data_index %in% which(diff(data%>% pull(beepvar)) == 1)) %>%
      dplyr::filter(consecutive_beep)
  }

  return(list(all_index$data_x_index, all_index$data_y_index))
}
