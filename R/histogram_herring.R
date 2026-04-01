#' Create histograms for selected station
#'
#' Generates histograms of length and weight of 0-group herring for a given station.
#'
#' @param data Full herring dataset
#' @param station_id Selected station number
#' @return A list with ggplot objects
#' @importFrom ggplot2 ggplot aes geom_histogram labs theme_minimal
#' @export
plot_station_histograms <- function(data, station_id) {
  station_data <- data[data$station_nr == station_id, ]

  p_len <- ggplot2::ggplot(station_data, ggplot2::aes(x = len_mm)) +
    ggplot2::geom_histogram(bins = 20, fill = "steelblue") +
    ggplot2::labs(title = paste("Length distribution - Station", station_id),
                  x = "Length (mm)", y = "Count") +
    ggplot2::theme_minimal()

  p_wgt <- ggplot2::ggplot(station_data, ggplot2::aes(x = wgt_g)) +
    ggplot2::geom_histogram(bins = 20, fill = "darkred") +
    ggplot2::labs(title = paste("Weight distribution - Station", station_id),
                  x = "Weight (g)", y = "Count") +
    ggplot2::theme_minimal()

  list(length_plot = p_len, weight_plot = p_wgt)
}
