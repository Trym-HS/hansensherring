#' Summarize herring data by station
#'
#' Computes mean length, mean weight, and total catch of 0-group herring per station.
#'
#' @param data Data frame with herring observations
#' @return A summarized data frame grouped by station
#' @importFrom dplyr group_by summarise n
#' @export
summarize_stations <- function(data) {
  data |>
    dplyr::group_by(station_nr, lat, lon) |>
    dplyr::summarise(
      mean_length = mean(len_mm, na.rm = TRUE),
      mean_weight = mean(wgt_g, na.rm = TRUE),
      total_catch = dplyr::n(),
      .groups = "drop"
    )
}

