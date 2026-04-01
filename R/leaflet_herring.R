#' Create Leaflet map of Barents Sea stations
#'
#' Generates a Leaflet map with station markers and hover labels.
#'
#' @param station_data Aggregated station data
#' @return A Leaflet map object
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#' @export
create_leaflet_map <- function(station_data) {
  leaflet::leaflet(station_data) |>
    leaflet::addTiles() |>
    leaflet::setView(lng = 30, lat = 75, zoom = 3) |>
    leaflet::addCircleMarkers(
      lng = ~lon,
      lat = ~lat,
      layerId = ~station_nr,
      radius = 6,
      color = "blue",
      fillOpacity = 0.8,
      label = ~paste0(
        "Station: ", station_nr,
        "Mean length: ", round(mean_length, 1), " mm",
        "Mean weight: ", round(mean_weight, 1), " g",
        "Total catch: ", total_catch
      )
    )
}
