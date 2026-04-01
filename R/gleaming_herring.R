#' Run the herring Shiny app
#'
#' Launches the Leaflet-shiny app with the 0-group herring station data overlaid
#'
#' @importFrom shiny shinyApp
#' @export
run_herring_app <- function() {
  shiny::shinyApp(ui = herring_ui(), server = herring_server)
}

