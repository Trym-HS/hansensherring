#' Shiny server logic for herring app
#'
#' Handles reactivity, map clicks, and plotting.
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @export
herring_server <- function(input, output, session) {

  data <- load_herring_data()
  station_data <- summarize_stations(data)

  output$map <- leaflet::renderLeaflet({
    create_leaflet_map(station_data)
  })

  selected_station <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$map_marker_click, {
    selected_station(input$map_marker_click$id)
  })

  output$length_hist <- shiny::renderPlot({
    req(selected_station())
    plots <- plot_station_histograms(data, selected_station())
    plots$length_plot
  })

  output$weight_hist <- shiny::renderPlot({
    req(selected_station())
    plots <- plot_station_histograms(data, selected_station())
    plots$weight_plot
  })
}
