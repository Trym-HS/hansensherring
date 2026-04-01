#' Shiny UI for herring app
#'
#' Defines the user UI-layout of the shiny app.
#'
#' @return A shiny UI object
#' @importFrom shiny fluidPage titlePanel sidebarLayout mainPanel plotOutput
#' @importFrom leaflet leafletOutput
#' @export
herring_ui <- function() {
  shiny::fluidPage(
    shiny::titlePanel("0-group Herring "),
    shiny::sidebarLayout(
      shiny::sidebarPanel(),
      shiny::mainPanel(
        leaflet::leafletOutput("map"),
        shiny::plotOutput("length_hist"),
        shiny::plotOutput("weight_hist")
      )
    )
  )
}
