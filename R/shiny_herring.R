
#' Launches the "hansensherring" shiny app
#'
#' This function launches the Hansen's Herring shiny app.
#' This app displays a Leaflet-map over the western part of Svalbard Archipelago,
#' with 20 generated trawl stations where herring *C.harengus* is found.
#' catch total, mean weigth and mean length is displayed while hovering over
#' stations, while clicking them displays histograms showing weight and length.
#'
#' @return
#' A shiny app object
#'

#' @importFrom shiny fluidPage titlePanel fluidRow column renderPlot reactive reactiveVal observeEvent req plotOutput shinyApp h4
#' @importFrom leaflet leaflet addTiles setView addCircleMarkers renderLeaflet leafletOutput
#' @importFrom dplyr rename filter group_by summarise n
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_histogram labs
#' @importFrom readxl read_excel

#library(shiny)
#library(leaflet)
#library(dplyr)
#library(ggplot2)
#library(readxl)

# ---------------------------
# DATA FUNCTIONS
# ---------------------------

#' @title Read Herring
#' @description Reads in excel dataset
#' @param path Takes in the path of dataset
#' @return A read dataset
#' @export


read_herring_data <- function(path) {
  readxl::read_excel(path)
}

# Cleaning data for all columns in herring_data.xlsx
#' @title Cleans herring data
#' @description cleans already read dataset
#' @param df Takes in the a dataset
#' @return A cleaned dataset
#' @export
#'
clean_herring_data <- function(df) {
  df %>%
    rename(
      station = station_nr,
      fish_id = ind_nr,
      lat = lat,
      lon = lon,
      length = len_mm,
      weight = wgt_g
    ) %>%
    filter(!is.na(station), !is.na(length), !is.na(weight))
}
# Sorting and further cleaning data for individual fish per station
#' @title Summarizes stations
#' @description groups and calculate mean of fish measurements
#' @param df Takes in the path
#' @return mean measurements and fish amounts grouped by station and location
#' @export

summarize_stations <- function(df) {
  df %>%
    dplyr::group_by(station, lat, lon) %>%
    dplyr::summarise(
      n_fish = n(),
      mean_length = mean(length, na.rm = TRUE),
      mean_weight = mean(weight, na.rm = TRUE),
      .groups = "drop"
    )
}

# ---------------------------
# PLOTTING FUNCTIONS
# ---------------------------

# Plotting both histograms for herring measurements


# Plotting histogram for length of fish per station
#' @title plots length histogram
#' @description plots the aesthetics and the displayed bins for the plot
#' @param df dataframe with excel data
#' @param station_id For station number in original file
#' @return plot for length histogram
#' @export
#'
plot_length_hist <- function(df, station_id) {
  df %>%
    filter(station == station_id) %>%
    ggplot(aes(x = length)) +
    geom_histogram(bins = 20) +
    labs(
      title = paste("Length distribution - Station", station_id),
      x = "Length (mm)",
      y = "Count"
    )
}

# Plotting histogram for weight of fish per station

#' @title plots weigth histogram
#' @description plots the aesthetics and the displayed bins for the plot
#' @param df dataframe with excel data
#' @param station_id # For station number in original file
#' @return plot for weight histogram
#' @export
#'
plot_weight_hist <- function(df, station_id) {
  df %>%
    filter(station == station_id) %>%
    ggplot(aes(x = weight)) +
    geom_histogram(bins = 20) +
    labs(
      title = paste("Weight distribution - Station", station_id),
      x = "Weight (g)",
      y = "Count"
    )
}

# ---------------------------
# UI
# ---------------------------

# Plotting the app's UI

ui <- fluidPage(
  titlePanel("Hansen's Herring"),

  fluidRow(
    column(
      width = 6,
      leaflet::leafletOutput("map", height = 600)
    ),
    column(
      width = 6,
      h4("Station plots"),
      plotOutput("length_plot"),
      plotOutput("weight_plot")
    )
  )
)

# ---------------------------
# SERVER
# ---------------------------

# Running app
#' @title launches the server and the app visuals
#' @description launches the shiny server and the accompanying visuals
#' @param input data from earlier functions
#' @param output final output for shiny app
#' @param session for displaying shiny app in seperate window
#' @return shiny application
#' @export
#'
server <- function(input, output, session) {

  # Loading data from original Excel-file
  df <- reactive({
    read_herring_data("data/herring_data.xlsx") %>%
      clean_herring_data()
  })

  summary_df <- reactive({
    summarize_stations(df())
  })

  # Selected station as an interactive object

  selected_station <- shiny::reactiveVal(NULL)

  # Establishing Leaflet mapping with location markers

  output$map <- leaflet::renderLeaflet({
    leaflet(summary_df()) %>%
      addTiles() %>%
      setView(lng = 15, lat = 77.5, zoom = 5) %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        layerId = ~station,
        radius = 6,
        popup = ~paste0(
          "<b>Station: </b>", station, "<br>",
          "<b>Fish count: </b>", n_fish, "<br>",
          "<b>Mean length: </b>", round(mean_length, 2), " mm<br>",
          "<b>Mean weight: </b>", round(mean_weight, 2), " g"
        )
      )
  })

  # Establishing mouse-click as input

  observeEvent(input$map_marker_click, {
    selected_station(as.numeric(input$map_marker_click$id))
  })

  # Displaying length and weight histograms

  output$length_plot <- renderPlot({
    req(selected_station())
    plot_length_hist(df(), selected_station())
  })


  output$weight_plot <- renderPlot({
    req(selected_station())
    plot_weight_hist(df(), selected_station())
  })
}



  shinyApp(ui, server)

# This app was made largely with troubleshooting help from ChatGPT 5.3, as well as guides from Biostat and lectures by Richard Telford (2026)
# Further advice and help from fellow students Pia Alina Brakstad Smith and Thorsten Schilling

