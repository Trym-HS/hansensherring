
library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(readxl)
library(usethis)

# ---------------------------
# DATA FUNCTIONS
# ---------------------------

read_herring_data <- function(path) {
  read_excel(path)
}
# Cleaning data for all columns in herring_data.xlsx

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

summarize_stations <- function(df) {
  df %>%
    group_by(station, lat, lon) %>%
    summarise(
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
      leafletOutput("map", height = 600)
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

server <- function(input, output, session) {

  # Loading data from original Excel-file

  df <- reactive({
    read_herring_data("herring_data.xlsx") %>%
      clean_herring_data()
  })

  summary_df <- reactive({
    summarize_stations(df())
  })

  # Selected station as an interactive object

  selected_station <- reactiveVal(NULL)

  # Establishing Leaflet mapping with location markers

  output$map <- renderLeaflet({
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

# Running app

shinyApp(ui, server)

