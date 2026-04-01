#' Load herring data from package extdata
#'
#' Reads the Excel file containing 0-group herring data and returns a data frame.
#'
#' @return A data.frame with columns:
#' \describe{
#'   \item{station_nr}{Station identifier}
#'   \item{ind_nr}{Individual fish ID}
#'   \item{lat}{Latitude}
#'   \item{lon}{Longitude}
#'   \item{len_mm}{Length in millimeters}
#'   \item{wgt_g}{Weight in grams}
#' }
#' @importFrom readxl read_excel
#' @export
load_herring_data <- function() {
  file_path <- system.file("extdata", "herring_data.xlsx", package = "hansensherring")
  readxl::read_excel(file_path)
}

