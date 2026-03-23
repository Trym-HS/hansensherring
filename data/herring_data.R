library(readxl)

herring_data <- read_excel("inst/raw_data/herring_data.xlsx")

# optional: clean it here or later
# herring_data <- clean_herring_data(herring_data)

usethis::use_data(herring_data, overwrite = TRUE)

