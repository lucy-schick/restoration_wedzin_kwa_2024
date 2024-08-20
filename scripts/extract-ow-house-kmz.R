library(sf)
library(tidyverse)
library(xml2)
library(rvest)

# Set the path to your KMZ file
kmz_file <- "~/Library/CloudStorage/OneDrive-Personal/Projects/2024-069-ow-wedzin-kwa-restoration/data/wetsuweten_treaty_society/Yinta_HouseGroups.kmz"

# Define the output directory where you want to unzip the files
output_dir <- "~/Library/CloudStorage/OneDrive-Personal/Projects/2024-069-ow-wedzin-kwa-restoration/data/wetsuweten_treaty_society/"

# Unzip the KMZ file
unzip(kmz_file, exdir = output_dir)

# Set the path to the KML file (inside the unzipped directory)
kml_file <- file.path(output_dir, "doc.kml")

# Read the KML file as an sf object
kml_data <- sf::st_read(kml_file)

ggplot2::ggplot(data = kml_data) +
  geom_sf(fill = "lightblue", color = "black")

kml_poly_raw <- st_collection_extract(kml_data, "POLYGON")

# Define the fields to extract
fields <- c("TTY_CODE", "CHIEFS1", "CLAN_WET1", "CLAN_CODE")

# Function to extract the desired value based on the field
extract_value <- function(description, field) {
  pattern <- paste0(field, ":")  # Add the colon dynamically
  html_content <- read_html(description)
  html_content %>%
    html_nodes(xpath = paste0("//td[contains(., '", pattern, "')]/following-sibling::td")) %>%
    html_text(trim = TRUE)
}

# Apply the extraction for each field in the list
kml_poly <- kml_poly_raw |>
  mutate(
    !!!set_names(fields, fields) |>
      map(~ map_chr(kml_poly_raw$Description, extract_value, field = .x))
  ) |>
  # split the Name column to name, chiefs, clan, clan_english based on text after : symbol
  tidyr::separate(Name,
                  into = c("name", "house", "chiefs", "clan", "clan_english"),
                  sep = " House: | Chiefs: | Clan: | Clan English: ",
                  extra = "merge"
  ) |>
  mutate(name = str_remove(name, "^Name: ")) |>
  janitor::clean_names()


# Save to a new KML file or another format (e.g., GeoJSON)
kml_poly |>
  sf::st_transform(crs = 3005) |>
  # put in location outside of the shared project for now
  sf::st_write("~/Library/CloudStorage/OneDrive-Personal/Projects/2024-069-ow-wedzin-kwa-restoration/data/wetsuweten_treaty_society/Yinta_HouseGroups.gpkg",
               delete_layer = TRUE)


# define the new secure directory
dir_secure <- "~/Projects/gis/data_secure/wetsuweten_treaty_society"
# if the ~Projects/gis/data_secure/wetsuweten_treaty_society directory does not exist create with fs
fs::dir_create(dir_secure)

# copy over the file to the new directory
fs::file_copy("~/Library/CloudStorage/OneDrive-Personal/Projects/2024-069-ow-wedzin-kwa-restoration/data/wetsuweten_treaty_society/Yinta_HouseGroups.gpkg",
              dir_secure,
              overwrite = TRUE)
