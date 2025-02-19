##pull the wq site locations from the report

{
  library(tidyverse)
  library(sf)
  library(tabulapdf)
}

path <- "/Users/airvine/zotero/storage/QTD5WGZ4/oliver_2020_analysis_of_2017_water_quality_monitoring_-_upper_bulkley_river_watershed.pdf"



#you would run with this the first time
tab_trim_17 <- tabulapdf::locate_areas(path, 17)

##since we have done this before though - numbers below are results

# top      left    bottom     right
# 96.13363  69.19376 397.76392 554.27171
tab_trim_17 = list(c(96.13363,  69.19376, 397.76392, 554.27171 ))



##extract the tables useing the areas you defined
table_17_raw <- tabulapdf::extract_tables(path,
                                          pages = seq(17,17),
                                          method = "lattice",
                                          area = tab_trim_17,
                                          guess = FALSE) |>
  set_names(17) ##should do this as input from "pages" part of the function

table_17 <- table_17_raw$"17"

# this could be wrangled but we used chat gpt since it got it right first try.  We saved it here:

library("parzer")

path <- "data/inputs_raw/oliver2020Analysis2017_site_locations_raw.csv"

# flag which site had periphyton sampling
sites_p <- c("1WP","3W", "5P", "7WP", "9P", "11WP", "12WP", "13WP", "14WP")

# flag sediment sites
sites_s <- c()

# read in and
wq_sites_raw <- readr::read_csv(path) |>
  dplyr::mutate(
    `Longitude (W)` = paste0("-", `Longitude (W)`),
    `Latitude (N)` = parzer::parse_lat(`Latitude (N)`),
    `Longitude (W)` = parzer::parse_lon(`Longitude (W)`),
    `Latitude (N)` = dplyr::case_when(
      `Site ID` =="Unnamed Spring" ~ 54.5078,
      TRUE ~ `Latitude (N)`
    ),
    `Longitude (W)` = dplyr::case_when(
      `Site ID` == "Unnamed Spring" ~ -126.3407,
      TRUE ~ `Longitude (W)`
    ),
    `Site Description` = dplyr::case_when(
      `Site ID` == "Unnamed Spring" ~ paste0(
        `Site Description`,
        " (Coordinates are a rough estimate included to allow spatial representation of site; should be revised based on actual field data.)"
      ),
      TRUE ~ `Site Description`
    )
  ) |>
  # highlight which site had periphyton sampling

wq_sites <- wq_sites_raw |>
  janitor::clean_names() |>
  # turn to spatial object
  sf::st_as_sf(coords = c("longitude_w", "latitude_n"), crs = 4326)

# burn to a geojson so we can even see it on github
wq_sites |>
  sf::st_write(
    "data/inputs_extracted/oliver2020Analysis2017_site_locations.geojson",
    delete_dsn = TRUE
  )

wq_sites |>
  sf::st_write(
    "~/Projects/gis/restoration_wedzin_kwa/oliver2020Analysis2017_site_locations.geojson",
    delete_dsn = TRUE
  )

# see what data is availale in ems with rems



