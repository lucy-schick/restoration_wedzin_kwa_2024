##pull the cv info from the Bulkley File

{
  library(tidyverse)
  library(sf)
  library(tabulizer)
}

path <- "/Users/airvine/zotero/storage/4AAI682C/gaboury_smith_2016_development_of_aquatic_restoration_designs_and_on-farm_cattle_management.pdf"


##define the area to extract table from for first page

#you would run with this the first time
tab_trim_18 <- tabulizer::locate_areas(path, 18)

##since we have done this before though - numbers below are results
# top      left    bottom     right
# 93.38721  77.74522 444.86271 713.59029
tab_trim_18 = list(c(93.38721,  77.74522, 444.86271, 713.59029))



##extract the tables useing the areas you defined
table_18_raw <- tabulizer::extract_tables(path,
                                       pages = seq(18,18),
                                        method = "lattice",
                                        area = tab_trim_18) %>%
  set_names(18) ##should do this as input from "pages" part of the function


# rm(tab_trim_17, tab_trim_18, tab_trim_19)

##look at them as lists of tibbles to see inconsistencies
##we want to be sure that there is the right number of columns

test_view <- table_18_raw %>%
  map(as_tibble)


# ##this is how we make a clean dataframe
table_18_df <- table_18_raw %>%
  pluck(1) %>%
  as_tibble() %>%
  janitor::row_to_names(1) %>%
  janitor::clean_names()


# make spatial, add fencing flag and fill restoration design type
table_18_sf <- table_18_df %>%
  dplyr::filter(utm_zone == "9U") %>%
  sf::st_as_sf(coords = c("easting", "northing"),
                         crs = 26909, remove = F) %>%
  st_transform(crs = 4326) %>%
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>%
  tidyr::separate(site_number, into = c('site_number', 'fenceing_flag'), extra = "merge") %>%
  dplyr::mutate(fenceing_flag = dplyr::case_when(is.na(fenceing_flag) ~ NA,
                                                 TRUE ~ TRUE),
                restoration_design = case_when(
                  restoration_design == "" ~ NA_character_,
                  TRUE ~ restoration_design
                )) %>%
  tidyr::fill(restoration_design)

# we cold read in the gpkg and add these sites but we either need to turn our points into polygons or
# turn our existing polygons to points
sites_rest_raw <- sf::st_read("~/Projects/gis/restoration_wedzin_kwa/sites_restoration.gpkg")

path <- "~/Projects/gis/restoration_wedzin_kwa/sites_restoration.gpkg"

##burn to the geopackage as a point layer for now
sf::st_write(table_18_sf,
             dsn = path,
             layer = "sites_wfn_proposed",
             delete_layer = TRUE,
             quite = TRUE)

sf::st_layers(path)

# accidentally  burned wrong name file in "sites_wfn" so will remove
# rfp::rfp_u_rm_gpkg_layers(path, "sites_wfn")


