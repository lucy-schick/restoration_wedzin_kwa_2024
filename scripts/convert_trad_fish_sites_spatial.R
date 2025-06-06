# Convert the traditional fishing sites documented Skeena Fish Populations and their Habitat, by Gottesfeld and Rabnett 2007,
# which are stored in "data/trad_fish_sites_wilson_rabnett2007FishPassage.csv" to a spatial object for visualization in QGIS

path_repo_trad_fish_sites <- "data/trad_fish_sites_gottesfeld_rabnett2007FishPassage.csv"

path_gis_trad_fish_sites <- fs::path(path.expand("~/Projects/gis/restoration_wedzin_kwa/trad_fish_sites_gottesfeld_rabnett2007FishPassage.gpkg"))

trad_fish_sites <- readr::read_csv(path_repo_trad_fish_sites) |>
  dplyr::filter(!is.na(utm_zone)) |>
  fpr::fpr_sp_assign_sf_from_utm() |>
  sf::st_write(dsn = path_gis_trad_fish_sites,
               layer = "trad_fish_sites",
               delete_layer = TRUE,
               quite = TRUE)
