# Load required packages
library(httr)
library(fs)

# Define the base WFS URL
url_base <- "https://maps.skeenasalmon.info/geoserver/ows"

# define output directory
dir_out <- "data/gis/skt/esi_sows"

# create directory with fs
fs::dir_create(dir_out)

# gitignor that directory
usethis::use_git_ignore(dir_out)

# Define the layer to download
layer_name_raw <- "geonode:bcce_watershed_summary_poly_2015_20150331_skeena"
layer_name_raw <- "geonode:UBulkley_wshed"

# layer_name_out <- stringr::str_extract(layer_name_raw, "(?<=:).*")

# turn above into a function
layer_name_prep <-  function(layer_name_raw = NULL){
  stringr::str_extract(layer_name_raw, "(?<=:).*")
}

layer_name_out <- layer_name_prep(layer_name_raw)

# Construct the WFS GetFeature request URL
query_params <- list(
  service = "WFS",
  version = "1.0.0",
  request = "GetFeature",
  typename = layer_name_raw,
  outputFormat = "json",
  srs = "EPSG:3005",
  srsName = "EPSG:3005"
)

# Send request and save response to a GeoJSON file
file_out <- fs::path(dir_out, layer_name_out, ext = "geojson")
response <- httr::GET(url = url_base, query = query_params, httr::write_disk(file_out, overwrite = TRUE))

# Check if download was successful
if (httr::status_code(response) == 200) {
  cat("GeoJSON saved to:", fs::path_abs(file_out), "\n")
} else {
  cat("Error: Failed to download layer. HTTP Status:", httr::status_code(response), "\n")
}
