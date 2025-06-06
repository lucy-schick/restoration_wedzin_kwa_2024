# wrangle the Nadina prescriptions into a spatial dataset

# make a subset of the stream/blk xref used in fwatlasbc::fwa_add_blks_to_stream_name
xref_str_blk <- fwatlasbc::fwa_stream_name |>
  dplyr::filter(stringr::str_starts(as.character(blk), "360"))

#reach breaks-----------------------------------------------------------------------------------------------------
path_reach <- 'data/inputs_raw/ncfdc_1998/reach_breaks.csv'
reaches_raw <- readr::read_csv(path_reach)

reaches <- reaches_raw |>
  # need to ditch those we don't have coords for
  dplyr::filter(!is.na(easting_fhap)) |>
  sf::st_as_sf(coords = c("easting_fhap", "northing_fhap"), crs = 32609, remove = FALSE) |>
  # create stream_name column so we can use fwatlasbc::fwa_add_blks_to_stream_name to get blueline key
  dplyr::mutate(
    stream_name = stringr::str_remove(reach_name_corrected, "\\s\\d+$") |>
      paste("Creek") |>
      stringr::str_replace("^Bulkley Creek$", "Bulkley River")
  ) |>
  # add the bluelinekey
  fwatlasbc::fwa_add_blks_to_stream_name(stream_name = xref_str_blk) |>
  sf::st_transform(crs = 4326)



# get the downtreawm_route_measure for the breaks
# here we grab the point on a stream corresponding to the river metre
reaches_pt <- purrr::map2(
  reaches$blk,
  reaches$reach_drm,
  ~fwapgr::fwa_index_point(
    tolerance = 1000,
    blue_line_key = .x,
    downstream_route_measure = .y
  )
) |>
  dplyr::bind_rows()

d <- dplyr::bind_cols(
  d_raw,
  d_pt
) |>
  sf::st_as_sf()


# burn to project for viewing
reaches |>
  sf::st_write("/Users/airvine/Projects/gis/restoration_wedzin_kwa/ncfdc_1998_reach_breaks.geojson", delete_dsn = TRUE)

path <- 'data/inputs_raw/ncfdc_1998/AppF_riparian_prescriptions.xls'


library(readxl)
library(dplyr)

# list all the sheet in the file (path)
d_sheets <- readxl::excel_sheets(path)

d_types <- readxl::read_excel(path) |>
  janitor::clean_names()

# get the boudnign box of the Neexdzi Kwah - we burn it local so we don't repeat
# blueline key
# blk <- 360873822
# # downstream route measure
# drm <- 166030.4
# aoi <- fwapgr::fwa_watershed_at_measure(blue_line_key = blk,
#                                             downstream_route_measure = drm) |>
#   # we put it in wsg84
#   sf::st_transform(4326)
#   # dplyr::select(geometry)
#
# # burn it to file
# sf::st_write(aoi, "data/gis/aoi.geojson")

aoi <- sf::st_read("data/gis/aoi.geojson")

#get the bounding box of our aoi
aoi_bb <- sf::st_bbox(aoi)




# Function to conditionally rename columns
col_rename <- function(df) {
  if ("distance_u_s" %in% names(df)) {
    df <- dplyr::rename(df, distance_u_s_km_m = distance_u_s)
  }
  df
}

# read in all the sheets
d_raw <- purrr::map(d_sheets, ~readxl::read_excel(path, sheet = .x)) |>
  purrr::map(janitor::clean_names) |>
  purrr::set_names(d_sheets) |>
  # make all columns character
  purrr::map(~dplyr::mutate_all(., as.character)) |>
  # sometimes distance_u_s_km_m and sometimes distance_u_s
  purrr::map(col_rename) |>
  dplyr::bind_rows(.id = "sheet_name") |>
  # we have a funky value in the distance column of the `Buck 11` sheet
  dplyr::mutate(distance_u_s_km_m = dplyr::case_when(riparian_poly_id == "UB8/Impact Site 1" ~ "72577",
                                                     TRUE ~ distance_u_s_km_m)) |>
  dplyr::mutate(stream_name = dplyr::case_when(stringr::str_detect(sheet_name, "Bulkley") ~ stringr::str_replace(sheet_name, "\\s\\d+$", " River"),
                                                 T ~ stringr::str_replace(sheet_name, "\\s\\d+$", " Creek")),
                rm = as.numeric(stringr::str_remove(distance_u_s_km_m, "\\+"))
  ) |>
  fwatlasbc::fwa_add_blks_to_stream_name(stream_name = xref_str_blk)
  # we havea funky value




# here we grab the point on a stream corresponding to the river metre
d_pt <- purrr::map2(
  d_raw$blk,
  d_raw$rm,
  ~fwapgr::fwa_locate_along(
    blue_line_key = .x,
    downstream_route_measure = .y
  )
) |>
  dplyr::bind_rows()

d <- dplyr::bind_cols(
  d_raw,
  d_pt
) |>
  sf::st_as_sf()


# burn to file
d |>
  sf::st_write(
    "~/Projects/gis/restoration_wedzin_kwa/sites_restoration.gpkg", layer = "ncfdc_1998_riparian", delete_layer = TRUE
    )

d |>
  sf::st_write("data/gis/ncfdc_1998_riparian.geojson", delete_dsn = TRUE)
