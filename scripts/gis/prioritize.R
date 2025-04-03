# aoi <- sf::st_read()
#
# # grab all the railways
# l_rail <- rfp::rfp_bcd_get_data(
#   bcdata_record_id = "whse_basemapping.gba_railway_tracks_sp"
# ) |>
#   sf::st_transform(4326) |>
#   janitor::clean_names()
#
#
# # streams in the bulkley and then filter to just keep the big ones
# l_streams <- rfp::rfp_bcd_get_data(
#   bcdata_record_id = "whse_basemapping.fwa_stream_networks_sp",
#   col_filter = "watershed_group_code",
#   col_filter_value = "BULK",
#   # grab a smaller object by including less columns
#   col_extract = c("linear_feature_id", "stream_order", "gnis_name", "downstream_route_measure", "blue_line_key", "length_metre")
# ) |>
#   sf::st_transform(4326) |>
#   janitor::clean_names() |>
#   dplyr::filter(stream_order >= 4)
#
# # clip them  with purrr and sf
# layers_trimmed <- purrr::map(
#   layers_all,
#   ~ sf::st_intersection(.x, aoi)
# )

#amalgamate sites-----------------------------------------------------------------------------------------------------
# make an sf abject with all the sites in it
path_hist <- c("/Users/airvine/Projects/gis/restoration_wedzin_kwa/sites_restoration.gpkg")
sites_hist_pt1 <- sf::st_read(
  path_hist,
  layer = "sites_poly"
) |>
# get centre of each as point
  sf::st_centroid() |>
  fpr::fpr_sp_assign_utm() |>
  sf::st_drop_geometry() |>
  # convert id to character for join
  dplyr::mutate(
    id = as.character(id),
  # give source column for tracking manually
  source = 'sites_poly'
  )

path_hist_layers <- c(
  "sites_wfn_proposed",
  "ncfdc_1998_prescriptions",
  "ncfdc_1998_riparian"
)

# read in each layer
sites_hist_pt2 <- purrr::map(
  path_hist_layers,
  ~sf::st_read(dsn = path_hist, layer = .x, quiet = TRUE)
) |>
  # extract coordinates for each
  purrr::map(
    ~fpr::fpr_sp_assign_utm(dat = .x)
  ) |>
  # drop geom for join
  purrr::map(
    ~sf::st_drop_geometry(x = .x)
  ) |>
  rlang::set_names(path_hist_layers) |>
  dplyr::bind_rows(.id = "source")


sites_hist <- dplyr::bind_rows(
    sites_hist_pt1,
    sites_hist_pt2
  ) |>
  dplyr::select(
    source, dplyr::everything()
  )

##2024----------------------------------------------------------------------------------------------------
paths_forms <- c(
  "/Users/airvine/Projects/gis/restoration_wedzin_kwa/data_field/2024/form_monitoring_ree_2024.gpkg",
  "/Users/airvine/Projects/gis/restoration_wedzin_kwa/data_field/2024/form_monitoring_ree_20240923.gpkg"
  )

sites_2024 <- paths_forms |>
  purrr::map(
    ~sf::st_read(dsn = .x, quiet = TRUE)
  ) |>
  # extract coordinates for each
  purrr::map(
    ~fpr::fpr_sp_assign_utm(dat = .x)
  ) |>
  # drop geom for join
  purrr::map(
    ~sf::st_drop_geometry(x = .x)
  ) |>
  # types again
  purrr::map(
    ~dplyr::mutate(.x, site_id = as.character(site_id))
  ) |>
  rlang::set_names(paths_forms) |>
  dplyr::bind_rows(.id = "source")

##join all----------------------------------------------------------------------------------------------------
sites_all_prep <- dplyr::bind_rows(
  sites_hist,
  sites_2024
) |>
  # make sf object - default is albers 3005 - same as the background_layer.gpkg
  fpr::fpr_sp_assign_sf_from_utm() |>
  # drop empty columns for now
  dplyr::select(dplyr::where(~!all(is.na(.x)))) |>
  dplyr::select(
    source, site_id, id, site_name, dplyr::contains("name"), dplyr::everything()
  )


#house groups-----------------------------------------------------------------------------------------------------
path_wet_house <- "/Users/airvine/Projects/gis/data_secure/wetsuweten_treaty_society/Yinta_HouseGroups.gpkg"

wet_house <- sf::st_read(
  path_wet_house
) |>
  # its already in 3005 but state explicitly anyway
  sf::st_transform(crs = 3005)


# since ngr::ngr_dbqs_filter_predicate should work for most our layers (or hopefully all) we will
# burn to the project gpkg

# lets get this info in R pure for now
sites_all_prep2 <- sf::st_join(
  sites_all_prep,
  wet_house |>
    dplyr::select(name_wet_house = name, house, chiefs, clan, clan_english),
  left = TRUE
)


#bulkley falls----------------------------------------------------------------------------------------------------
# although we may move to freshwater atlas - for now we make a polygon for upstream of the falls for a simple join
# grab the point from the gpkg layer
falls_bulk_raw <- sf::st_read(
  "/Users/airvine/Projects/gis/restoration_wedzin_kwa/sites_restoration.gpkg",
  layer = "price_2014"
) |>
  dplyr::filter(
    number == 121
  ) |>
  fpr::fpr_sp_assign_latlong()

falls_bulk_fwa <- fwapgr::fwa_index_point(
  x = falls_bulk_raw$lon,
  y = falls_bulk_raw$lat
)

# get the upstream polygon
# falls_bulk_wshd <- fwapgr::fwa_watershed_at_measure(
#   blue_line_key = falls_bulk$blue_line_key,
#   downstream_route_measure = falls_bulk$downstream_route_measure
# )
#
# ggplot2::ggplot() +
#   ggplot2::geom_sf(data = falls_bulk_wshd, lwd = 0.15, fill = "steelblue", alpha = 0.5)

# rather than generate another polygon - just use the freshwater atlas to determine if we are upstream of the falls


# fwa details for each site----------------------------------------------------------------------------------------------------
sites_all_prep3 <- sites_all_prep2 |>
  fpr::fpr_sp_assign_latlong()

sites_all_prep4 <- purrr::map2(
  sites_all_prep3$lon,
  sites_all_prep3$lat,
  ~fwapgr::fwa_index_point(x = .x, y = .y)
) |>
  dplyr::bind_rows()

sites_all_prep5 <- dplyr::bind_cols(
  sites_all_prep3,
  sites_all_prep4 |>
    dplyr::mutate(geometry_fwa = geometry)
)  |> dplyr::mutate(
  falls_upstream = purrr::map_chr(
    stringr::str_split(localcode_ltree, "\\."),
    ~ .x[3]
    # 830486 is from falls_bulkley
  ) |> as.integer() > 830486
)
  # ###################!!!!!   figure this out !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#########################################
  # strangly there is a bluelinekey that doesn't seem to match with anything in our project or db... need to figure that out
  # dplyr::filter(is.na(falls_upstream))


#floodplain-----------------------------------------------------------------------------------------------------
path_floodplain <- "/Users/airvine/Projects/gis/restoration_wedzin_kwa/habitat_lateral.tif"

floodplain <- terra::rast(path_floodplain)
# see the name of the bands - only 1 called habitat_lateral

sites_all_prep6 <- sites_all_prep5 |>
  dplyr::mutate(
    floodplain_ind = terra::extract(
      floodplain,
      sf::st_coordinates(sites_all_prep5)
    )$habitat_lateral
  )

################################################################################################################
#----------------------------------------automate queries via csv---------------------------------------------------
################################################################################################################

csv_prior <- readr::read_csv("data/inputs_raw/restoration_site_priority_parameters_raw.csv")

path_background_layers <- "/Users/airvine/Projects/gis/restoration_wedzin_kwa/background_layers.gpkg"


# burn our sites to the gpkg
# sites_all_prep6 |>
#   dplyr::select(-geometry, geometry_fwa) |>
#   sf::st_write(
#     dsn = path_background_layers,
#     layer = "sites_all_prep6",
#     delete_layer = TRUE,
#     append = FALSE
#   )

# gdalraster::ogr_layer_delete(dsn = path_background_layers, layer = "sites_all_prep6")
#
# sf::st_layers(path_background_layers)


# test the function
sites_all_prep7 <- ngr::ngr_spk_join(
  target_tbl = sites_all_prep6,
  mask_tbl = "whse_cadastre.pmbc_parcel_fabric_poly_svw",
  target_col_return = "*",
  mask_col_return = "owner_type",
  mask_col_filter = NULL,
  mask_col_filter_values = NULL,
  mask_col_filter_values_negate = FALSE,
  join_fun = sf::st_intersects,
  path_gpkg = path_background_layers,
  mask_rm = TRUE
)

# layer_info_raw <- as.data.frame(sf::st_layers(path_background_layers))
#   # dplyr::select(-crs) |>
#   # dplyr::mutate(geomtype = as.character(geomtype))
#
# layer_info <- tibble::tibble(
#   name = layer_info_raw$name,
#   geomtype = as.character(layer_info_raw$geomtype)
# )
#
#
# # now that we know this works lets try doing a list of polygons and column names
# names_poly_col <- dplyr::left_join(
#   csv_prior,
#   layer_info,
#   by = c("source_schema_table" = "name")
# )
# well that didn't work since the geomtype is not defined for some layers (dff issue likely)

# define the gpkg polygon layers we are going to join
poly_join_names <- c(
  "whse_basemapping.fwa_wetlands_poly",
  "whse_tantalis.ta_park_ecores_pa_svw",
  "whse_admin_boundaries.clab_indian_reserves",
  "whse_cadastre.pmbc_parcel_fabric_poly_svw",
  "whse_forest_tenure.ften_range_poly_carto_vw"
)

# now filter the csv input to get those rows
poly_join <- csv_prior |>
  dplyr::filter(
    source_schema_table %in% poly_join_names
  )

# we turn it to 7 before the loop
sites_all_prep7 <- sites_all_prep6

for (i in seq_len(nrow(poly_join))) {
  joined <- ngr_spk_join(
    target_tbl = sites_all_prep7,
    mask_tbl = poly_join$source_schema_table[i],
    mask_col_return = poly_join$source_column_name[i],
    path_gpkg = path_background_layers
  )

  # extract new column only by name
  new_col <- poly_join$source_column_name[i]
  sites_all_prep7 <- dplyr::mutate(sites_all_prep7, !!new_col := joined[[new_col]])
}

# rearrange the cols for presenting
sites_all_prep8 <- sites_all_prep7 |>
  dplyr::select(
    source:site_name_proposed,
    gnis_name,
    name_wet_house:clan_english,
    falls_upstream:owner_type,
    client_name = array_to_string,
    dplyr::everything()
  )

# burn to data_secure for now
sites_all_prep8 |>
  sf::st_write(
  "/Users/airvine/Projects/gis/data_secure/wetsuweten_treaty_society/sites_prioritized.geojson"
)





