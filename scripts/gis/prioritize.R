source('scripts/functions.R')
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
path_sites_hist <- c("/Users/airvine/Projects/gis/restoration_wedzin_kwa/sites_restoration.gpkg")
sites_hist_pt1 <- sf::st_read(
  path_sites_hist,
  layer = "sites_poly"
) |>
# get centre of each polygon as point
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
  # these are out of place b/c they are aligned with the reach-breaks
  "ncfdc_1998_riparian"
)

# read in each layer
sites_hist_pt2 <- purrr::map(
  path_hist_layers,
  ~sf::st_read(dsn = path_sites_hist, layer = .x, quiet = TRUE)
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
# forms_prep <- paths_forms |>
#   purrr::map(
#     ~sf::st_read(dsn = .x, quiet = TRUE)
#   )

# let's have a look at the columns that differ
# cols_diff <- setdiff(names(forms_prep[[2]]), names(forms_prep[[1]]))

sites_2024 <- paths_forms |>
  purrr::map(
    ~sf::st_read(dsn = .x, quiet = TRUE)
  ) |>
  # extract coordinates for each
  purrr::map(
    ~fpr::fpr_sp_assign_utm(dat = .x)
  ) |>
  # drop geom for join - doesn't work if we group the purrr calls so we do 1 by 1. not sure why
  purrr::map(
    ~sf::st_drop_geometry(x = .x)
  ) |>
# types again
purrr::map(
  ~dplyr::mutate(.x, site_id = as.character(site_id))
) |>
  rlang::set_names(paths_forms) |>
  dplyr::bind_rows(.id = "source") |>
  # keep only the new sites for this as many of these are monitoring
  dplyr::filter(
    new_site == 'yes'
  ) |>
  # truncate the source name to keep it easy to lookup
  dplyr::mutate(source = basename(source))


##join all----------------------------------------------------------------------------------------------------
# keep only a minimum amount of columns
cols_keep <- c(
  "source",
  # MWMT
  "site_name_proposed",
  # wetsuweten 2016
  "site_name",
  # Mackay 1998
  "id",
  # forms
  "site_id",
  # site_name,      #already have this column
  "site_name_wfn"
)


sites_all_prep <- dplyr::bind_rows(
  sites_hist,
  sites_2024
) |>
  # make sf object - default is albers 3005 - same as the background_layer.gpkg
  fpr::fpr_sp_assign_sf_from_utm() |>
  # # drop empty columns for now
  # dplyr::select(dplyr::where(~!all(is.na(.x)))) |>
  # dplyr::select(
  #   source, site_id, id, site_name, dplyr::contains("name"), dplyr::everything()
  # ) |>
  dplyr::select(
    dplyr::all_of(
      cols_keep
    )
  )|>
  dplyr::mutate(
    idx = dplyr::row_number()
  ) |>
  # get lat long to use for fwapg queries
  fpr::fpr_sp_assign_latlong()


#house groups-----------------------------------------------------------------------------------------------------
path_wet_house <- "/Users/airvine/Projects/gis/data_secure/wetsuweten_treaty_society/Yinta_HouseGroups.gpkg"

wet_house <- sf::st_read(
  path_wet_house
) |>
  # its already in 3005 but state explicitly anyway
  sf::st_transform(crs = 3005)


# ngr::ngr_dbqs_filter_predicate wont yet work without spatialite extension (see https://github.com/NewGraphEnvironment/ngr/issues/9) so...

# lets get this info in R pure for now
sites_all_prep2 <- sf::st_join(
  sites_all_prep,
  wet_house |>
    dplyr::select(name_wet_house = name, house, chiefs, clan, clan_english),
  left = TRUE
)


#bulkley falls----------------------------------------------------------------------------------------------------
# grab the point from the gpkg layer
falls_bulk_raw <- sf::st_read(
  "/Users/airvine/Projects/gis/restoration_wedzin_kwa/sites_restoration.gpkg",
  layer = "price_2014"
) |>
  dplyr::filter(
    number == 121
  ) |>
  fpr::fpr_sp_assign_latlong()

# tie the point to the bulkley river
falls_bulk_fwa <- fwapgr::fwa_index_point(
  x = falls_bulk_raw$lon,
  y = falls_bulk_raw$lat
)

# extract the pieces of the localcode_ltree
falls_bulk_fwa_parts <- stringr::str_split(falls_bulk_fwa$localcode_ltree, "\\.")[[1]]

# not doing this but we could get the upstream polygon
# falls_bulk_wshd <- fwapgr::fwa_watershed_at_measure(
#   blue_line_key = falls_bulk$blue_line_key,
#   downstream_route_measure = falls_bulk$downstream_route_measure
# )
#
# ggplot2::ggplot() +
#   ggplot2::geom_sf(data = falls_bulk_wshd, lwd = 0.15, fill = "steelblue", alpha = 0.5)

# rather than generate another polygon - just use the freshwater atlas to determine if we are upstream or downstream of the falls
# for each point


## fwa details ---------------------------------------------------------------------------------------------------
# get closest point on each stream - https://smnorris.github.io/fwapg/04_functions.html
sites_all_prep3 <- purrr::map2(
  sites_all_prep2$lon,
  sites_all_prep2$lat,
  ~fwapgr::fwa_index_point(
    x = .x,
    y = .y,
    # we can adjust this to be sure we get a result for everything
    tolerance = 200,
    limit = 1
    , properties = c(
      "gnis_name",
      "distance_to_stream",
      "downstream_route_measure",
      "linear_feature_id",
      "localcode_ltree",
      "wscode_ltree",
      "blue_line_key"
      )
    )
) |>
  dplyr::bind_rows()

# sites_all_prep4 <- dplyr::bind_cols(
#   sites_all_prep2,
#   sites_all_prep3 |>
#     dplyr::mutate(geometry_fwa = geometry)
# ) |>
#   dplyr::mutate(
#     # grabs the third element of the split string
#     bulkley_falls_downstream = (
#       purrr::map_chr(
#         stringr::str_split(localcode_ltree, "\\."),
#         ~ .x[3]
#       ) |>
#         as.integer() > 830486
#     ) |>
#       as.integer()
#   )


# because FWA_Upstream is not exposed on the fwapg webservice yet we will do this manually
sites_all_prep4 <- dplyr::bind_cols(
   sites_all_prep2 |>
     # we no longer need the lat/long so drop
     dplyr::select(-lat, -lon),
   sites_all_prep3 |>
     dplyr::mutate(geometry_fwa = geometry) |>
     sf::st_drop_geometry()
) |>
  dplyr::mutate(
    # split localcode_ltree into parts, check for downstream of falls
    bulkley_falls_downstream = purrr::map_int(
      stringr::str_split(localcode_ltree, "\\."),
      ~ {
        # case 1: less than 3 parts → upstream (0)
        if (length(.x) < 3) {
          0
          # case 2: first two parts don't match falls code → upstream (0)
        } else if (!all(.x[1:2] == falls_bulk_fwa_parts[1:2])) {
          0
          # case 3: first two parts match, compare third part numerically
        } else {
          # falls_bulk_fwa_parts[3] is actually the segment the falls lands on so
          # if a site were to fall on the segment or on a trib that empties into this segment our result
          # would be wrong.  If we want to be accurate we would feed it the segment above which is localcode xxx.xxxxxxxx.830486
          as.integer(.x[3]) > as.integer(falls_bulk_fwa_parts[3]) |>
            as.integer()
        }
      }
    )
  )

# ###################!!!!!  fwa mismatches - need  intervention !!!!!!#########################################
  # strangely there is a bluelinekey that doesn't seem to match with anything in our project or db... need to figure that out
# also - sometimes we have a closer match that is not actually the stream we want to tie too (ex. small intermittent trib next to Richfield)


#floodplain-----------------------------------------------------------------------------------------------------
path_floodplain <- "/Users/airvine/Projects/gis/restoration_wedzin_kwa/habitat_lateral.tif"

floodplain <- terra::rast(path_floodplain)
# see the name of the bands - only 1 called habitat_lateral

sites_all_prep5 <- sites_all_prep4 |>
  dplyr::mutate(
    floodplain_ind = terra::extract(
      floodplain,
      sf::st_coordinates(sites_all_prep4)
    )$habitat_lateral
  )

#Import ranking params-----------------------------------------------------------------------------------------------------
ranking_params <- readr::read_csv("data/inputs_raw/restoration_site_priority_parameters_raw.csv")
path_background_layers <- "~/Projects/gis/restoration_wedzin_kwa/background_layers.gpkg"
path_gis <- "~/Projects/gis/restoration_wedzin_kwa/"


#----------------------------------------polygon queries via csv---------------------------------------------------
## Issues to note----------------------------------------------------------------------------------------------------
# there are gaps in the polygons for things like land ownership.  For example if the site is pinned to the stream
# the area below the high water mark is not included in the polygon.  But the ownership on other side may be private
# making access and works depend on the owner. That is dealt with in next section


# burn our sites to the gpkg
# sites_all_prep5 |>
#   dplyr::select(-geometry, geometry_fwa) |>
#   sf::st_write(
#     dsn = path_background_layers,
#     layer = "sites_all_prep5",
#     delete_layer = TRUE,
#     append = FALSE
#   )

# gdalraster::ogr_layer_delete(dsn = path_background_layers, layer = "sites_all_prep5")
#
# sf::st_layers(path_background_layers)


# test the function
# sites_all_prep6 <- ngr::ngr_spk_join(
#   target_tbl = sites_all_prep5,
#   mask_tbl = "whse_cadastre.pmbc_parcel_fabric_poly_svw",
#   target_col_return = "*",
#   mask_col_return = "owner_type",
#   mask_col_filter = NULL,
#   mask_col_filter_values = NULL,
#   mask_col_filter_values_negate = FALSE,
#   join_fun = sf::st_intersects,
#   path_gpkg = path_background_layers,
#   mask_rm = TRUE
# )


# layer_info <- ngr::ngr_spk_layer_info(path = path_background_layers) |>
#   dplyr::arrange(name)

# get the layer geom type for all gpkgs
# doesn't quite work since the geomtype may not be defined for some layers (dff issue likely)...
layer_info <- fs::path(
  path_gis,

  ranking_params |>
    dplyr::filter(
      !is.na(source_file) &
        is.na(data_secure)) |>
    dplyr::filter(fs::path_ext(source_file) == "gpkg") |>
    dplyr::distinct(source_file) |>
    dplyr::pull(source_file)
) |>
  purrr::map(ngr::ngr_spk_layer_info) |>
  dplyr::bind_rows() |>
  dplyr::arrange(name)

# now that we know this works lets try doing a list of polygons and column names
names_poly_col <- dplyr::left_join(
  ranking_params |>
    dplyr::select(source_file, source_layer),
  layer_info |>
    dplyr::select(name, geomtype),
  by = c("source_layer" = "name")
) |>
  dplyr::filter(stringr::str_detect(geomtype, "POLY|COLLECTION"))

poly_join_names <- names_poly_col |>
  dplyr::pull(source_layer)


# define the gpkg polygon layers we are going to join
# TO DO - use flag in rank spreadsheet to filter
# poly_join_names <- c(
#   "whse_basemapping.fwa_wetlands_poly",
#   "whse_tantalis.ta_park_ecores_pa_svw",
#   "whse_admin_boundaries.clab_indian_reserves",
#   "whse_cadastre.pmbc_parcel_fabric_poly_svw",
#   "whse_forest_tenure.ften_range_poly_carto_vw",
#   # "whse_forest_vegetation.veg_burn_severity_sp",
#   "whse_land_and_natural_resource.prot_historical_fire_polys_sp",
#   # "whse_basemapping.fwa_assessment_watersheds_poly",
#   # "skeena_east"
# )



# now filter the csv input to get those rows
poly_join <- ranking_params |>
  dplyr::filter(
    source_layer %in% poly_join_names
  )


# we convert to next object before the loop to facilitate reproduction
sites_all_prep6 <- sites_all_prep5

for (i in seq_len(nrow(poly_join))) {
  joined <- ngr::ngr_spk_join(
    target_tbl = sites_all_prep6,
    mask_tbl = poly_join$source_layer[i],
    mask_col_return = poly_join$source_column_name[i],
    path_gpkg = fs::path(path_gis, poly_join$source_file[i])
    # path_gpkg = path_background_layers
  )

  # extract new column only by name
  new_col <- poly_join$source_column_name[i]
  sites_all_prep6 <- dplyr::mutate(sites_all_prep6, !!new_col := joined[[new_col]])
}

#Land Ownership non result due to no direct overlap -----------------------------------------------------------------------------------------------------
# for the results that don't have a land ownership type use the nngeo package to find the closeset polgons within 100m

sites_all_prep7_no_ownership <- sites_all_prep6 |>
  dplyr::filter(is.na(owner_type)) |>
  dplyr::select(
    idx
    # dplyr::everything(),
    # -owner_type
    )

sites_all_prep7_no_ownership2 <- ngr::ngr_spk_join(
  target_tbl = sites_all_prep7_no_ownership,
  mask_tbl = "whse_cadastre.pmbc_parcel_fabric_poly_svw",
  mask_col_return = "owner_type",
  path_gpkg = path_background_layers,
  join_fun = nngeo::st_nn,
  k = 2,
  maxdist = 100
) |>
  dplyr::select(idx, owner_type) |>
  dplyr::distinct(idx, owner_type) |>
  dplyr::rename(owner_type1 = owner_type)


# this fails when there are not two different outcomes!!! Skipping this for now!!
sites_all_prep7_no_ownership3 <- sites_all_prep7_no_ownership2 |>
  dplyr::distinct(idx, owner_type1) |>
  dplyr::group_by(idx) |>
  dplyr::mutate(rowid = dplyr::row_number()) |>
  tidyr::pivot_wider(
    names_from = rowid,
    values_from = owner_type1,
    names_prefix = "owner_type"
  ) |>
  dplyr::mutate(
    # for non-results
    owner_type1 = dplyr::case_when(
      is.na(owner_type1) ~ "Not Matched",
      TRUE ~ owner_type1
    )
  ) |>
  # so we want to be conservative - if !is.na for owner_type2 and owner_type1 == Private|Mixed Ownership we want
  # to swap owner_type1 and owner_type2 values
  dplyr::mutate(
    swap = !is.na(owner_type2) & owner_type1 %in% c("Private", "Mixed Ownership"),
    owner_type_tmp = dplyr::if_else(swap, owner_type1, owner_type2),
    owner_type1 = dplyr::if_else(swap, owner_type2, owner_type1),
    owner_type2 = dplyr::if_else(swap, owner_type_tmp, owner_type2)
  ) |>
  dplyr::select(-swap, -owner_type_tmp) |>
  sf::st_drop_geometry()

#now join back to the main data
sites_all_prep8 <- dplyr::left_join(
  sites_all_prep6,
  # sites_all_prep7_no_ownership2,
  #swap this in when we have multiple ownership types
  sites_all_prep7_no_ownership3,
  by = "idx"
) |>
  dplyr::mutate(owner_type = dplyr::case_when(is.na(owner_type) ~ owner_type1, T ~ owner_type)) |>
  dplyr::select(-owner_type1)
  #need to deal here when we have >1 owner type
  # dplyr::mutate(
  #   owner_type = dplyr::coalesce(owner_type, owner_type1)
  # ) |>
  # dplyr::select(-owner_type1) |>
  # dplyr::select(
  #   idx,
  #   source:site_name_proposed,
  #   gnis_name,
  #   name_wet_house:clan_english,
  #   bulkley_falls_downstream:fire_year,
  #   client_name,
  #   dplyr::everything()
  # )

# # burn to data_secure for now - without the riparian polygons
sites_all_prep8 |>
  sf::st_write(
  "/Users/airvine/Projects/gis/data_secure/wetsuweten_treaty_society/sites_prioritized.geojson",
  delete_dsn = TRUE,
  append = FALSE
)

# #--------------------------------------------------draft scorer functions---------------------------------------------------
# # example of running the function on 1 column
# out1 <- priority_scorer_numeric(
#   dat_values = sites_all_prep8,
#   dat_ranks = ranking_params,
#   col_rank = "bulkley_falls_downstream"
# )
#
# cols_rank <- c(
#   "bulkley_falls_downstream",
#   "floodplain_ind"
# )
#
# # try passing a list of col_rank
# out2 <- purrr::map(
#   cols_rank,
#   ~ priority_scorer_numeric(
#     dat_values = sites_all_prep8,
#     dat_ranks = ranking_params,
#     col_rank = .x
#   )
# )
#
# out3 <- purrr::reduce(out2, dplyr::inner_join, by = "idx")
#
# out4 <- out3 |>
#   dplyr::mutate(
#     total_score = rowSums(dplyr::across(dplyr::matches("score")), na.rm = TRUE)
#   )
#
#
# out1 <- priority_scorer_string(
#   dat_values = sites_all_prep8,
#   dat_ranks = ranking_params,
#   col_rank = "owner_type",
#   col_filter = "source_column_name"
# )


# bcfishpass spawn/rear columns from the stream segment joined -------------------------------------------------------
streams <- sf::st_read(
  path_background_layers,
  layer = "bcfishpass.streams_vw"
)

cols_stream <- ranking_params |>
  # get ranking columns from that target layer
  dplyr::filter(source_layer == "bcfishpass.streams_vw" &
                  rank == TRUE) |>
  dplyr::pull(source_column_name)

# run a simple join on the linear_feature_id (generated for each site in the "fwa details" section above
sites_all_prep9 <- dplyr::left_join(
  sites_all_prep8,
  streams |>
    dplyr::select(
      linear_feature_id,
      dplyr::all_of(
        cols_stream
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(cols_stream),
        ~ as.integer(.x)
      )
    )|>
    dplyr::distinct(linear_feature_id, .keep_all = TRUE) |>
    sf::st_drop_geometry(),
  by = "linear_feature_id",
  na_matches = "never"
)

# score ranking -----------------------------------------------------------------------------------------------------
# try passing a list of col_rank

cols_rank <- ranking_params |>
  dplyr::filter(rank == TRUE) |>
  dplyr::pull(source_column_name)

# cols_rank <- c(
#   "bulkley_falls_downstream",
#   "floodplain_ind"
# )
#
#
# cols_rank2 <- c(
#   cols_stream
#   , cols_rank
#   # string type
#   , "owner_type"
#   )

out2 <- purrr::map(
  cols_rank,
  ~ priority_scorer(
    dat_values = sites_all_prep9,
    dat_ranks = ranking_params,
    col_rank = .x
    # col_idx = "idx_test"
    # need to watch that this is the same as the column name in the dat_ranks
    # col_filter = "source_column_name"
  )
)

# we have to watch out here b/c if there is no result then it fails
out3 <- purrr::reduce(out2, dplyr::inner_join, by = "idx")

out4 <- out3 |>
  dplyr::mutate(
    total_score = rowSums(dplyr::across(dplyr::matches("score")), na.rm = TRUE)
  )

# join back to our dataframe
sites_all_ranked <- dplyr::left_join(
  sites_all_prep9,
  out4,
  by = "idx"
) |>
  dplyr::select(total_score, dplyr::everything())|>
  # add the lat longs to keep it simple
  fpr::fpr_sp_assign_latlong()


sites_all_ranked |>
  # stamp a version
  dplyr::mutate(version = format(Sys.Date(), "%Y%m%d")) |>
  sf::st_write(
    fs::path(path_gis, "sites_prioritized.geojson"),
    delete_dsn = TRUE,
    append = FALSE
  )



# bcfishpass spawn/rear watershed summaries -----------------------------------------------------------------------------------------------------
# see the columns in the db version - they are actually different than the ones from featureserve where our q layer comes from. weird
fpr::fpr_db_query(
  fpr::fpr_dbq_lscols(schema = 'whse_basemapping', table = 'fwa_assessment_watersheds_poly')
)

fpr::fpr_db_query(
  fpr::fpr_dbq_lscols(schema = 'bcfishpass', table = 'log_aw_linear_summary'),
  db_var = "bcfishpass_dev",
)

# here are the metrics we want
cols_linear_summary <- c(
  "assessment_watershed_id",
  "length_spawningrearing_obsrvd_co",
  "length_spawningrearing_model_co",
  "length_spawningrearing_obsrvd_ch",
  "length_spawningrearing_model_ch"
)

cols_sql <- glue::glue_sql_collapse(
  x = glue::glue_sql(
    "l.{`col`}", col = cols_linear_summary, .con = DBI::ANSI()
  ),
  sep = ", "
)

sql_query <- glue::glue_sql(
  "SELECT {cols_sql}
   FROM bcfishpass.log_aw_linear_summary l
   JOIN whse_basemapping.fwa_assessment_watersheds_poly w
     ON l.assessment_watershed_id = w.watershed_feature_id
   WHERE w.watershed_group_code = 'BULK';",
  .con = DBI::ANSI()
)

fpr::fpr_db_query(sql_query, db_var = "bcfishpass_dev")

# separate by sub-basin
# lets build a custom watersehed just for upstream of the confluence of Neexdzii Kwa and Wetzin Kwa
# blueline key
blk <- 360873822
# downstream route measure
drm <- 166030.4

aoi <- fwapgr::fwa_watershed_at_measure(blue_line_key = blk,
                                        downstream_route_measure = drm) |>
  sf::st_transform(crs = 3005)


# read in the watersheds clipped by the neexdzi kwa aoi.
wshds_raw <- sf::st_intersection(
    sf::st_read(path_background_layers,
                layer = "whse_basemapping.fwa_named_watersheds_poly"),
    aoi |>
      sf::st_geometry()
  ) |>
  dplyr::filter(
    # keep only polygons
    sf::st_geometry_type(geom) == "POLYGON",
    # if we wanted to use whse_basemapping.fwa_named_watersheds_poly we would do this - as only 3 parts to the code
    stringr::str_count(wscode_ltree, "\\.") == 2,
    sf::st_is_valid(geom)
  ) |>
  dplyr::mutate(area_ha2 = as.numeric(sf::st_area(geom)/10000)) |>
  # we are hacking around here b/c our
  dplyr::filter(abs(area_ha - area_ha2) <= 0.01)  # 100 m² = 0.01 ha


wshds_ass |>
  sf::st_write(
    "/Users/airvine/Projects/gis/restoration_wedzin_kwa/test.geojson"
  )


# link sites to "critical habitat" layer built by DFO


# score based on !is.na for weight_value_{rank} columns
#Weight by group (social, biological, cultural-----------------------------------------------------------------------------------------------------
