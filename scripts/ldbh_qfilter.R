ldbh_qtable_quote <- function(table_name, quote_type = "double") {
  if (quote_type == "single") {
    table_name_quoted <- glue::glue("'{table_name}'")
  }

  if (quote_type == "double") {
    table_name_quoted <- glue::glue("\"{table_name}\"")
  }

  if (!quote_type %in% c("double", "single")){
    cli::cli_abort("Invalid quote_type specified. Use 'single' or 'double'.")
  }
  table_name_quoted
}


ldbh_qfilter <- function(
    target_tbl,
    mask_tbl,
    target_col_return = '*',
    mask_col_return = NULL,
    mask_col_filter = NULL,
    mask_col_filter_values = NULL,
    mask_col_filter_values_negate = FALSE,
    spatial_function = 'ST_Intersection',
    quote_tbl = FALSE,
    ...
) {
  # Validate spatial function choice - perhaps we should seperate Geometric Operations (ST_Intersection - create new
  # geometry under certain conditions) from Geometric Binary Predicates (TRUE/FALSE based on whether the specified
  # spatial relationship holds) - https://r-spatial.github.io/sf/reference/index.html
  if (!spatial_function %in% c("ST_Intersection", "ST_Intersects", "ST_Contains", "ST_Within", "ST_Overlaps", "ST_Crosses", "ST_Touches")) {
    stop("Invalid spatial function specified.")
  }

  # Wrap scalar inputs in vectors
  if (!is.null(target_col_return) && !is.vector(target_col_return)) {
    target_col_return <- c(target_col_return)
  }
  if (!is.null(mask_col_return) && !is.vector(mask_col_return)) {
    mask_col_return <- c(mask_col_return)
  }
  if (!is.null(mask_col_filter_values) && !is.vector(mask_col_filter_values)) {
    mask_col_filter_values <- c(mask_col_filter_values)
  }

  chk::chk_not_null(target_tbl)
  chk::chk_not_null(mask_tbl)
  chk::chk_string(target_tbl)
  chk::chk_string(mask_tbl)
  chk::chk_not_null(target_col_return)
  chk::chk_vector(target_col_return)
  if (!is.null(mask_col_return)) {
    chk::chk_vector(mask_col_return)
  }
  if (!is.null(mask_col_filter)) {
    chk::chk_vector(mask_col_filter)
  }
  if (!is.null(mask_col_filter_values)) {
    chk::chk_vector(mask_col_filter_values)
  }

  # Check if '*' is an element of mask_col_return, if mask_col_return is not NULL
  if (!is.null(mask_col_return) && '*' %in% mask_col_return) {
    cli::cli_abort(message = "Using '*' for mask_col_return is not allowed. Please specify mask_col_return explicitly or
                   leave it NULL if no additional columns are needed from the mask layer.")
  }

  if ("geom" %in% target_col_return) {
    cli::cli_abort("explicitly requesting 'geom' in target_col_return is not necessary as it will be returned.
                    Pleae revise target_col_return input")
  }


  if (identical(target_col_return, '*') && spatial_function != "ST_Intersection") {
    cli::cli_alert_danger("Using 'target_col_return = \"*\"' will result in an extra geom column named 'geom..{{column_index}}'
                          in the returned object. To avoid this, consider specifying columns explicitly and not
                          including the 'geom' column of the target table.")
  }
  if (identical(target_col_return, '*') && spatial_function == "ST_Intersection") {
    cli::cli_alert_danger("Using 'target_col_return = \"*\"' will include all columns from the target table, including
                          the geom column. Because 'ST_Intersection' is
                          specified as spatial_function, an extra geometry column named 'geom_intersection' will be
                          included in the output. Further processing will then be required to reassign geom_intersection
                          as the active geometry column.  To avoid this, consider specifying columns explicitly and not
                          including the 'geom' column of the target table.")
  }

  # Check if 'target_col_return' is a single value equal to "*" and capture that
  is_star <- identical(target_col_return, "*")

  # Use 'is_star' to decide whether to construct the columns string or use 'target.*'
  cols_target_str <- if (!is_star) {
    # Construct the columns string only if specific columns are requested
    target_cols <- glue::glue("target.{target_col_return}")
    glue::glue_collapse(target_cols, sep = ", ")
  } else {
    "target.*"
  }

  # Check if 'mask_col_return' is NULL or has values and construct 'cols_mask_str' accordingly
  cols_mask_str <- if (!is.null(mask_col_return) && length(mask_col_return) > 0) {

    # Construct the mask columns string
    mask_cols <- glue::glue("mask.{mask_col_return}")
    mask_cols_glued <- glue::glue_collapse(mask_cols, sep = ", ")
    glue::glue(", {mask_cols_glued}")  # Adding a leading comma for SQL syntax
  } else {
    ""  # If 'mask_col_return' is NULL or empty, no mask columns are added
  }

  # Adjust the filtering condition for the mask layer based on mask_col_filter_values_negate
  mask_filter_condition <- if (!is.null(mask_col_filter) && !is.null(mask_col_filter_values)) {
    filter_values_str <- glue::glue_collapse(glue::glue("'{mask_col_filter_values}'"), sep = ", ")
    if (mask_col_filter_values_negate) {
      glue::glue(" AND mask.{mask_col_filter} NOT IN ({filter_values_str})")
    } else {
      glue::glue(" AND mask.{mask_col_filter} IN ({filter_values_str})")
    }
  } else {
    ""
  }

    if (spatial_function == "ST_Intersection") {
    # Use spatial_function for modifying geometries in the SELECT clause
    spatial_operation_select <- "ST_Intersection(target.geom, mask.geom) AS geom_intersection"
  } else {
    spatial_operation_select <- "target.geom"
  }

  # Use ST_Intersects for the join condition as a reliable fallback
  if (spatial_function == "ST_Intersection"){
    join_condition <- "ST_Intersects(target.geom, mask.geom)"
  }
  if (!spatial_function %in% c("ST_Intersection")) {
    join_condition <- glue::glue("{spatial_function}(target.geom, mask.geom)")
  }

  # Adjust quoting so that we can use in geopackage when table names have periods in them
  if(quote_tbl) {
    target_tbl <- ldbh_qtable_quote(target_tbl, ...)
    mask_tbl <- ldbh_qtable_quote(mask_tbl, ...)
  }

  # Construct SQL query using glue, ensuring correct application of spatial functions and conditionally including quoted
  # schema tables
  query <- glue::glue(
    "SELECT {cols_target_str}{cols_mask_str}, {spatial_operation_select} ",
    "FROM {target_tbl} AS target ",
    "JOIN {mask_tbl} AS mask ON {join_condition} ",
    "{mask_filter_condition};"
  )


  return(query)
}


# tables <- fpr_db_query(fpr_dbq_lstables())
#
# tables %>%
#   dplyr::filter(str_detect(table_name, "transport"))

# Example usage
test_intersects_poly <- fpr_db_query(
  ldbh_qfilter(
    target_tbl = "whse_admin_boundaries.clab_indian_reserves",
    mask_tbl = "whse_basemapping.fwa_watershed_groups_poly",
    mask_col_filter = "watershed_group_code",
    mask_col_filter_values = c("FRAN", "NECR")
)
)
names(test_intersects_poly)

# example where we clip and join Indian reserves to watershed groups
test_intersects_poly_join_target_col_return <- fpr_db_query(
  ldbh_qfilter(
    target_tbl = "whse_admin_boundaries.clab_indian_reserves",
    mask_tbl = "whse_basemapping.fwa_watershed_groups_poly",
    mask_col_filter = "watershed_group_code",
    mask_col_filter_values = c("FRAN", "NECR"),
    target_col_return = c("clab_id", "english_name")
  )
)

names(test_intersects_poly_join_target_col_return)

# test joining select columns from mask
test_intersects_poly_join_mask_col_return <- fpr_db_query(
  ldbh_qfilter(
    target_tbl = "whse_admin_boundaries.clab_indian_reserves",
    mask_tbl = "whse_basemapping.fwa_watershed_groups_poly",
    mask_col_filter = "watershed_group_code",
    mask_col_filter_values = c("FRAN", "NECR"),
    mask_col_return = c("watershed_group_code", "watershed_group_name")
  )
)
names(test_intersects_poly_join_mask_col_return)



test_intersects_poly_join_mask_target_col_return <- fpr_db_query(
  ldbh_qfilter(
    target_tbl = "whse_admin_boundaries.clab_indian_reserves",
    mask_tbl = "whse_basemapping.fwa_watershed_groups_poly",
    mask_col_filter = "watershed_group_code",
    mask_col_filter_values = c("FRAN", "NECR"),
    mask_col_return = c("watershed_group_code", "watershed_group_name"),
    target_col_return = c("clab_id", "english_name")
  )
)
names(test_intersects_poly_join_mask_target_col_return)

# test with point
test_point_within <- fpr_db_query(
  ldbh_qfilter(
    target_tbl = "whse_fish.pscis_design_proposal_svw",
    mask_tbl = "whse_basemapping.fwa_watershed_groups_poly",
    mask_col_filter = "watershed_group_code",
    mask_col_filter_values = c("FRAN", "NECR"),
    spatial_function = "ST_Within"
  )
)
names(test_point_within)


test_within_point_mask_col_return <- fpr_db_query(
  ldbh_qfilter(
    target_tbl = "whse_fish.pscis_design_proposal_svw",
    mask_tbl = "whse_basemapping.fwa_watershed_groups_poly",
    mask_col_filter = "watershed_group_code",
    mask_col_filter_values = c("FRAN", "NECR"),
    mask_col_return = c("watershed_group_code", "watershed_group_name"),
    spatial_function = "ST_Within"
  )
)
names(test_within_point_mask_col_return)

test_within_point_target_col_return <- fpr_db_query(
  ldbh_qfilter(
    target_tbl = "whse_fish.pscis_design_proposal_svw",
    mask_tbl = "whse_basemapping.fwa_watershed_groups_poly",
    mask_col_filter = "watershed_group_code",
    mask_col_filter_values = c("FRAN", "NECR"),
    target_col_return = c('stream_crossing_id'),
    spatial_function = "ST_Within"
  )
)
names(test_within_point_target_col_return)

test_within_point_target_mask_col_return <- fpr_db_query(
  ldbh_qfilter(
    target_tbl = "whse_fish.pscis_design_proposal_svw",
    mask_tbl = "whse_basemapping.fwa_watershed_groups_poly",
    mask_col_filter = "watershed_group_code",
    mask_col_filter_values = c("FRAN", "NECR"),
    mask_col_return = c("watershed_group_code", "watershed_group_name"),
    target_col_return = 'stream_crossing_id',
    spatial_function = "ST_Within"
  )
)
names(test_within_point_target_mask_col_return)


test_intersects_join_mask_target_abort_extra_geom <- fpr_db_query(
  ldbh_qfilter(
    target_tbl = "whse_admin_boundaries.clab_indian_reserves",
    mask_tbl = "whse_basemapping.fwa_watershed_groups_poly",
    mask_col_filter = "watershed_group_code",
    mask_col_filter_values = c("FRAN", "NECR"),
    mask_col_return = c("watershed_group_code", "watershed_group_name"),
    target_col_return = c("clab_id", "english_name", "geom")
  )
)

# test with line
test_intersects_line <- fpr_db_query(
  ldbh_qfilter(
    target_tbl = "whse_imagery_and_base_maps.mot_road_structure_sp",
    mask_tbl = "whse_tantalis.ta_park_ecores_pa_svw",
    mask_col_filter = "protected_lands_name",
    mask_col_filter_values = c("MORICE LAKE PARK"),
    mask_col_return = c("protected_lands_name", "protected_lands_designation")
  )
)

names(test_intersects_line)

# test line raods
test_intersects_line <- fpr_db_query(
  ldbh_qfilter(
    target_tbl = "whse_basemapping.transport_line",
    mask_tbl = "whse_admin_boundaries.clab_indian_reserves",
    mask_col_return = c("clab_id", "english_name"),
    mask_col_filter = "english_name",
    mask_col_filter_values = c("MORICETOWN 1")
  )
)

names(test_intersects_line)

# test line roads within
test_within_poly <- fpr_db_query(
  ldbh_qfilter(
    target_tbl = "whse_basemapping.transport_line",
    mask_tbl = "whse_admin_boundaries.clab_indian_reserves",
    mask_col_return = c("clab_id", "english_name"),
    mask_col_filter = "english_name",
    mask_col_filter_values = c("MORICETOWN 1"),
    spatial_function = "ST_Within"
  )
)

names(test_within_poly)







# ----------test and plot all spatial functions-------------------

# Vector of spatial functions
spatial_funcs <- c(
  "ST_Intersection",
  "ST_Intersects",
  "ST_Contains",
  "ST_Within",
  "ST_Overlaps",
  "ST_Crosses",
  "ST_Touches")

test_all_line <- map(spatial_funcs,
                                     ~ fpr_db_query(ldbh_qfilter(
                                       target_tbl = "whse_basemapping.transport_line",
                                       mask_tbl = "whse_admin_boundaries.clab_indian_reserves",
                                       mask_col_filter = "english_name",
                                       mask_col_filter_values = c("MORICETOWN 1"),
                                       spatial_function = .x
                                     ))) %>%
  set_names(spatial_funcs)

test <- test_all_line

test %>%
  map(names)

# Create a plot for each object and add the name of the object as the title
plots <- map2(test, names(test), ~ ggplot() +
                geom_sf(data = .x, aes(geometry = geom)) +
                ggtitle(.y))

# Print the plots
print(plots)

test_all_poly <- map(spatial_funcs,
                     ~ fpr_db_query(ldbh_qfilter(
                       target_tbl = "whse_forest_vegetation.veg_comp_lyr_r1_poly",
                       mask_tbl = "whse_admin_boundaries.clab_indian_reserves",
                       mask_col_filter = "english_name",
                       mask_col_filter_values = c("MORICETOWN 1"),
                       spatial_function = .x
                     ))) %>%
  set_names(spatial_funcs)

test <- test_all_poly

test %>%
  map(names)

# Create a plot for each object and add the name of the object as the title
plots <- map2(test, names(test), ~ ggplot() +
                geom_sf(data = .x, aes(geometry = geom)) +
                ggtitle(.y))

ggplot() +
  geom_sf(data = test$ST_Intersection, aes(geometry = geom)) +
  ggtitle("ST_Intersection")

identical(test$ST_Intersection$geom, test$ST_Intersection$geom..188)

ggplot() +
  geom_sf(data = test$ST_Intersection, aes(geometry = geom_original)) +
  ggtitle("ST_Intersection_og")

ggplot() +
  geom_sf(data = test$ST_Intersection, aes(geometry = geom..188)) +
  ggtitle("ST_Intersection_geom..188")

ggplot() +
  geom_sf(data = test$ST_Intersects, aes(geometry = geom)) +
  ggtitle("ST_Intersects")


# Print the plots
print(plots)



test_all_line_mask_col_return <- map(spatial_funcs,
               ~ fpr_db_query(ldbh_qfilter(
                 target_tbl = "whse_basemapping.transport_line",
                 mask_tbl = "whse_admin_boundaries.clab_indian_reserves",
                 mask_col_return = c("clab_id", "english_name"),
                 mask_col_filter = "english_name",
                 mask_col_filter_values = c("MORICETOWN 1"),
  spatial_function = .x
))) %>%
  set_names(spatial_funcs)

test_all_line_mask_col_return %>%
  map(names)

map(test_all_poly_mask_col_return, ~ ggplot() +
      geom_sf(data = .x, aes(geometry = geom)))

test_all_line_mask_target_col_return <- map(spatial_funcs,
                                     ~ fpr_db_query(ldbh_qfilter(
                                       target_tbl = "whse_basemapping.transport_line",
                                       mask_tbl = "whse_admin_boundaries.clab_indian_reserves",
                                       mask_col_return = c("clab_id", "english_name"),
                                       mask_col_filter = "english_name",
                                       mask_col_filter_values = c("MORICETOWN 1"),
                                       target_col_return = c("transport_line_id", "custodian_partner_org"),
                                       spatial_function = .x
                                     ))) %>%
  set_names(spatial_funcs)

test_all_line_mask_target_col_return %>%
  map(names)

map(test_all_line_mask_target_col_return, ~ ggplot() +
      geom_sf(data = .x, aes(geometry = geom)))

wsg <- fpr::fpr_db_query(query = "SELECT watershed_group_code FROM whse_basemapping.fwa_watershed_groups_poly")

# grab just 10% of the watershed groups and get all pscis points outside them
wsg_subset <- wsg %>%
  dplyr::pull(watershed_group_code) %>%
  .[1:((length(.) * 0.1) %>% as.integer())]

test_all_line_negate <- map(spatial_funcs,
                     ~ fpr_db_query(ldbh_qfilter(
                       target_tbl = "whse_fish.pscis_assessment_svw",
                       mask_tbl = "whse_basemapping.fwa_watershed_groups_poly",
                       mask_col_filter = "watershed_group_code",
                       mask_col_filter_values = wsg_subset,
                       mask_col_return = c("watershed_group_code", "watershed_group_name"),
                       mask_col_filter_values_negate = TRUE,
                       spatial_function = .x
                     ))) %>%
  set_names(spatial_funcs)

test_all_line_negate %>%
  map(names)

map(test_all_line_negate, ~ ggplot() +
      geom_sf(data = .x, aes(geometry = geom)))

# ----------test geopackage-------------------

path <- "/Users/airvine/Projects/gis/ng_koot_west_2023/background_layers.gpkg"

t <- sf::st_read(
  dsn = path,
  #test_intersects_poly <- fpr_db_query(
  query = ldbh_qfilter(
    target_tbl = "whse_imagery_and_base_maps.mot_culverts_sp",
    mask_tbl = "fwa_watershed_groups_poly",
    mask_col_filter = "watershed_group_code",
    mask_col_filter_values = c("MORK"),
    quote_tbl = TRUE
  )
)

# to do
# function to drop 'geom..{column_index}' if it is present
# function to drop geom (or optionally rename to geom_original) and rename geom_intersection to geom (all conditional that geom_intersection is present in the sf object)


# or... just don't allow "*" or inclusion of "geom" as the input for target_col_return.
# ie. target_col_return <- names(target_tbl)

# other issues
# the join assumes only 1 match of the target to the mask but there could be many!  Using st_intersection and joining on
# st_intersects is weird because parts of a feature that land outside of the mask will still be joined to the result...

#ideas - could we just use target_layer and mask_layer vs target_tbl, mask_*?


# how to fix
# 1. no * allowed
# 2. if else for st_intersection and use inner_join as per veg layer on db_newgraph
# 3. change table_schema to tbl
