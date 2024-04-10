#' Filter postgres Point layer that falls with postgres Polygon Layer
#'
#' This function performs a spatial join between a point layer and a polygon layer. Optionally filters polygon
#'  by specific values of one specified column. The default spatial function is ST_Intersects but you can also
#'  use any of the following functions: ST_Contains, ST_Within, ST_Overlaps, ST_Crosses, ST_Touches.
#'
#' @param pnt_schema_table String (single quoted) schema.table name of the point layer.
#' @param pol_schema_table String (single quoted) schema.table name of the polygon layer.
#' @param pol_col_join String (single quoted) column in the polygon layer to join on.
#' @param pnt_col_return String (single quoted) columns to return from the point layer. Default is '*'.
#' @param pol_col_return String (single quoted) columns to return from the polygon layer. Default is '*'.
#' @param funct String (single quoted) spatial function to use. Default is 'ST_Intersects'.
#'  - ST_Intersects: Returns TRUE if the geometries share any portion of space.
#'  - ST_Contains: Returns TRUE if and only if no points of the second geometry lie in the exterior of the first geometry,
#'  and at least one point of the interior of each geometry intersects the interior of the other.
#'  - ST_Within: Returns TRUE if the first geometry is completely within the second geometry.
#'  - ST_Overlaps: Returns TRUE if the geometries share space, are of the same dimension, but are not completely
#'  contained by each other.
#'  - ST_Crosses: Returns TRUE if the supplied geometries have some, but not all, interior points in common.
#'  - ST_Touches: Returns TRUE if the geometries have at least one point in common, but their interiors do not intersect.
#' @param pol_col_join_values String (double quoted) vector of values to filter the polygon layer on. Default is NULL.
#' @importFrom glue glue
#' @importFrom chk chk_string
#' @family join
#' @return A SQL query string.
#' @examples
#' # Example 1: Basic usage with default parameters
#' fpr_db_query(query = ldbq_filter_pnt_by_poly(
#'   pnt_schema_table = 'whse_fish.pscis_remediation_svw',
#'   pol_schema_table = 'whse_basemapping.fwa_watershed_groups_poly'
#' ))
#'
#' # Example 2: Filtering the polygon layer
#' fpr_db_query(query = ldbq_filter_pnt_by_poly(
#'   pnt_schema_table = 'whse_fish.fiss_stream_sample_sites_sp',
#'   pol_schema_table = 'whse_basemapping.fwa_watershed_groups_poly',
#'   pol_col_join = 'watershed_group_code',
#'   pol_col_join_values = c("FRAN", "NECR")
#' ))
#'
#' # Example 3: Specifying columns to return
#' fpr_db_query(query = ldbq_filter_pnt_by_poly(
#'   pnt_schema_table = 'whse_fish.fiss_stream_sample_sites_sp',
#'   pol_schema_table = 'whse_basemapping.fwa_watershed_groups_poly',
#'   pol_col_join = 'watershed_group_code',
#'   pol_col_join_values = c("FRAN", "NECR"),
#'   pnt_col_return = c('stream_sample_site_id', 'geom'),
#'   pol_col_return = c('watershed_group_code', 'watershed_group_name')
#' ))
#'
#' # Example 4: Specifying columns to return and using ST_Within
#' fpr_db_query(query = ldbq_filter_pnt_by_poly(
#'   pnt_schema_table = 'whse_fish.fiss_stream_sample_sites_sp',
#'   pol_schema_table = 'whse_basemapping.fwa_watershed_groups_poly',
#'   pol_col_join = 'watershed_group_code',
#'   pol_col_join_values = c("FRAN", "NECR"),
#'   pnt_col_return = c('stream_sample_site_id', 'geom'),
#'   pol_col_return = c('watershed_group_code', 'watershed_group_name'),
#'   funct = 'ST_Within'
#' ))

ldbq_filter_pnt_by_poly <- function(
    pnt_schema_table = NULL,
    pol_schema_table = NULL,
    pol_col_join = NULL,
    pnt_col_return = '*',
    pol_col_return = '*',
    funct = 'ST_Intersects',
    pol_col_join_values = NULL) {

  chk::chk_string(pnt_schema_table)
  chk::chk_string(pol_schema_table)
  chk::chk_string(funct)

  if (!is.null(pol_col_join)) {
    chk::chk_string(pol_col_join)
  }
  if (!is.null(pnt_col_return)) {
    chk::chk_character(pnt_col_return)
  }
  if (!is.null(pol_col_return)) {
    chk::chk_character(pol_col_return)
  }
  if (!is.null(pol_col_join_values)) {
    chk::chk_character(pol_col_join_values)
  }
  # Handle vectors for pnt_col_return and pol_col_return
  cols_pnt_str <- if (is.character(pnt_col_return)) {
    glue::glue("point.{glue::glue_collapse(pnt_col_return, sep = ', point.')}")
  } else {
    "point.*"
  }

  cols_pol_str <- if (is.character(pol_col_return)) {
    glue::glue("poly.{glue::glue_collapse(pol_col_return, sep = ', poly.')}")
  } else {
    "poly.*"
  }

  join_on_str <- if (!is.null(pol_col_join_values)) {
    glue::glue(glue::glue_collapse(sprintf("'%s'", pol_col_join_values), sep = ', '))
  } else {
    NULL
  }

  query <- glue::glue("SELECT {cols_pnt_str}, {cols_pol_str}
   FROM {pnt_schema_table} point
   INNER JOIN {pol_schema_table} poly
   ON {funct}(poly.geom, point.geom)")

  if (!is.null(join_on_str)) {
    query <- glue::glue("{query} WHERE poly.{pol_col_join} IN ({join_on_str});")
  } else {
    query <- glue::glue("{query};")
  }

  query
}

# test <- fpr_db_query(ldbq_filter_pnt_by_poly(
#   pnt_schema_table = 'whse_environmental_monitoring.envcan_hydrometric_stn_sp',
#   pol_schema_table = 'whse_basemapping.fwa_watershed_groups_poly',
#   pol_col_join = 'watershed_group_code',
#   pol_col_join_values = c("FRAN", "NECR"),
#   pnt_col_return = c('station_number', 'geom'),
#   pol_col_return = c('watershed_group_code', 'watershed_group_name')
# )
# )
#
#
# t <- fpr_db_query(ldbq_filter_pnt_by_poly(
#       pnt_schema_table = 'whse_environmental_monitoring.envcan_hydrometric_stn_sp',
#       pol_schema_table = 'whse_basemapping.fwa_watershed_groups_poly',
#       pol_col_join = 'watershed_group_code',
#       pol_col_join_values = c("FRAN", "NECR")
# )
# )
#
#
#  t2 <- fpr_db_query(query = ldbq_filter_pnt_by_poly(
#    pnt_schema_table = 'whse_fish.pscis_remediation_svw',
#    pol_schema_table = 'whse_basemapping.fwa_watershed_groups_poly'
#  ))

# Vector of spatial functions
# spatial_funcs <- c("ST_Intersects", "ST_Contains", "ST_Within", "ST_Overlaps", "ST_Crosses", "ST_Touches")
#
# # Use map() to apply each function
# results <- map(spatial_funcs,
#                ~ fpr_db_query(ldbq_filter_pnt_by_poly(
#   pnt_schema_table = 'whse_environmental_monitoring.envcan_hydrometric_stn_sp',
#   pol_schema_table = 'whse_basemapping.fwa_watershed_groups_poly',
#   pol_col_join = 'watershed_group_code',
#   pol_col_join_values = c("FRAN", "NECR"),
#   funct = .x
# ))) %>%
#   set_names(spatial_funcs)
