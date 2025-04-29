# get the bbox of our aoi
# separate by sub-basin
# lets build a custom watersehed just for upstream of the confluence of Neexdzii Kwa and Wetzin Kwa
# blueline key
blk <- 360873822
# downstream route measure
drm <- 166030.4

aoi <- fwapgr::fwa_watershed_at_measure(blue_line_key = blk,
                                        downstream_route_measure = drm) |>
  sf::st_transform(crs = 3005)

aoi_bb <- sf::st_bbox(aoi)

# get layer metadata from skt_geoserver_info.R
layer_prep <- layer_metadata |>
  dplyr::filter(
    stringr::str_detect(layer_title, "ESI Fish & Fish Habitat Indicator")
  ) |>
  # not used yet
  dplyr::mutate(
    indicator = stringr::str_remove(layer_title, "^ESI Fish & Fish Habitat Indicator [^:]+: "),
    layer_name = janitor::make_clean_names(indicator),
    column_name = stringr::str_remove(layer_name_raw, "^geonode:")
  )

layer_ls <- layer_prep |>
  dplyr::pull(layer_name_raw)

layer_names_out <- layer_prep |>
  dplyr::pull(layer_name)

dir_out <- "data/gis/skt/esi_sows"

# specify the names out and download them all
purrr::pwalk(
  .l = list(
    layer_name_raw = layer_ls,
    layer_name_out = layer_names_out
  ),
  .f = ngr::ngr_spk_geoserv_dlv,
  dir_out = dir_out,
  bbox = aoi_bb  # only grab the watersheds that fall in our aoi bbox
)

# really we have a pile of redundant geom info. We only need the assessment columns and not the geoms
# list the geojsons and read them in
wshd_esi_ls <- fs::dir_ls(dir_out)

wshd_esi_raw <- wshd_esi_ls |>
  purrr::map(sf::st_read)

path_background_layers <- "/Users/airvine/Projects/gis/restoration_wedzin_kwa/background_layers.gpkg"

layer_info <- ngr::ngr_spk_layer_info(path = path_background_layers) |>
  dplyr::arrange(name)

wshd_ass_raw <- sf::st_read(path_background_layers,
              layer = "whse_basemapping.fwa_assessment_watersheds_poly")

# get a list of all the col names in wshd_esi_raw
wshd_esi_names <- wshd_esi_raw |>
  purrr::map(names)

#join esi cols to the whse_basemapping.fwa_assessment_watersheds_poly -----------------------------------------------------------------------------------------------------
# clean up the inputs and remove duplicates
wshd_clean <- function(wshd){
  wshd |>
    sf::st_drop_geometry() |>
    dplyr::select(-id, -AU_name, -WTRSHD_) |>
    # there are duplicate polygons
    dplyr::distinct()
}

wshd_esi_prep <- wshd_esi_raw |>
  purrr::map(
    wshd_clean
  )

# join the esi together
wshd_esi_prep2 <- purrr::reduce(wshd_esi_prep, dplyr::inner_join, by = "FID")


# join esi to the provincial assessment layer
wshd_esi_prep3 <- dplyr::left_join(
  wshd_ass_raw |> dplyr::mutate(id = as.integer(id)),
  wshd_esi_prep2,
  by = c("id" = "FID")
)

wshd_esi_prep3 |>
  sf::st_write(
    "/Users/airvine/Projects/gis/restoration_wedzin_kwa/wshd_esi_prep.geojson"
  )


# keep just the ones that overlap our study area
wshd_esi <- sf::st_intersection(
  wshd_esi_prep3,
  aoi |>
    sf::st_geometry()
)
  # dplyr::filter(
  #   # keep only polygons
  #   sf::st_geometry_type(geom) == "POLYGON",
  #   sf::st_is_valid(geom)
  # ) |>
  # dplyr::mutate(area_ha2 = as.numeric(sf::st_area(geom)/10000)) |>
  # # we are hacking around here b/c our
  # dplyr::filter(abs(area_ha - area_ha2) <= 0.01)  # 100 m² = 0.01 ha

# burn to the project
wshd_esi |>
  sf::st_write(
    "/Users/airvine/Projects/gis/restoration_wedzin_kwa/wshd_esi.geojson",
    delete_dsn = TRUE
  )

library(geosapi)
# ok this worked but guessing our credentials got changed after we hit it up.  Our function seems a bit more robust
# but this is likely the better way.
# https://github.com/eblondel/geosapi
gsman <- geosapi::GSManager$new(
  url = "https://maps.skeenasalmon.info/geoserver", #baseUrl of the Geoserver
  user = "al_irvine", pwd = "salmon2020!", #credentials
  logger = "INFO" #logger, for info or debugging purpose
)

# layerNames <- gsman$getLayerNames()
