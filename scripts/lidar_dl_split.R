# download lidar tif, split into 1:5000 mapsheets

# define where we are putting it

path <- "~/Projects/gis/lidar"

dir.create(path)

dl <- "https://nrs.objectstore.gov.bc.ca/gdwuts/093/093l/2019/dem/bc_093l048_xli1m_utm09_2019.tif"

system.time(
  download.file(
    url = dl,
    destfile = paste0(path, basename(dl))
  )
)

# get reasonable extent of grid
extract_aoi_grid <- function(aoi_min, aoi_max) {
  min_num <- as.numeric(str_sub(aoi_min, 5, 7))
  max_num <- as.numeric(str_sub(aoi_max, 5, 7))

  min_letter <- str_sub(aoi_min, 4, 4) %>% charToRaw() %>% as.integer()
  max_letter <- str_sub(aoi_max, 4, 4) %>% charToRaw() %>% as.integer()

  letter_seq <- min_letter:max_letter %>% intToUtf8() %>% str_split("") %>% unlist()
  num_seq <- min_num:max_num %>% str_pad(3, pad = "0")

  aoi_seq <- expand.grid(letter_seq, num_seq) %>%
    mutate(Var1 = paste0("093", Var1, Var2)) %>%
    pull(Var1)

  return(aoi_seq)
}

aoi_min <- "093L047"
aoi_max <- "093L049"

grid_val <- extract_aoi_grid(aoi_min, aoi_max)


# get the utm from the filename

zone <- str_extract(basename(dl), "(?<=utm)\\d{2}") |>
  as.numeric()

grid <- bcdata::bcdc_query_geodata(record = "bcgs-1-2-500-mapsheet-grid-nad-83", crs = 26900 + zone ) |>
  dplyr::filter(str_detect(MAP_TILE, str_c(grid_val, collapse = "|"))) |>
  bcdata::collect()

093L048322

grid <- rfp::rfp_bcd_get_data(bcdata_record_id = "bcgs-1-2-500-mapsheet-grid-nad-83")


