library(tidyverse)
library(sf)
library(terra)


# download lidar tif, split into 1:5000 mapsheets


# as per ?download.file we need to set a rasonable download time limit - so 5 minutes for a huge file (prob way more than need)
options(timeout = max(300, getOption("timeout")))

# define where we are putting it
path <- "~/Projects/gis/lidar"

dir.create(path)

# there are some SLICK functions to do this here
# https://github.com/bcgov/bcmaps/issues/99


# for now though

dl <- "https://nrs.objectstore.gov.bc.ca/gdwuts/093/093l/2019/dem/bc_093l048_xli1m_utm09_2019.tif"

system.time(
  download.file(
    url = dl,
    destfile = fs::path(path, basename(dl))
  )
)


# get the utm from the filename
zone <- str_extract(basename(dl), "(?<=utm)\\d{2}") |>
  as.numeric()

# get the bounding box of the tif file without reading it in
path_lidar <- fs::path(path, basename(dl))
tif <- terra::rast(path_lidar)

# Extract the bounding box (extent)
tif_bbox <- ext(tif)

# Convert the bounding box to a SpatialPolygon and assign the original CRS
tif_bbox_sp <- as.polygons(tif_bbox, crs = crs(tif))

# Transform the bounding box to EPSG 3005
tif_bbox_sp_transformed <- project(tif_bbox_sp, "EPSG:3005")

# Convert to sf object (optional, for easier handling)
tif_bbox_sf <- st_as_sf(tif_bbox_sp_transformed)

# get the grids that overlap and transform to the same zone (could use INTERSECTS except that
# they don't line up even when we put our grid in the same crs as the tif.... wierd -not sure why)
grid <- bcdata::bcdc_query_geodata("WHSE_BASEMAPPING.BCGS_5K_GRID") |>
  filter(WITHIN(tif_bbox_sf)) %>%
  collect() |>
  # sf::st_transform(crs = 32600 + zone)
  # sf::st_transform(2950 + (zone - 7))
# NAD83(CSRS) / UTM zone 9N (EPSG:3156)
 sf::st_transform(3147 + zone)

# visualize
ggplot2::ggplot() +
  geom_sf(data = grid, fill = "transparent", color = "red") +
  geom_sf(data = tif_bbox_sf, fill = "transparent", color = "blue") +
  theme_minimal()


# split the tif into the grids
split_raster <- function(tif, grid, path) {
  for (i in 1:nrow(grid)) {
    grid_i <- grid[i, ]
    grid_i_sf <- st_as_sf(grid_i)
    tif_i <- terra::crop(tif, grid_i_sf)
    # append the 1:5000 maptile name to the end of the details regarding resolution, utm and year
    writeRaster(tif_i, filename = fs::path(path, paste0(str_extract(basename(dl), "xl[^.]*"), "_", grid_i$MAP_TILE, ".tif")), overwrite = TRUE)
  }
}

split_raster(tif, grid, path)

# make a little function that allows you to copy a file to a new directory in path that you define
path_gis <- "~/Projects/gis/restoration_wedzin_kwa/lidar"
file_cp <- "093L05822.tif"
file_cp <- "093L04832.tif"
fs::dir_create(path_gis)

fs::file_copy(
  fs::path(path, file_cp),
  fs::path(path_gis, file_cp),
  overwrite = TRUE
)

# lets convert them to cogs and load them to s3
library(processx)

working_directory <- "/Users/airvine/Projects/gis/lidar"
# list the files in the working directory with fs
files <- fs::dir_ls(working_directory, glob = "*.tif")

args_stub <- c('run', '-n', 'dff', 'rio', 'cogeo', 'create')

# for each file add the file name twice to the args_stub (ex. c('run', '-n', 'dff', 'rio', 'cogeo', 'create', 'file1', 'file1')
args <- purrr::map(files, ~ c(args_stub, ., .))

# Define the command and working directory
command <- "conda"



system_run <- function(args){
  result <- tryCatch({
    processx::run(
      command,
      args = args,
      echo = TRUE,            # Print the command output live
      wd = working_directory, # Set the working directory
      spinner = TRUE,         # Show a spinner
      timeout = 60            # Timeout after 60 seconds
    )
  }, error = function(e) {
    # Handle errors: e.g., print a custom error message
    cat("An error occurred: ", e$message, "\n")
    NULL  # Return NULL or another appropriate value
  })

  # Check if the command was successful
  if (!is.null(result)) {
    cat("Exit status:", result$status, "\n")
    cat("Output:\n", result$stdout)
  } else {
    cat("Failed to execute the command properly.\n")
  }
}

purrr::walk(args, system_run)

# now lets run aws s3 sync data/lidar/ s3://23cog through processx too
command <- "aws"
args <- c('s3', 'sync', working_directory, 's3://23cog')
system_run(args)


#to load to qgis its like https://23cog.s3.us-west-2.amazonaws.com/bc_093l058_xli1m_utm09_2019.tif

