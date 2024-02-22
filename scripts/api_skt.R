library(ckanr) #plus our standards with keyboard shortcut load
source("scripts/functions.R")

##set up the access
ckanr_setup(url = "https://data.skeenasalmon.info/", key = Sys.getenv("SKT_API_KEY"))

# what version are we looking for
ckan_version()

# Packages-----------------------------------------------------------------------------------------------------
# get all the packages and their details
# api limit is 1000 so we need to do it in two steps
packages_all <- bind_rows(
  package_list_current(as = "table", limit = 10000),
  package_list_current(as = "table", limit = 10000, offset = 1000)
  )

data_deets <- packages_all %>%
  # org_packages %>%
  pull(id) %>%
  map(package_show, as = "table") %>%
  map(pluck, "resources") %>%
  purrr::set_names(
    paste(packages_all$organization$name, packages_all$notes, packages_all$publication_yr, packages_all$author,
          packages_all$name, sep = '===')) %>%
  # purrr::set_names(paste(org_packages$org, org_packages$name, sep = "_")) %>%
  bind_rows(.id = "source") %>%
  tidyr::separate(source, c("organization_name", "notes", "publication_yr", "author", "package_name"), sep = "===")

# that takes a while to run so we will save so we don't need to repeat. gitignored....
saveRDS(data_deets, "data/skt/data_deets.rds")

# went quickly through the 3000 datasets and made some notes of the strings we want to detect to filter it down
# would have been smarter to go through the packages names but that can be done too.  Doesn't take that long and is
# a good process to get familiar with the data available
dataset_filter_raw <- "Old Growth Geospatial Data
Old Growth
Legal Old Growth
Air Photo
Photo BC*
name Data Files - Description - Zipped folder containing all the data files used for analysis
name - R Scripts - Description - Zipped folder containing the R-scripts used for data analysis
Riparian
Riparian and In-Stream Assessment of the Bulkley River System
Restoration
Floodplain mapping
Ecological Function
Habitat Report Cards: Upper Bulkley CU
Habitat Report Cards: Upper Skeena CU
Conservation Unit snapshots: Upper Skeena
Conservation Unit snapshots: Upper Bulkley
Annual Knowledge Plan
Mature and Old Forests
Tier 1 Fish and Fish Habitat Assessment
SSAF State of the Value Report
Integrated Watershed Restoration Plan
Upper Bulkley and Morice Water and Salmon Sustainability Views
Upper Bulkley Fish and Aquatic Review Summary of Data
Combined Upper Bulkley Air Photos
UB River Riparian Restoration Project Summary 2021
Upper Bulkley River riparian restoration 2022
Upper Bulkley River Watershed Water Temperature Monitoring Program 2016-21 Data Report
Hourly Water Temperature by Site
Wet'suwet'en"

# Split the text into a vector of strings
dataset_filter <- strsplit(dataset_filter_raw, split = "\n")[[1]]

rm(dataset_filter_raw)

# read iin the rds
data_deets <- readRDS("data/skt/data_deets.rds")

# filter packages with stringr to find the ones that contain any of the strings in text_filter
dat_filtered <- data_deets %>%
  dplyr::filter(sapply(name, function(x) any(str_detect(x, dataset_filter))))

# group by why it was filtered
dat_filtered_list <- map(dataset_filter, ~data_deets %>% filter(str_detect(name, .x))) %>%
  set_names(dataset_filter) %>%
  # remove empty list items
  discard(~nrow(.x) == 0)
  # bind_rows(.id = "source")

# find the air photo datasets and download a few
air_photo <- data_deets %>%
  dplyr::filter(str_detect(name, "Air Photo"))
  pull(url)


##create a folder to download to
dir.create('data/test')


urls <- air_photo %>%
  filter(package_name == "upper-bulkley-historic-air-photo-mosaics") %>%
  pull(url) %>%
  # to avoid dl errors we need to remove the NAs as well as those files that end without a file extension at the end (ex. .com/ and *123)
  na.omit() %>%
  .[str_detect(., ".*\\.[a-zA-Z0-9]+$")]

# Use walk (designed to be used for its side effects vs map which returns info to get ckan_fetch to download all the files.
walk(.x = urls,
     .f = ~ckan_fetch(.x, store = 'disk', path = paste0('data/skt/', basename(.x))))


# ah crap - it looks like they are all combined air photos. We want the raw dog I think.

# let's try the lakelse air photos
urls <- air_photo %>%
  filter(package_name == "lakelse-1937-historical-air-photo-archive") %>%
  pull(url) %>%
  # we need to remove the NAs as well as those files that end without a file extension at the end (ex. .com/ and *123)
  na.omit() %>%
  .[str_detect(., ".*\\.[a-zA-Z0-9]+$")]

# Use walk (designed to be used for its side effects vs map which returns info to get ckan_fetch to download all the files.
walk(.x = urls,
     .f = ~ckan_fetch(.x, store = 'disk', path = paste0('data/skt/', basename(.x))))

# this can be used to read in the data_deets file so it need not always be rebuilt (takes a few minutes)
# data_deets <- readRDS("data/skt/data_deets.rds")

# there is a function in the scripts/functions.R file that will download all the files from a package at once
fetch_package(package_nm = "upper-bulkley-fish-and-aquatic-review-riparian-disturbance")
fetch_package(package_nm = "upper-bulkley-historic-air-photo-mosaics")
fetch_package(package_nm = "riparian-ecosystems-and-fish-habitat")



