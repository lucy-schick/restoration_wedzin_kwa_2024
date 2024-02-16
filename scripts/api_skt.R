library(ckanr)

##set up the access
ckanr_setup(url = "https://data.skeenasalmon.info/", key = Sys.getenv("SKT_API_KEY"))

# what version are we looking for
ckan_version()

# Packages-----------------------------------------------------------------------------------------------------
##get a tibble of the packages available.   a tibble so easy to view
# packages_names <- as_tibble(package_list(as = "table", limit = 10000))

# get all the packages and their detailss
packages_all <- bind_rows(
  package_list_current(as = "table", limit = 10000),
  package_list_current(as = "table", limit = 10000, offset = 1000)
  )


# t <- package_show('wild-salmon-policy', as = 'table') %>%
#   pluck("resources")


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


# filter packages with stringr to find the ones that contain any of the strings in text_filter
dat_filtered <- data_deets %>%
  dplyr::filter(sapply(name, function(x) any(str_detect(x, text_filter))))

# group by why it was filtered
dat_filtered_list <- map(text_filter, ~data_deets %>% filter(str_detect(name, .x))) %>%
  set_names(text_filter) %>%
  bind_rows(.id = "source")

# find the air photo datasets and download a few
air_photo <- data_deets %>%
  dplyr::filter(str_detect(name, "Air Photo"))
  pull(url)

# lets have a look at a Air Photo from 1937 of Lakelse area
url <- "https://data.skeenasalmon.info/dataset/ab2c329c-b5ed-4d95-b830-7cf209ca4dd8/resource/702229f0-36e5-4e87-a58d-848020cdd64e/download/bc40087.tif"


##create a folder to download to
dir.create('data/test')

# grab Lakelse air photo
ckan_fetch(url, store = 'disk', path = paste0('data/skt/', basename(url)))


# ah crap - it looks like they are all combined air photos. We want the raw dog I think.
urls_raw <- air_photo %>%
  filter(package_name == "upper-bulkley-historic-air-photo-mosaics") %>%
  pull(url) %>%
  # we need to remove the NAs as well as those files that end without a file extension at the end (ex. .com/ and *123)
  na.omit() %>%
  .[str_detect(., ".*\\.[a-zA-Z0-9]+$")]

# Use walk to apply ckan_fetch to download the files.
walk(.x = urls_raw,
     .f = ~ckan_fetch(.x, store = 'disk', path = paste0('data/skt/', basename(.x))))






