library(ckanr)

##set up the access
ckanr_setup(url = "https://data.skeenasalmon.info/", key = Sys.getenv("SKT_API_KEY"))

# what version are we looking for
ckan_version()

# Organizations-----------------------------------------------------------------------------------------------------
# get a list of all the organizations - this is returning only 25 orgs and there are obviously way more
# note sure why this is happening
orgs <- organization_list(as = "table", limit = "1000")

# # if we don't use `as='table' (doh!) we needed to do this
# # Use map to get the names of the nested list items in each element
# nested_names <- map(p, names)
#
# # Extract all items that are not lists themselves
# items <- map(p, ~.x[!sapply(.x, is.list)])
#
# # Extract all items that are not lists themselves and convert them into tibbles
# dat <- map(items, ~as_tibble(as.list(.x[!sapply(.x, is.null)]))) %>%
#   bind_rows()


# to get around the limit of 25 organizations we can use the following code which calls hidden helper functions
# with ::: to get the full list of organizations
orgs <- ckanr:::jsd(
  ckanr:::ckan_GET(
    url = "https://data.skeenasalmon.info/",
    "organization_list",
    key = Sys.getenv("SKT_API_KEY"))
  )


# however this just gives us the org name and that's it.  If we ask for all fields we can just get 25 orgs
# at a time. If we wanted to get them all we could set `offset` and page through 1:176 in chunks and join
# together.  I don't think we will bother yet though because we cannot retreive all the packages for an org anyway
# Could be fixed by SKT by setting site’s configuration ckan.group_and_organization_list_all_fields_max
orgs_all_paged <- ckanr:::jsd(
  ckanr:::ckan_GET(
    url = "https://data.skeenasalmon.info/",
    "organization_list",
    # If we ask for all fields we just get 25 orgs
    # doesn't matter if we set limit = 1000 or rows = 1000
    # can work around by starting at offset = 0 and then incrementing by 25
    query = ckanr:::cc(list(all_fields = TRUE, offset = 26, rows = 50)),
    key = Sys.getenv("SKT_API_KEY"))
)

# get details about an organization including their datasets.
org_deets <- organization_show(
  "morice-water-monitoring-trust",
  include_datasets = TRUE,
  as = "table"
)

# get morice-water-monitoring-trust package ids and associated information
# package_count = 19 but packages shows only 10 t
# known issues here https://github.com/ckan/ckan/issues/6295 and https://github.com/ropensci/ckanr/issues/183 (commented here)
org_packages <- org_deets %>%
  pluck("packages")

# we try to get all the packages by hacking underlying call to the api (ckanr:::ckan_GET , ckanr:::ckan_VERB , ckanr:::jsd)
# but it appears the api is set to only return first 10 packages.  See https://docs.ckan.org/en/2.9/api/ and
# screenshot at https://github.com/ropensci/ckanr/issues/183
org_packages_raw <- ckanr:::jsd(
  ckanr:::ckan_GET(url = "https://data.skeenasalmon.info/",
                   "organization_show",
                   query = ckanr:::cc(list(id = "965a9f57-4deb-434d-b507-101400a755d3",
                                           include_datasets = TRUE,
                                           rows = 1000
                                           )),
                   key = Sys.getenv("SKT_API_KEY")
  )
) %>%
  pluck("packages")

##Groups----------------------------------------------------------------------------------------------------
# lets try working around the organizations by working in groups and see what happens
groups <- group_list(as = "table", all_fields = TRUE, limit = 1000)



## View the datasets contained within the packages ----------------------------------------------------------------------------------------------------

# get_ids <- function(package_id, type = "resources"){
#   package_show(
#     id = package_id,
#     as = "table"
#   ) %>%
#     pluck(type)
# }

org_dat_ids <- org_packages %>%
  pull(id) %>%
  map(package_show, as = "table") %>%
  map(pluck, "resources") %>%
  purrr::set_names(paste(org_packages$org, org_packages$name, sep = "_")) %>%
  bind_rows(.id = "source") %>%
  # sometimes we have more than one underscore so we use merge to merge the extra ones
  tidyr::separate(source, c("org", "name"), sep = "_", extra = "merge")

# dat_ids <- org_packages %>%
#   pull(id) %>%
#   map_dfr(get_ids, .id = "source")

org_urls_raw <- org_dat_ids %>% pull(url)

# we need to remove the NAs as well as those files that end without a file extension at the end (ex. .com/ and *123)
org_urls <- org_urls_raw %>%
  na.omit() %>%
  .[str_detect(., ".*\\.[a-zA-Z0-9]+$")]

##create a folder to download to
dir.create('data/test')

# Use walk to apply ckan_fetch to download the files.
walk(.x = org_urls,
    .f = ~ckan_fetch(.x, store = 'disk', path = paste0('data/skt/', basename(.x))))




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




# get_dat_ids <- function(){
#   packages_all %>%
#   # org_packages %>%
#   pull(id) %>%
#   map(package_show, as = "table") %>%
#   map(pluck, "resources") %>%
#   purrr::set_names(packages_all$organization$name) %>%
#   # purrr::set_names(paste(org_packages$org, org_packages$name, sep = "_")) %>%
#   bind_rows(.id = "organization_name")
#   # sometimes we have more than one underscore so we use merge to merge the extra ones
#   # tidyr::separate(source, c("org", "name"), sep = "_", extra = "merge")
# }
#
# dat_ids_all <- packages_all %>%
#   pull(value) %>%
#   map_dfr(get_ids)


# went quickly through the 3000 datasets and made some notes of the strings we want to detect to filter it down
# will adapt to see what

text_filter_raw <- "Old Growth Geospatial Data
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
text_filter <- strsplit(text_filter_raw, split = "\n")[[1]]



# filter packages with stringr to find the ones that contain any of the strings in text_filter

packages_filtered <- dat_ids_all %>%
  filter(sapply(name, function(x) any(str_detect(x, text_filter))))

packages_filtered_list <- map(text_filter, ~dat_ids_all %>% filter(str_detect(name, .x))) %>%
  set_names(text_filter) %>%
  bind_rows(.id = "source")


water_temp <- packages_all %>%
  dplyr::filter(str_detect(value, "water-temperature"))


##list the files that have a certain tag
files <- tag_list('water temperature', as = 'table')

##get details about groups
groups <- group_list()


# https://data.skeenasalmon.info/dataset/f8739b35-6db5-4661-a4c7-93535d67b2a9/resource/ee82b34e-4e17-40d5-a83f-bf757edba323/download/may-24-2023.zip
##find the package id
test <- package_show('wild-salmon-policy', as = 'table')$resources[, 1:20]
test <- package_show('ee82b34e-4e17-40d5-a83f-bf757edba323', as = 'table')$resources[, 1:10]

##identify what we want to download
res <- resource_show(id = "ee82b34e-4e17-40d5-a83f-bf757edba323", as = "table")

##create a folder to download to
dir.create('data/test')

##download and name the same as in the SKT
ckan_fetch(res$url, 'disk', path = paste0('data/', basename(res$url)))


