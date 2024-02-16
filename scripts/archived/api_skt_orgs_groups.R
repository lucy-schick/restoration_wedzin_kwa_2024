# so much looking around here that want to keep around for a while but not clog the main script


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


## View the datasets contained within the packages ----------------------------------------------------------------------------------------------------
org_dat_ids <- org_packages %>%
  pull(id) %>%
  map(package_show, as = "table") %>%
  map(pluck, "resources") %>%
  purrr::set_names(paste(org_packages$org, org_packages$name, sep = "_")) %>%
  bind_rows(.id = "source") %>%
  # sometimes we have more than one underscore so we use merge to merge the extra ones
  tidyr::separate(source, c("org", "name"), sep = "_", extra = "merge")

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



##Groups----------------------------------------------------------------------------------------------------
# lets try working around the organizations by working in groups and see what happens
groups <- group_list(as = "table", all_fields = TRUE, limit = 1000)




