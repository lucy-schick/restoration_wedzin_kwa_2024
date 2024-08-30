##pull the cv info from the Bulkley File

{
  library(tidyverse)
  library(tabulizer)
  library(pdftools)
}

source("scripts/functions.R")
# fetch_package(package_nm= "mid-bulkley-detailed-fish-habitat-riparian-channel-assessment-for-watershed-restoration")

path <- "data/skt/mid-bulkley-detailed-fish-habitat-riparian-channel-assessment-for-watershed-restoration/mid-bulkley_detailed_assessment_watershed_restoration_report.pdf"


# because this table spans multiple pages we will need to extract the tables from each page and then combine them


#you would run with this the first time
tab_trim_10 <- tabulizer::locate_areas(path, 10)

##since we have done this before though - numbers below are results
# top      left    bottom     right
# 78.22822  78.11779 673.92883 528.05153


tab_trim_10 = list(c(78.22822,  78.11779, 673.92883, 528.05153 ))

##extract the tables useing the areas you defined
table_10_raw <- tabulizer::extract_tables(path,
                                          pages = seq(10,10),
                                          method = "lattice",
                                          area = tab_trim_10)

# ##this is how we make a clean dataframe
table_10_df <- table_10_raw %>%
  pluck(1) %>%
  as_tibble() %>%
  janitor::row_to_names(1) %>%
  janitor::clean_names()



#you would run with this the first time
tab_trim_11 <- tabulizer::locate_areas(path, 11)

##since we have done this before though - numbers below are results
# top      left    bottom     right
# 79.20000  84.92025 617.56564 549.43067


tab_trim_11 = list(c(79.20000,  84.92025, 617.56564, 549.43067))

##extract the tables useing the areas you defined
table_11_raw <- tabulizer::extract_tables(path,
                                          pages = seq(11,11),
                                          method = "lattice",
                                          area = tab_trim_11)

# ##this is how we make a clean dataframe
table_11_df <- table_11_raw %>%
  pluck(1) %>%
  as_tibble() %>%
  janitor::row_to_names(1) %>%
  janitor::clean_names()

# bind the tables together
table_10_11 <- bind_rows(table_10_df, table_11_df)

# burn it out to a csv so we can point to it and read it in later
table_10_11 %>%
  readr::write_csv("data/ncfdc_1998_prescriptions_raw.csv")

# to do
# remove nas and - and insert real nas






