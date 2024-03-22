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


# --------------------------------------------- extract prescriptions
# now lets try extracting pages 292 - 355 and feeding it to chattr to produce a tribble of all the prescriptions
pdftools::pdf_subset(path,
                     pages = 292:355,
                     output = "data/skt/mid-bulkley-detailed-fish-habitat-riparian-channel-assessment-for-watershed-restoration/mid-bulkley_detailed_assessment_watershed_restoration_report_prescriptions.pdf")

#Link to the openAI chats which were used to build this script
#https://chat.openai.com/c/c2f49a0a-94ca-4728-84bb-19039df3967b


# Read PDF file and get the number of pages
pdf_text <- pdf_text("data/skt/mid-bulkley-detailed-fish-habitat-riparian-channel-assessment-for-watershed-restoration/mid-bulkley_detailed_assessment_watershed_restoration_report_prescriptions.pdf")
num_pages <- length(pdf_text)

# Initialize an empty tibble
data <- tibble(
  SubBasin = character(),
  Creek = character(),
  Reach = numeric(),
  PrescriptionNumber = numeric(),
  RelatedRiparianPrescription = character(),
  Category = numeric(),
  Location = character(),
  UTM = character(),
  LandTenure = character(),
  ImpactDescription = character(),
  Goals = character(),
  MasterPlanObjectives = character(),
  DescriptionOfProposedWorks = character(),
  TechnicalReferences = character()
)

# Loop through each page of the PDF
for (i in 1:num_pages) {
  # Extract text from the current page
  current_text <- pdf_text[[i]]

  # Check if the current page contains "Sub-Basin:"
  if (grepl("Sub-Basin:", current_text)) {
    # Define regular expressions
    regex_sub_basin <- "Sub-Basin:\\s*([\\s\\S]+?)(?=Creek:|$)"
    regex_creek <- "Creek:\\s*([\\s\\S]+?)(?=Reach:|$)"
    regex_reach <- "Reach:\\s*([\\s\\S]+?)(?=Prescription #:|$)"
    regex_prescription_number <- "Prescription #:\\s*([\\s\\S]+?)(?=Related Riparian Prescription:|$)"
    regex_related_prescription <- "Related Riparian Prescription:\\s*([\\s\\S]+?)(?=Category:|$)"
    regex_category <- "Category:\\s*([\\s\\S]+?)(?=Location:|$)"
    regex_location <- "Location:\\s*([\\s\\S]+?)(?=Land Tenure:|$)"
    regex_utm <- "(?<=UTM )\\d+\\.?\\d*\\s*\\d+\\.?\\d*"
    regex_land_tenure <- "Land Tenure:\\s*([\\s\\S]+?)(?=Impact Description:|$)"
    regex_impact_description <- "Impact Description:\\s*([\\s\\S]+?)(?=\\n\\s*\\n|$)"
    regex_goals <- "Goal\\(s\\):\\s*([\\s\\S]+?)(?=Master Plan Objectives:|$)"
    regex_master_plan_objectives <- "Master Plan Objectives:\\s*([\\s\\S]+?)(?=Description of Proposed Works:|$)"
    regex_description_of_proposed_works <- "Description of Proposed Works:\\s*([\\s\\S]+?)(?=Technical References:|$)"
    regex_technical_references <- "Technical References:\\s*([\\s\\S]+?)(?=\\n\\s*\\n|$)"  # Assuming it's the last section

    # Extract information using regular expressions
    sub_basin <- str_match(current_text, regex_sub_basin)[, 2]
    creek <- str_match(current_text, regex_creek)[, 2]
    reach <- str_match(current_text, regex_reach)[, 2]
    prescription_number <- str_match(current_text, regex_prescription_number)[, 2]
    related_prescription <- str_match(current_text, regex_related_prescription)[, 2]
    category <- str_match(current_text, regex_category)[, 2]
    location <- str_match(current_text, regex_location)[, 2]
    utm <- str_extract(location, regex_utm)  # Extract UTM coordinates from Location
    land_tenure <- str_match(current_text, regex_land_tenure)[, 2]
    impact_description <- str_match(current_text, regex_impact_description)[, 2]
    goals <- str_match(current_text, regex_goals)[, 2]
    master_plan_objectives <- str_match(current_text, regex_master_plan_objectives)[, 2]
    description_of_proposed_works <- str_match(current_text, regex_description_of_proposed_works)[, 2]
    technical_references <- str_match(current_text, regex_technical_references)[, 2]

    # Add extracted prescription to the tibble
    data <- add_row(data,
                    SubBasin = sub_basin,
                    Creek = creek,
                    Reach = as.numeric(reach),
                    PrescriptionNumber = as.numeric(prescription_number),
                    RelatedRiparianPrescription = related_prescription,
                    Category = as.numeric(category),
                    Location = location,
                    UTM = utm,
                    LandTenure = land_tenure,
                    ImpactDescription = impact_description,
                    Goals = goals,  # New
                    MasterPlanObjectives = master_plan_objectives,  # New
                    DescriptionOfProposedWorks = description_of_proposed_works,  # New
                    TechnicalReferences = technical_references)  # New
  }
}

cleaned_data <- data %>%
  mutate(across(everything(), ~gsub("\n", "", .)))

# Write the tibble to a CSV file
cleaned_data %>%
  readr::write_csv("data/ncfdc_1998_prescriptions.csv")
