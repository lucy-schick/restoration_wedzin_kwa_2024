# Load necessary packages
library(tibble)
library(stringr)
library(purrr)
library(pdftools)

# Read PDF file and get the number of pages
pdf_text <- pdftools::pdf_text("data/skt/mid-bulkley-detailed-fish-habitat-riparian-channel-assessment-for-watershed-restoration/mid-bulkley_detailed_assessment_watershed_restoration_report_prescriptions.pdf")
num_pages <- length(pdf_text)

header_pattern <-  "Mid-Bulkley Detailed Fish Habitat/Riparian/Channel Assessment for Watershed Restoration\n\n\n"
appendix_pattern <- "Appendix G-\\d+\\n"
multiple_newlines_pattern <- "\\n{2,}"

# Remove header and appendix text
pdf_text_cleaned <- purrr::map(pdf_text, ~ {
  stringr::str_remove_all(.x, header_pattern) |>
    stringr::str_remove_all(appendix_pattern) |>
    stringr::str_replace_all(multiple_newlines_pattern, "\n")
})

# Combine all pages into a single string
pdf_text_combined <- paste(pdf_text_cleaned, collapse = "\n")

# Separate into individual prescriptions
prescriptions_raw <- stringr::str_split(pdf_text_combined, "Sub-Basin:", simplify = TRUE)

# Remove empty entries and reformat into a list
prescriptions_list <- prescriptions_raw[prescriptions_raw != ""]
prescriptions_list <- c(paste0("Sub-Basin:", prescriptions_list[1]), paste0("Sub-Basin:", prescriptions_list[-1]))

# Turn into list
prescriptions_list_clean <- as.list(prescriptions_list)

# Initialize an empty tibble to store the results
data <- tibble(
  sub_basin = character(),
  creek = character(),
  reach = numeric(),
  prescription_number = numeric(),
  related_riparian_prescription = character(),
  category = numeric(),
  location = character(),
  utm = character(),
  land_tenure = character(),
  impact_description = character(),
  goals = character(),
  master_plan_objectives = character(),
  description_of_proposed_works = character(),
  technical_references = character(),
  survey_and_design_work_required = character(),
  survey_and_design_cost_estimate = character(),
  estimated_cost_of_implementing_works = character(),
  approvals_required = character()
)

# Define the patterns for each section
regex_sub_basin <- "Sub-Basin:\\s*([\\s\\S]+?)(?=Creek:|$)"
regex_creek <- "Creek:\\s*([\\s\\S]+?)(?=Reach:|$)"
regex_reach <- "Reach:\\s*([\\s\\S]+?)(?=Prescription #:|$)"
regex_prescription_number <- "Prescription #:\\s*([\\s\\S]+?)(?=Related Riparian Prescription:|$)"
regex_related_prescription <- "Related Riparian Prescription:\\s*([\\s\\S]+?)(?=Category:|$)"
regex_category <- "Category:\\s*([\\s\\S]+?)(?=Location:|$)"
## We need to extract text from Location all the way to Land Tenure (not just until UTM) because some prescriptions
## have more locations info after the UTM coordinates
regex_location <- "Location:\\s*([\\s\\S]+?)(?=Land Tenure:|$)"
regex_utm <- "(?<=UTM )\\d+\\.?\\d*\\s*\\d+\\.?\\d*"
regex_land_tenure <- "Land Tenure:\\s*([\\s\\S]+?)(?=Impact Description:|$)"
regex_impact_description <- "Impact Description:\\s*([\\s\\S]+?)(?=\\n\\s*\\n|$)"
regex_goals <- "Goal\\(s\\):\\s*([\\s\\S]+?)(?=Master Plan Objectives:|$)"
# regex_master_plan_objectives <- "Master Plan Objectives:\\s*([\\s\\S]+?)(?=Description of Proposed Works:|$)"
regex_master_plan_objectives <- "Master Plan Objectives(?:\\s*\\(see figure [0-9]+\\):)?\\s*([\\s\\S]+?)(?=Description of Proposed Works(?:\\s*\\(see figure [0-9]+\\):)?|Technical References:|$)"
regex_description_of_proposed_works <- "Description of Proposed Works(?:\\s*\\(see figure [0-9]+\\):)?\\s*([\\s\\S]+?)(?=Technical References:|$)"
regex_technical_references <- "Technical References:\\s*([\\s\\S]+?)(?=\\n\\s*\\n|$)"
regex_survey_and_design_work_required <- "Survey and Design Work Required:\\s*([\\s\\S]+?)(?=Survey and Design Cost Estimate:|$)"
regex_survey_and_design_cost_estimate <- "Survey and Design Cost Estimate:\\s*([\\s\\S]+?)(?=Estimated Cost of Implementing Works:|$)"
regex_estimated_cost_of_implementing_works <- "Estimated Cost of Implementing Works:\\s*([\\s\\S]+?)(?=Approvals Required:|$)"
regex_approvals_required <- "Approvals Required:\\s*([\\s\\S]+?)(?=\\n\\s*\\n|$)"

# Extract information using regular expressions and add to the tibble
data <- purrr::map_df(prescriptions_list_clean, ~ {
  current_text <- .x

  # Extract information using regular expressions
  sub_basin <- stringr::str_match(current_text, regex_sub_basin)[, 2]
  creek <- stringr::str_match(current_text, regex_creek)[, 2]
  reach <- stringr::str_match(current_text, regex_reach)[, 2]
  prescription_number <- stringr::str_match(current_text, regex_prescription_number)[, 2]
  related_prescription <- stringr::str_match(current_text, regex_related_prescription)[, 2]
  category <- stringr::str_match(current_text, regex_category)[, 2]
  location <- stringr::str_match(current_text, regex_location)[, 2]
  utm <- stringr::str_extract(location, regex_utm)
  land_tenure <- stringr::str_match(current_text, regex_land_tenure)[, 2]
  impact_description <- stringr::str_match(current_text, regex_impact_description)[, 2]
  goals <- stringr::str_match(current_text, regex_goals)[, 2]
  master_plan_objectives <- stringr::str_match(current_text, regex_master_plan_objectives)[, 2]
  description_of_proposed_works <- stringr::str_match(current_text, regex_description_of_proposed_works)[, 2]
  technical_references <- stringr::str_match(current_text, regex_technical_references)[, 2]
  survey_and_design_work_required <- stringr::str_match(current_text, regex_survey_and_design_work_required)[, 2]
  survey_and_design_cost_estimate <- stringr::str_match(current_text, regex_survey_and_design_cost_estimate)[, 2]
  estimated_cost_of_implementing_works <- stringr::str_match(current_text, regex_estimated_cost_of_implementing_works)[, 2]
  approvals_required <- stringr::str_match(current_text, regex_approvals_required)[, 2]

  # Return a tibble with the extracted data
  tibble(
    sub_basin = sub_basin,
    creek = creek,
    reach = as.numeric(reach),
    prescription_number = as.numeric(prescription_number),
    related_riparian_prescription = related_prescription,
    category = as.numeric(category),
    location = location,
    utm = utm,
    land_tenure = land_tenure,
    impact_description = impact_description,
    goals = goals,
    master_plan_objectives = master_plan_objectives,
    description_of_proposed_works = description_of_proposed_works,
    technical_references = technical_references,
    survey_and_design_work_required = survey_and_design_work_required,
    survey_and_design_cost_estimate = survey_and_design_cost_estimate,
    estimated_cost_of_implementing_works = estimated_cost_of_implementing_works,
    approvals_required = approvals_required
  )
})


extract_utms <- data |>
  # Separate the UTM coordinates. We will leave the UTM info in the location description because in some cases it contains
  # more than one set of UTM coordinates, but since we are only extracted the first set, its good to leave the others somewhere
  tidyr::separate_wider_delim(cols = utm, delim = ".", names = c('utm_zone', 'utm_easting', 'utm_northing'))

cleaned_data <- extract_utms %>%
  # replace all \n with empty strings
  mutate(across(everything(), ~gsub("\n", "", .)))
  # replace none (chr) and N/A (chr) to N/A


  # Write the tibble to a CSV file
  cleaned_data %>%
  readr::write_csv("data/ncfdc_1998_prescriptions.csv")




##-----------make sure we get all the data even if Sub-Basin is not present on the page


fields <- c("Sub-Basin", "Creek", "Reach", "Prescription Number", "Related Riparian Prescription",
            "Category", "Location", "UTM", "Land Tenure", "Impact Description",
            "Goals", "Master Plan Objectives", "Description of Proposed Works", "Technical References")
