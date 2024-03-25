# extract elements of text proceeded by a colon and preserve their order to facilitate the mining
# of pdfs to tibbles so that info can be used in new reporting.
library(tidyverse)
library(pdftools)

path <- "data/skt/mid-bulkley-detailed-fish-habitat-riparian-channel-assessment-for-watershed-restoration/mid-bulkley_detailed_assessment_watershed_restoration_report_prescriptions.pdf"
text_content <- pdf_text(path)



# Step 2: Refine Extraction with Improved Regex
# Adjusting the regex and post-processing to handle variations like "crossing\nLand Tenure"
potential_field_names <- str_extract_all(text_content, "\\n\\s*[A-Za-z0-9\\-]+(?:[\\s\\n]+[A-Za-z0-9\\-]+)*:") %>%
  unlist() %>%
  map_chr(~ str_remove(.x, "^\\n\\s*")) %>%
  map_chr(~ str_trim(.x, side = "both")) %>%
  map_chr(~ str_remove(.x, ":$")) %>%
  # Normalize variations by extracting the last word before the colon if it follows a pattern indicating a subcategory
  map_chr(~ if_else(str_detect(.x, "\\n"), str_extract(.x, "[A-Za-z0-9\\-]+$"), .x))

# Step 3: Identify and Preserve Order of Repeated Field Names
# Initialize an empty list to track occurrences
field_names_list <- list()

# Loop through each potential field name
for (name in potential_field_names) {
  if (!name %in% names(field_names_list)) {
    # If the name isn't in the list yet, add it with a count of 1
    field_names_list[[name]] <- 1
  } else {
    # If it is, increment the count
    field_names_list[[name]] <- field_names_list[[name]] + 1
  }
}

# Now, extract names that occur more than once while preserving order
repeated_field_names <- names(field_names_list)[unlist(field_names_list) > 1]

# Since unique_field_names are less relevant for order preservation, they can still be identified as before or simply by exclusion
unique_field_names <- names(field_names_list)[unlist(field_names_list) == 1]

unique_field_names
repeated_field_names
