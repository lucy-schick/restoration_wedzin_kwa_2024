path <- "/Users/airvine/Projects/gis/data/upperbulkley_cumulative_threat_scores.gdb"

fs::dir_ls(path)
library(xml2)

# see the table names
sf::st_layers(path)

# we can't store gdb on mergin so we will read in and burn out as gpkg
d <- sf::st_read(path)

# write to geopackage
sf::st_write(d, "/Users/airvine/Projects/gis/restoration_wedzin_kwa/upperbulkley_cumulative_threat_scores.gpkg")

path <- "/Users/airvine/Projects/gis/data/upperbulkley_cumulative_threat_scores.xml"
doc <- xml2::read_xml(path)
# Define namespaces
ns <- xml2::xml_ns(doc)

# path <- "/Users/airvine/Projects/gis/data/upperbulkley_cumulative_threat_scores.xml"
# doc <- xml2::read_xml(path)
meta <- xml2::xml_find_all(doc, ".//gco:CharacterString", ns) |>
  xml2::xml_text()


meta[5]
field_df <- xml2::xml_find_all(doc, ".//gco:CharacterString", ns) |>
  xml2::xml_text() |>
  (\(x) stringr::str_extract(x[5], "Attribute Description:.*"))() |>
  stringr::str_remove("Attribute Description:") |>
  stringr::str_split_fixed("\\.\\s*(?=\\w+\\s*-)", n = Inf) |>
  as.character() |>
  purrr::map_dfr(\(x) {
    parts <- stringr::str_split_fixed(x, "\\s*-\\s+", 2)
    tibble::tibble(
      field = stringr::str_trim(parts[1]),
      description = stringr::str_trim(parts[2])
    )
  })

