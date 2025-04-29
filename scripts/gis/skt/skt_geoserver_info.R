library(httr)
library(xml2)
library(tibble)

# Define the GeoServer WFS GetCapabilities URL
url_base <- "https://maps.skeenasalmon.info/geoserver/ows"

# test out diff versions - 1.0.0 works for crs and bbox but 2.0.0 gives no results for those
geo_version <- "1.0.0 "
geo_service = "WFS"

# Send request to GetCapabilities
response <- httr::GET(url_base, query = list(
  service = geo_service,
  version = geo_version,
  request = "GetCapabilities"
))

# Check response status
if (httr::status_code(response) == 200) {
  # Parse XML response
  xml_content <- xml2::read_xml(httr::content(response, as = "text", encoding = "UTF-8"))
  ns <- xml2::xml_ns(xml_content)

  # Extract layer metadata
  layer_names <- xml2::xml_find_all(xml_content, "//d1:FeatureType/d1:Name", ns) |> xml2::xml_text()
  layer_titles <- xml2::xml_find_all(xml_content, "//d1:FeatureType/d1:Title", ns) |> xml2::xml_text()
  layer_descriptions <- xml2::xml_find_all(xml_content, "//d1:FeatureType/d1:Abstract", ns) |> xml2::xml_text()
  crs_list <- xml2::xml_find_all(xml_content, "//d1:FeatureType/d1:SRS", ns) |> xml2::xml_text()

  # Extract bounding box values
  bbox_nodes <- xml2::xml_find_all(xml_content, "//d1:FeatureType/d1:LatLongBoundingBox", ns)
  bbox_values <- lapply(bbox_nodes, function(node) {
    paste(
      xml2::xml_attr(node, "minx"),
      xml2::xml_attr(node, "miny"),
      xml2::xml_attr(node, "maxx"),
      xml2::xml_attr(node, "maxy"),
      sep = ", "
    )
  })

  if (length(bbox_values) < length(layer_names)) {
    bbox_values <- c(bbox_values, rep(NA, length(layer_names) - length(bbox_values)))
  }

  # Ensure descriptions align (some layers may not have descriptions)
  if (length(layer_descriptions) < length(layer_names)) {
    layer_descriptions <- c(layer_descriptions, rep(NA, length(layer_names) - length(layer_descriptions)))
  }
  if (length(crs_list) < length(layer_names)) {
    crs_list <- c(crs_list, rep(NA, length(layer_names) - length(crs_list)))
  }

  # Extract attributes and geometry type from DescribeFeatureType
  attribute_list <- list()
  geom_type_list <- list()

  for (layer in layer_names) {
    response_attr <- httr::GET(url_base, query = list(
      service = geo_service,
      version = geo_version,
      request = "DescribeFeatureType",
      typeName = layer
    ))

    if (httr::status_code(response_attr) == 200) {
      xml_attr_content <- xml2::read_xml(httr::content(response_attr, as = "text", encoding = "UTF-8"))
      ns_attr <- xml2::xml_ns(xml_attr_content)

      attributes <- xml2::xml_find_all(xml_attr_content, "//xsd:element", ns_attr) |> xml2::xml_attr("name")
      geom_type <- xml2::xml_find_first(xml_attr_content, "//xsd:element[contains(@type, 'gml') or contains(@type, 'Geometry') or contains(@type, 'geometry')]", ns_attr) |> xml2::xml_attr("type")

      attribute_list[[layer]] <- paste(attributes, collapse = ", ")
      geom_type_list[[layer]] <- ifelse(is.na(geom_type), "Unknown", geom_type)
    } else {
      attribute_list[[layer]] <- NA
      geom_type_list[[layer]] <- NA
    }
  }

  # Compile into a tibble
  layer_metadata <- tibble(
    layer_name_raw = layer_names,
    layer_title = layer_titles,
    layer_description = layer_descriptions,
    crs = crs_list,
    bbox = bbox_values,
    attributes = unlist(attribute_list),
    geom_type = unlist(geom_type_list)
  )

  # Display lookup table
  print(layer_metadata)
} else {
  cat("Error: Unable to retrieve GetCapabilities. HTTP Status:", httr::status_code(response), "\n")
}


# save the metadata to a csv file in data/inputs_extracted/skt_geoserver_info.csv
readr::write_csv(layer_metadata, "data/inputs_extracted/skt_geoserver_info.csv",)
