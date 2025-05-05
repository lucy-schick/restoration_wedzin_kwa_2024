##funciton ot find a string in your directory from https://stackoverflow.com/questions/45502010/is-there-an-r-version-of-rstudios-find-in-files

fif <- function(what, where=".", in_files="\\.[Rr]$", recursive = TRUE,
                ignore.case = TRUE) {
  fils <- list.files(path = where, pattern = in_files, recursive = recursive)
  found <- FALSE
  file_cmd <- Sys.which("file")
  for (fil in fils) {

    if (nchar(file_cmd) > 0) {
      ftype <- system2(file_cmd, fil, TRUE)
      if (!grepl("text", ftype)[1]) next
    }
    contents <- readLines(fil)
    res <- grepl(what, contents, ignore.case = ignore.case)
    res <- which(res)
    if (length(res) > 0) {
      found <-  TRUE
      cat(sprintf("%s\n", fil), sep="")
      cat(sprintf(" % 4s: %s\n", res, contents[res]), sep="")
    }
  }
  if (!found) message("(No results found)")
}

# Function to clip a point layer to a polygon layer based on a join column and factor
# @param schema_table_point String (quoted) name of point layer schema.table
# @param schema_table_polygon String (quoted) name of polygon layer schema.table
# @param join_column String (quoted) name of column to join tables on from polygon. See column names of any table with \link{fpr_dbq_lscols}
# @param join_on String (quoted) or vector of specific terms to join on.
#
lfpr_dbq_clip <- function(
    schema_table_point,
    schema_table_polygon,
    join_column,
    join_on) {

  join_on_string <- paste0("'", join_on, "'", collapse = ", ")

  glue::glue("SELECT point.*, poly.{join_column}
   FROM {schema_table_point} point
   INNER JOIN {schema_table_polygon} poly
   ON ST_Intersects(poly.geom, point.geom)
   WHERE poly.{join_column} IN ({join_on_string});")

}

# Creates hydrographs
#' @param station String (quoted) station number
#' @param pane_hydat Boolean TRUE if you want a pane layout of all hydrographs
#' @param single_hydat Boolean TRUE if you want a single hydrograph with mean flows
#' @param start_year Specific start year, if not specified, will use the first year of the data
#' @param end_year Specific end year, if not specified, will use the first year of the data
#' @param fig/hydrology_stats_ hydrology stats figure saved to the fig folder
#' @param fig/hydrograph_ hydrograph figure saved to the fig folder

lfpr_create_hydrograph <- function(
    station = NULL,
    pane_hydat = TRUE,
    single_hydat = TRUE,
    start_year = NULL,
    end_year = NULL){

  if(is.null(station)){
    poisutils::ps_error('Please provide a station number, for example "08EE004"')
  }

  chk::chk_string(station)
  chk::chk_flag(pane_hydat)
  chk::chk_flag(single_hydat)

  flow_raw <- tidyhydat::hy_daily_flows(station)

  if(is.null(start_year)){
    start_year <- flow_raw$Date %>% min() %>% lubridate::year()
  }

  if(is.null(end_year)){
    end_year <- flow_raw$Date %>% max() %>% lubridate::year()
  }

  chk::chk_number(start_year)
  chk::chk_number(end_year)

  tidyhat_info <- tidyhydat::search_stn_number(station)



  ##### Hydrograph Stats #####

  ##build caption for the stats figure
  caption_info <- dplyr::mutate(tidyhat_info, title_stats = paste0(stringr::str_to_title(STATION_NAME),
                                                                   " (Station #",STATION_NUMBER," - Lat " ,round(LATITUDE,6),
                                                                   " Lon ",round(LONGITUDE,6), "). Available daily discharge data from ", start_year,
                                                                   # FIRST_YEAR, ##removed the default here
                                                                   " to ",end_year, "."))

  hydrograph1_stats_caption <- caption_info$title_stats



  if (pane_hydat == TRUE){
    #Create pane of hydrographs with "Mean", "Minimum", "Maximum", and "Standard Deviation" flows
    hydrograph_stats_print <- fasstr::plot_data_screening(station_number = station, start_year = start_year,
                                                          include_stats = c("Mean", "Minimum", "Maximum", "Standard Deviation"),
                                                          plot_availability = FALSE)[["Data_Screening"]] + ggdark::dark_theme_bw() ##first version is not dark
    hydrograph_stats_print

    #Save hydrograph pane
    ggplot2::ggsave(plot = hydrograph_stats_print, file=paste0("fig/hydrology_stats_", station, ".png"),
                    h=3.4, w=5.11, units="in", dpi=300)

   cli::cli_alert(hydrograph1_stats_caption)
  }



  ##### Single Hydrograph  #####

  ##build caption for the single figure
  caption_info2 <- dplyr::mutate(tidyhat_info, title_stats2 = paste0(stringr::str_to_title(STATION_NAME),
                                                                     " (Station #",STATION_NUMBER," - Lat " ,round(LATITUDE,6),
                                                                     " Lon ",round(LONGITUDE,6), "). Available mean daily discharge data from ", start_year,
                                                                     # FIRST_YEAR, ##removed the default here
                                                                     " to ",end_year, "."))

  hydrograph1_stats_caption2 <- caption_info2$title_stats2

  if (single_hydat == TRUE){
    # Create single hydrograph with mean flows from date range
    flow <- flow_raw %>%
      dplyr::mutate(day_of_year = yday(Date)) %>%
      dplyr::group_by(day_of_year) %>%
      dplyr::summarise(daily_ave = mean(Value, na.rm=TRUE),
                       daily_sd = sd(Value, na.rm = TRUE),
                       max = max(Value, na.rm = TRUE),
                       min = min(Value, na.rm = TRUE)) %>%
      dplyr::mutate(Date = as.Date(day_of_year))

    plot <- ggplot2::ggplot()+
      ggplot2::geom_ribbon(data = flow, aes(x = Date, ymax = max,
                                            ymin = min),
                           alpha = 0.3, linetype = 1)+
      ggplot2::scale_x_date(date_labels = "%b", date_breaks = "2 month") +
      ggplot2::labs(x = NULL, y = expression(paste("Mean Daily Discharge (", m^3, "/s)", sep="")))+
      ggdark::dark_theme_bw() +
      ggplot2::geom_line(data = flow, aes(x = Date, y = daily_ave),
                         linetype = 1, linewidth = 0.7) +
      ggplot2::scale_colour_manual(values = c("grey10", "red"))
    plot

    ggplot2::ggsave(plot = plot, file=paste0("fig/hydrograph_", station, ".png"),
                    h=3.4, w=5.11, units="in", dpi=300)

    cli::cli_alert(hydrograph1_stats_caption2)
  }
}



# make a function for downloading files straight from skt ckan
fetch_package <- function(package_nm = NULL,
                          ckan_info = data_deets,
                          store = "disk",
                          path_stub = "data/skt/",
                          csv_output = TRUE) {
  info <- ckan_info |>
    dplyr::filter(package_name == package_nm)

  urls <- info |>
    dplyr::pull(url) |>
    # to avoid dl errors we need to remove the NAs as well as those files that end without a file extension at the end (ex. .com/ and *123)
    na.omit() %>%
    .[stringr::str_detect(., ".*\\.[a-zA-Z0-9]+$")]

  # create the directory if it doesn't exist
  dir.create(paste0(path_stub, package_nm))

  purrr::walk(.x = urls,
       .f = ~ckanr::ckan_fetch(.x, store = store, path = paste0(path_stub, package_nm, "/", basename(.x))))

  # if csv_output = TRUE burn out a little csv file of the information about everything that is downloaded
  if (csv_output) {
    info |>
      dplyr::arrange(basename(url)) |>
      readr::write_csv(paste0(path_stub, package_nm, "/001_pkg_info_", package_nm, ".csv"))
  }
}

# make column names for excel cols that span multiple rows (ie. 6 and 7)
wkb_col_names <-  function(wkb,
                           slice_from = 5,
                           slice_to = 6,
                           max_col = NULL
){
  a <- wkb %>%
    slice(slice_from:slice_to) %>%
    # rownames_to_column() %>%
    t() %>%
    tibble::as_tibble() %>%
    tidyr::fill(V1, .direction = 'down') %>%
    dplyr::mutate(across(everything(), .fns = ~replace_na(.,'')))
  if(slice_from != slice_to){
    a <- a %>% dplyr::mutate(col_names = paste0(V1, V2))
  }else a <- a %>% dplyr::mutate(col_names = V1)
  # replace_na(list(V2 = "unknown")) %>%
  # (col_names = paste0(V1, V2)) %>%
  b <- a %>% pull(col_names) %>%
    janitor::make_clean_names()
  if(!is.null(max_col)){length(b) <- max_col}
  return(b)
}

#' Check for NULL values in a specific column of a SQL table
#'
#' @param schema_table A string specifying the schema and table in the format "schema.table".
#' @param col_nulls A string specifying the column to check for NULL values.
#'
#' @return A string containing the SQL query.
#' @importFrom glue glue
#' @export
ldbq_chk_null <- function(schema_table, col_nulls) {
  query <- glue::glue(
    "SELECT COUNT(*)
    FROM {schema_table}
    WHERE {col_nulls} IS NULL;"
  )

  return(query)
}


ldfo_sad_plot_line <- function(dat, region, col_y, col_facet, col_group, col_group_exclude = NULL, value_group_exclude = NULL, theme = ggdark::dark_theme_bw()) {
  dat <- dat %>%
    dplyr::filter(waterbody == region) %>%
    dplyr::filter(!is.na(!!sym(col_y)))

  if (!is.null(col_group_exclude)) {
    dat <- dat |>
      dplyr::filter(!!sym(col_group_exclude) != value_group_exclude)
  }

  dat %>%
    arrange(!!sym(col_facet), !!sym(col_group)) %>%
    ggplot(aes(x = !!sym(col_group), y = !!sym(col_y), group = !!sym(col_facet))) +
    geom_line() +
    geom_point() +
    facet_wrap(as.formula(paste0("~", col_facet)), scales = "free") +
    labs(x = "Year", y = col_y) +
    theme
}



#' #' Priority Scoring for Numeric Values
#' #'
#' #' Applies priority scoring to a dataset based on numeric values and corresponding rank weights.
#' #'
#' #' This function joins a data frame of values to be scored with a data frame of ranking weights.
#' #' It matches values dynamically by category, extracts scores, and prepares an output with numeric
#' #' scores suitable for downstream summarization.
#' #'
#' #' @param dat_values [data.frame] A data frame containing the numeric values to be scored. The name of the column
#' #' that contains the numeric scores must align with `col_rank` param.
#' #' @param dat_ranks [data.frame] A data frame containing the ranking weights and categories. Contains the columns
#' #' "weight_value_low" "weight_value_mod" "weight_value_high" "weight_score_low" "weight_score_mod" "weight_score_high"
#' #' @param col_filter [character] A single string specifying the column in `dat_ranks` used to filter for the scoring category. Default is `'column_name_raw'`.
#' #' @param col_rank [character] A single string specifying the ranking category to use for scoring. This string must
#' #' match the name of the column from `dat_values` that contains the numeric value that will recieve its coinciding score.  Must also match
#' #' the name of a parameter found in the `col_filter` column of `dat_ranks` (ex. a `weight_value_low
#' #' for `bulkley_falls_downstream` is 1 and and if that value of 1 is present in the `bulkley_falls_downstream` column of `dat_values`
#' #' then it will be joined with a cooinciding `weight_score_low` of 0 - from the `dat_ranks` dataframe)
#' #' @param col_idx [character] Optional. A single string specifying the column name to use as an identifier in the output.
#' #' If the column does not exist, row numbers will be used. Default is `'idx'`.
#' #'
#' #' @returns A [tibble][tibble::tibble] containing the identifier column and numeric score columns.
#' #' If `col_idx` does not exist in `dat_values`, it is created as row numbers. All score columns are converted to numeric.
#' #'
#' #' @details
#' #' The function works by:
#' #' 1. Filtering `dat_ranks` for the desired ranking category.
#' #' 2. Reshaping and cleaning the ranking data.
#' #' 3. Joining `dat_values` with the cleaned ranks based on the dynamic value category.
#' #' 4. Extracting and converting score columns to numeric.
#' #'
#' #' Note: Input validation is performed using [chk::chk_*()] functions.
#' #'
#' #' @seealso
#' #' [dplyr::filter()], [dplyr::left_join()], [tidyr::pivot_longer()], [tidyr::pivot_wider()], [stringr::str_remove()]
#' #'
#' #' @importFrom dplyr filter select mutate across left_join contains rename_with all_of row_number matches
#' #' @importFrom tidyr pivot_longer separate pivot_wider
#' #' @importFrom stringr str_remove
#' #' @importFrom sf st_drop_geometry
#' #' @importFrom rlang .data set_names
#' #' @export
#' priority_scorer_numeric <- function(dat_values, dat_ranks, col_filter = "column_name_raw", col_rank, col_idx = "idx"){
#'
#'   # Validate inputs
#'   chk::chk_data(dat_values)
#'   chk::chk_data(dat_ranks)
#'   chk::chk_string(col_filter)
#'   chk::chk_string(col_rank)
#'   chk::chk_string(col_idx)
#'
#'   # if idx column doesn't exist assign
#'   if (!col_idx %in% names(dat_values)) {
#'     dat_values <- dplyr::mutate(dat_values, !!col_idx := dplyr::row_number())
#'   } else {
#'     # if col_idx exists, check for duplicates and NAs
#'     vals <- dat_values[[col_idx]]
#'     if (any(is.na(vals)) || anyDuplicated(vals) > 0) {
#'       cli::cli_abort(c(
#'         "Column specified as {.arg col_idx} ({.val {col_idx}}) exists but is invalid.",
#'         "x" = "It contains {.val {sum(is.na(vals))}} missing and {.val {anyDuplicated(vals)}} duplicate values.",
#'         "i" = "Either correct the column or specify a new name for {.arg col_idx} to be generated."
#'       ))
#'     }
#'   }
#'
#'   # Prepare ranking row
#'   dat_ranks_row <- dplyr::filter(dat_ranks, .data[[col_filter]] == col_rank) |>
#'     dplyr::select(dplyr::contains("weight"), -dplyr::contains("notes")) |>
#'     dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
#'
#'
#'   # Reshape ranking data
#'   dat_ranks2 <- dat_ranks_row |>
#'     tidyr::pivot_longer(
#'       cols = dplyr::everything(),
#'       names_to = "name",
#'       values_to = "value"
#'     ) |>
#'     dplyr::mutate(name = stringr::str_remove(name, "weight_")) |>
#'     # separate off the category (ie. value vs score vs notes)
#'     tidyr::separate(name, into = c("category", "level"), sep = "_(?=[^_]+$)") |>
#'     tidyr::pivot_wider(names_from = category, values_from = value) |>
#'     dplyr::rename_with(~ paste0(col_rank, "_", .x), -level)
#'
#'   # Join data
#'   dat_out_prep <- dplyr::left_join(
#'     dplyr::mutate(dat_values, dplyr::across(dplyr::everything(), as.character)) |>
#'       sf::st_drop_geometry(),
#'     dat_ranks2 |>
#'       dplyr::select(-level),
#'     by = rlang::set_names(paste0(col_rank, "_value"), col_rank),
#'     na_matches = "never"
#'   )
#'
#'   # Select and convert scores to numeric
#'   dat_out <- dat_out_prep |>
#'     dplyr::select(
#'       # You need all_of() to tell dplyr: "Use the value of this string as the column name."
#'       dplyr::all_of(col_idx),
#'       dplyr::contains("score")
#'       ) |>
#'     # convert the score back to numeric so we can add it up later
#'     dplyr::mutate(dplyr::across(dplyr::matches("score"), as.numeric))
#'
#'   dat_out
#' }
#'
#'
#' # this one needs to detect if the string we have for our result matches one of the strings in one of the columns of the
#' # weights_value_{*} columns. If it does, then we need to get the score from the corresponding weight_score_{*} column
#' priority_scorer_string <- function(dat_values, dat_ranks, col_filter = "column_name_raw", col_rank, col_idx = "idx"){
#'
#'   # Validate inputs
#'   chk::chk_data(dat_values)
#'   chk::chk_data(dat_ranks)
#'   chk::chk_string(col_filter)
#'   chk::chk_string(col_rank)
#'   chk::chk_string(col_idx)
#'
#'   # if idx column doesn't exist assign
#'   if (!col_idx %in% names(dat_values)) {
#'     dat_values <- dplyr::mutate(dat_values, !!col_idx := dplyr::row_number())
#'   } else {
#'     # if col_idx exists, check for duplicates and NAs
#'     vals <- dat_values[[col_idx]]
#'     if (any(is.na(vals)) || anyDuplicated(vals) > 0) {
#'       cli::cli_abort(c(
#'         "Column specified as {.arg col_idx} ({.val {col_idx}}) exists but is invalid.",
#'         "x" = "It contains {.val {sum(is.na(vals))}} missing and {.val {anyDuplicated(vals)}} duplicate values.",
#'         "i" = "Either correct the column or specify a new name for {.arg col_idx} to be generated."
#'       ))
#'     }
#'   }
#'
#'   # Prepare ranking row
#'   dat_ranks_row <- dplyr::filter(dat_ranks, .data[[col_filter]] == col_rank) |>
#'     dplyr::select(dplyr::contains("weight"), -dplyr::contains("notes")) |>
#'     dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
#'
#'   # Reshape ranking data
#'   dat_ranks2 <- dat_ranks_row |>
#'     tidyr::pivot_longer(
#'       cols = dplyr::everything(),
#'       names_to = "name",
#'       values_to = "value"
#'     ) |>
#'     dplyr::mutate(name = stringr::str_remove(name, "weight_")) |>
#'     # separate off the category (ie. value vs score vs notes)
#'     tidyr::separate(name, into = c("category", "level"), sep = "_(?=[^_]+$)") |>
#'     tidyr::pivot_wider(names_from = category, values_from = value) |>
#'     dplyr::rename_with(~ paste0(col_rank, "_", .x), -level)
#'
#'   # so only this section differs from the numeric version. can prob join the two with if statement
#'   # this bit searches for our strings to see if they are present in the weight_value_{*} columns
#'   # then replaces the multiple strings with just the matched string so we can join
#'   # Could deal with NA values here somehow.  Not done yet
#'   dat_out_prep <- purrr::map_dfr(
#'     seq_len(nrow(dat_values)),
#'     function(i) {
#'       value <- dat_values[[i, col_rank]]
#'
#'       match_row <- dat_ranks2 |>
#'         dplyr::filter(
#'           stringr::str_detect(
#'             # then replace the multiple strings with just the matched on so we can join
#'             .data[[paste0(col_rank, "_value")]],
#'             value
#'           )
#'         )
#'
#'       dplyr::bind_cols(
#'         dat_values[i, ],
#'         match_row |>
#'           dplyr::select(paste0(col_rank, "_score"))
#'       )
#'     }
#'   ) |>
#'     sf::st_drop_geometry()
#'
#'   # Select and convert scores to numeric
#'   dat_out <- dat_out_prep |>
#'     dplyr::select(
#'       # You need all_of() to tell dplyr: "Use the value of this string as the column name."
#'       dplyr::all_of(col_idx),
#'       dplyr::contains("score")
#'     ) |>
#'     # convert the score back to numeric so we can add it up later
#'     dplyr::mutate(dplyr::across(dplyr::matches("score"), as.numeric))
#'
#'   dat_out
#' }

# priority_scorer ----------------------------------------------------------------------------------------------------
#' Priority Scoring
#'
#' Adds priority scoring columns to each row of a tibble based on numeric values and corresponding rank weights
#' stored in a separate tibble.  Example use case is applying scores to a spatial file of potential restoration
#' sites dynamically based on the values detailed in a ranking spreadsheet (after these tables are read into R)
#'
#' Joins a data frame of values to be scored with a data frame of ranking weights.
#' It matches values dynamically by category, extracts scores, and prepares an output with numeric
#' scores suitable for summarization.
#'
#' @param dat_values [data.frame] A data frame containing the numeric values to be scored. The name of the column
#' that contains the numeric scores must align with `col_rank` param.
#' @param dat_ranks [data.frame] A data frame containing the ranking weights and categories. Contains the columns
#' "weight_value_low" "weight_value_mod" "weight_value_high" "weight_score_low" "weight_score_mod" "weight_score_high"
#' @param col_rank [character] A single string specifying the ranking category to use for scoring. This string must
#' @param col_filter [character] A single string specifying the column in `dat_ranks` used to filter for the scoring category. Default is `'column_name_raw'`.
#' match the name of the column from `dat_values` that contains the numeric value that will recieve its coinciding score.  Must also match
#' the name of a parameter found in the `col_filter` column of `dat_ranks` (ex. a `weight_value_low
#' for `bulkley_falls_downstream` is 1 and and if that value of 1 is present in the `bulkley_falls_downstream` column of `dat_values`
#' then it will be joined with a cooinciding `weight_score_low` of 0 - from the `dat_ranks` dataframe)
#' @param col_idx [character] Optional. A single string specifying the column name to use as an identifier in the output.
#' If the column does not exist, row numbers will be used. Default is `'idx'`.
#'
#' @returns A [tibble][tibble::tibble] containing the identifier column and numeric score columns.
#'
#' @details
#' The function works by:
#' 1. Filtering `dat_ranks` for the desired ranking category.
#' 2. Reshaping and cleaning the ranking data.
#' 3. Joining `dat_values` with the cleaned ranks based on the dynamic value category.
#' 4. Extracting and converting score columns to numeric.
#'
#' Note: Input validation is performed using [chk::chk_*()] functions.
#'
#' @seealso
#' [dplyr::filter()], [dplyr::left_join()], [tidyr::pivot_longer()], [tidyr::pivot_wider()], [stringr::str_remove()]
#'
#' @importFrom dplyr filter select mutate across left_join contains rename_with all_of row_number matches
#' @importFrom tidyr pivot_longer separate pivot_wider
#' @importFrom stringr str_remove
#' @importFrom sf st_drop_geometry
#' @importFrom rlang .data set_names
#' @export
priority_scorer <- function(dat_values, dat_ranks, col_rank, col_filter = "source_column_name",  col_idx = "idx"){

  # Validate inputs
  chk::chk_data(dat_values)
  chk::chk_data(dat_ranks)
  chk::chk_string(col_filter)
  chk::chk_string(col_rank)
  chk::chk_string(col_idx)

  # if idx column doesn't exist assign
  if (!col_idx %in% names(dat_values)) {
    dat_values <- dplyr::mutate(dat_values, !!col_idx := dplyr::row_number())
  } else {
    # if col_idx exists, check for duplicates and NAs
    vals <- dat_values[[col_idx]]
    if (any(is.na(vals)) || anyDuplicated(vals) > 0) {
      cli::cli_abort(c(
        "Column specified as {.arg col_idx} ({.val {col_idx}}) exists but is invalid.",
        "x" = "It contains {.val {sum(is.na(vals))}} missing and {.val {anyDuplicated(vals)}} duplicate values.",
        "i" = "Either correct the column or specify a new name for {.arg col_idx} to be generated."
      ))
    }
  }

  # Prepare ranking row
  dat_ranks_row <- dplyr::filter(dat_ranks, .data[[col_filter]] == col_rank) |>
    dplyr::select(dplyr::contains("weight"), -dplyr::contains("notes")) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  # Reshape ranking data
  dat_ranks2 <- dat_ranks_row |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "name",
      values_to = "value"
    ) |>
    dplyr::mutate(name = stringr::str_remove(name, "weight_")) |>
    # separate off the category (ie. value vs score vs notes)
    tidyr::separate(name, into = c("category", "level"), sep = "_(?=[^_]+$)") |>
    tidyr::pivot_wider(names_from = category, values_from = value) |>
    dplyr::rename_with(~ paste0(col_rank, "_", .x), -level)

  # # check type: numeric vs string ---------------------------------------------------------------------------------------
  col_rank_value <- paste0(col_rank, "_value")

  # keep only non-NA values, test conversion
  vals <- dat_ranks2[[col_rank_value]][!is.na(dat_ranks2[[col_rank_value]])]
  col_rank_value_is_numeric <- all(!is.na(suppressWarnings(as.numeric(vals))))

  if (col_rank_value_is_numeric) {
    # Join data
    dat_out_prep <- dplyr::left_join(
      dat_values |>
        dplyr::mutate(
          dplyr::across(dplyr::all_of(col_rank), as.numeric)
        ) |>
        sf::st_drop_geometry(),
      dat_ranks2 |>
        dplyr::mutate(
          dplyr::across(dplyr::all_of(col_rank_value), as.numeric)
        ) |>
        dplyr::select(-level),
      by = rlang::set_names(col_rank_value, col_rank),
      na_matches = "never"
    )

  } else {
    # this bit searches for our strings to see if they are present in the weight_value_{*} columns
    # then replaces the multiple strings with just the matched string so we can join
    # Could deal with NA values here somehow.  Not done yet
    dat_out_prep <- purrr::map_dfr(
      seq_len(nrow(dat_values)),
      function(i) {
        value <- dat_values[[i, col_rank]]

        match_row <- dat_ranks2 |>
          dplyr::filter(
            stringr::str_detect(
              # then replace the multiple strings with just the matched one so we can join
              .data[[paste0(col_rank, "_value")]],
              value
            )
          )

        dplyr::bind_cols(
          dat_values[i, ],
          match_row |>
            dplyr::select(paste0(col_rank, "_score"))
        )
      }
    ) |>
      sf::st_drop_geometry()
  }

  #type end-----------------------------------------------------------------------------------------------------

  # Select and convert scores to numeric
  dat_out <- dat_out_prep |>
    dplyr::select(
      # You need all_of() to tell dplyr: "Use the value of this string as the column name."
      dplyr::all_of(col_idx),
      dplyr::contains("score")
    ) |>
    # convert the score back to numeric so we can add it up later
    dplyr::mutate(dplyr::across(dplyr::matches("score"), as.numeric))

  dat_out
}
