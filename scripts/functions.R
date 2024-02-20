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
fpr_dbq_clip_local <- function(
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
# @param station String (quoted) number of station
# @param pane_hydat Boolean TRUE if you want a pane layout of all hydrographs
# @param single_hydat Boolean TRUE if you want a single hydrograph with mean flows

fpr_create_hydrograph_local <- function(
    station,
    pane_hydat = TRUE,
    single_hydat = TRUE,
    start_year = NULL,
    end_year = NULL){


  flow_raw <- tidyhydat::hy_daily_flows(station)

  if(is.null(start_year)){
    start_year <- flow_raw$Date %>% min() %>% lubridate::year()
  }

  if(is.null(end_year)){
    end_year <- flow_raw$Date %>% max() %>% lubridate::year()
  }

  tidyhat_info <- search_stn_number(station)

  ##build caption for the figure
  caption_info <- mutate(tidyhat_info, title_stats = paste0(stringr::str_to_title(STATION_NAME),
                                                            " (Station #",STATION_NUMBER," - Lat " ,round(LATITUDE,6),
                                                            " Lon ",round(LONGITUDE,6), "). Available daily discharge data from ", start_year,
                                                            # FIRST_YEAR, ##removed the default here
                                                            " to ",end_year, "."))

  hydrograph1_stats_caption <- caption_info$title_stats
  #to.string(hydrograph1_stats_caption)


  if (pane_hydat == TRUE){
    #Create pane of hydrographs with "Mean", "Minimum", "Maximum", and "Standard Deviation" flows
    hydrograph_stats_print <- fasstr::plot_data_screening(station_number = station, start_year = start_year,
                                                          include_stats = c("Mean", "Minimum", "Maximum", "Standard Deviation"),
                                                          plot_availability = FALSE)[["Data_Screening"]] + ggdark::dark_theme_bw() ##first version is not dark
    hydrograph_stats_print

    #Save hydrograph pane
    ggsave(plot = hydrograph_stats_print, file=paste0("fig/hydrology_stats_", station, ".png"),
           h=3.4, w=5.11, units="in", dpi=300)
  }


  if (single_hydat == TRUE){
    # Create single hydrograph with mean flows from date range
    flow <- flow_raw %>%
      mutate(day_of_year = yday(Date)) %>%
      group_by(day_of_year) %>%
      summarise(daily_ave = mean(Value, na.rm=TRUE),
                daily_sd = sd(Value, na.rm = TRUE),
                max = max(Value, na.rm = TRUE),
                min = min(Value, na.rm = TRUE)) %>%
      # q025 = quantile(Value, probs = 0.025),
      # q975 = quantile(Value, probs = 0.975)) %>%
      mutate(Date = as.Date(day_of_year, origin = "2015-12-31"))

    plot <- ggplot()+
      geom_ribbon(data = flow, aes(x = Date, ymax = max,
                                   ymin = min),
                  alpha = 0.3, linetype = 1)+
      # geom_ribbon(data = flow_UDR, aes(x = Date, ymax = daily_ave + 2 * daily_sd,
      #                                  ymin = daily_ave - 2 * daily_sd),
      #             alpha = 0.4, linetype = 1)+
      # geom_ribbon(data = flow_UDR, aes(x = Date, ymax = q975,
      #                                  ymin = q025),
      #             alpha = 0.3, linetype = 1)+

      scale_x_date(date_labels = "%b", date_breaks = "2 month") +
      labs(x = NULL, y = expression(paste("Mean Daily Discharge (", m^3, "/s)", sep="")))+
      ggdark::dark_theme_bw() +
      # ylim(0,600) +
      # theme(axis.text.y=element_blank())+
      # scale_y_continuous() +
      geom_line(data = flow, aes(x = Date, y = daily_ave),
                linetype = 1, linewidth = 0.7) +
      scale_colour_manual(values = c("grey10", "red"))
    # coord_cartesian(ylim = c(0, 600))
    plot

    ggsave(plot = plot, file=paste0("./fig/hydrograph_", station, ".png"),
           h=3.4, w=5.11, units="in", dpi=300)
  }

  return(hydrograph1_stats_caption)


}

