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


# make a function for downloading files straight from skt ckan
fetch_package <- function(package_nm = NULL,
                          ckan_info = data_deets,
                          store = "disk",
                          path_stub = "data/skt/",
                          csv_output = TRUE) {
  info <- ckan_info %>%
    filter(package_name == package_nm)

  urls <- info %>%
    pull(url) %>%
  # to avoid dl errors we need to remove the NAs as well as those files that end without a file extension at the end (ex. .com/ and *123)
  na.omit() %>%
  .[str_detect(., ".*\\.[a-zA-Z0-9]+$")]

  # create the directory if it doesn't exist
  dir.create(paste0(path_stub, package_nm))

  walk(.x = urls,
       .f = ~ckan_fetch(.x, store = store, path = paste0(path_stub, package_nm, "/", basename(.x))))

  # if csv_output = TRUE burn out a little csv file of the information about everything that is downloaded
  if (csv_output) {
    info %>%
      arrange(basename(url)) %>%
      write_csv(paste0(path_stub, package_nm, "/001_pkg_info_", package_nm, ".csv"))
  }
}

