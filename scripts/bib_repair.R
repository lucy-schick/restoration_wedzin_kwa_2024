
library(rbbt)
library(xciter)
library(ngr)




keys = rbbt::bbt_detect_citations(
  list.files(
    pattern = "*.Rmd")
)

path_bib <- system.file("extdata", "NewGraphEnvironment.bib", package = "xciter")


keys_missing <- xct_bib_keys_missing(
  path_bib = path_bib,
  citations = rbbt::bbt_detect_citations(
    list.files(
      pattern = "*.Rmd")
  )
)

keys_matched <- xct_keys_guess_match(
  keys_missing,
  keys_bib = xct_bib_keys_extract(path_bib),
  stringdist_threshold = 18,
  no_match_rows_include = TRUE
  ) |>
dplyr::arrange(key_missing)


file_list <- fs::dir_ls(glob = "*.Rmd")

purrr::map2(
  .x = keys_matched$key_missing,
  .y = keys_matched$key_missing_guess_match,
  ~ ngr::ngr_str_replace_in_files(text_current = .x, text_replace = .y, files = file_list)
)
