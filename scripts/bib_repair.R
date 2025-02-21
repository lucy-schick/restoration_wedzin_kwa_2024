
library(rbbt)
library(xciter)
library(ngr)


keys = rbbt::bbt_detect_citations(
  list.files(
    pattern = "*.Rmd")
)

path_bib <- system.file("extdata", "NewGraphEnvironment.bib", package = "xciter")


keys_missing <- xciter::xct_bib_keys_missing(
  path_bib = path_bib,
  citations = rbbt::bbt_detect_citations(
    list.files(
      pattern = "*.Rmd")
  )
)

# print to console so we can copy paste into xciter data
# cat("keys_missing <- c(\n",
#     paste0('"', keys_missing, '"', collapse = ",\n "),
#     "\n)\n", sep = "")

# we have mismatches that we need to fix custom!!!!!!!!!!!! We see the by very CARFEFUL comparison between
# what is proposed and what they are meant to be.
# we built a xref table in xciter to deal with these particular cases.  We will resolve those first

path_xref <- system.file("extdata", "xct_xref_citations_match.csv", package = "xciter")

xct_xref_citations_match <- readr::read_csv(path_xref)

# we are looking here
file_list <- fs::dir_ls(glob = "*.Rmd")

# replace them
purrr::map2(
  .x = xct_xref_citations_match$key_match,
  .y = xct_xref_citations_match$key_match,
  ~ ngr::ngr_str_replace_in_files(text_current = .x, text_replace = .y, files = file_list)
)


keys_matched <- xciter::xct_keys_guess_match(
  keys_missing,
  keys_bib = xciter::xct_bib_keys_extract(path_bib),
  # stringdist_method = "osa",
  stringdist_threshold = 25,
  no_match_rows_include = TRUE
) |>
  dplyr::arrange(key_missing)
  # dplyr::filter(!is.na(key_missing_guess_match))





purrr::map2(
  .x = keys_matched$key_missing,
  .y = keys_matched$key_missing_guess_match,
  ~ ngr::ngr_str_replace_in_files(text_current = .x, text_replace = .y, files = file_list)
)


