# install.packages('pak')

pkgs_cran <- c(
  'tidyverse',
  'knitr',
  'bookdown',
  'rmarkdown',
  'pagedown',
  'readwritesqlite',
  'RPostgres',
  'sf',
  'tidyhydat',
  'fasstr',
  'tidyhydat',
  'ggdark',
  'fishbc'
)

pkgs_gh <- c(
  "newgraphenvironment/fpr",
  "haozhu233/kableExtra"
)

pkgs_all <- c(pkgs_cran,
              pkgs_gh)


# install or upgrade all the packages with pak
lapply(pkgs_all,
       pak::pkg_install, ask = FALSE)

# load all the packages
pkgs_ld <- c(pkgs_cran,
             basename(pkgs_gh))

lapply(pkgs_ld,
       require,
       character.only = TRUE)
