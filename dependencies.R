packages <- c(

  'dplyr',
  'tidyr',
  'readxl',
  'shiny',
  'leaflet',
  'shinyjs',
  'sf',
  'reactable',
  'stringr',
  'viridis',
  'wesanderson',
  'purrr',
  'bslib',
  'bsicons',
  'htmltools',
  'openxlsx',
  'remotes',
  'stringr',
  'shinyjs',
  'ggplot2',
  '')
options(Ncpus = -1)
for (pkg in packages) {
  if (pkg == '') {
    next
  }
  install.packages(pkg)
  if ( ! library(pkg, character.only=TRUE, logical.return=TRUE) ) {
    quit(status=1, save='no')
  }
}

packages_dev <- c(
  'NIVA-Denmark/ambiR',
  '')
for (pkg_dev in packages_dev) {
  if (pkg_dev == '') {
    next
  }
  remotes::install_github(pkg_dev, force=T)
  pkg_dev <- substr(pkg_dev,1+regexpr("/",pkg_dev)[1],nchar(pkg_dev))
  if ( ! library(pkg_dev, character.only=TRUE, logical.return=TRUE) ) {
    quit(status=1, save='no')
  }
}
