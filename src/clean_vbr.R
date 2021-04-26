# LOAD, TRANSFORM, AND SAVE DATA

library(readr)
library(dplyr, warn.conflicts = FALSE)
library(stringr)



#' Read-in the combined City-of-Vancouver Business License &
#' Statistics Canada Related Companies Data and Save to Package
#'
#' This package takes existing CSV files and saves the as .Rda after data
#' cleaning.
#'
#' @param path A path to a csv file
#' @param save_to_pkg Should the data be saved as an Rda file in the data
#'   directory (as 'vbr')?
#'
#' @return The cleaned business registry data. Saved as `vbr` if save_to_pkg =
#'   TRUE.
#' @import readr dplyr tibble
#' @importFrom tidyr replace_na
#' @importFrom stringr str_extract
#' @export
#'
#' @examples
#' \dontrun{
#' data_vbr(save_to_pkg = FALSE)
#' }
data_vbr <- function(path = "data-raw/license_data.csv",
                     save_to_pkg = TRUE) {
  colspec <- cols(
    .default = col_character(),
    FOLDERYEAR = col_double(),
    LicenceRSN = col_double(),
    LicenceRevisionNumber = col_double(),
    IssuedDate = col_skip(), # col_date(format = ""),
    ExpiredDate = col_skip(), # format = ""),
    House = col_character(),
    NumberofEmployees = col_double(),
    FeePaid = col_double(),
    ExtractDate = col_skip(),
    perc_missing = col_double(),
    age = col_double(),
    BusinessType = col_factor(),
    BusinessTradeName = col_factor(),
    BusinessName = col_factor(),
    Status = col_factor(),
    UnitType = col_factor(),
    LocalArea = col_factor(),
    prov_cleaned = col_factor(),
    Country = col_factor(),
    Province = col_skip(),
    City = col_skip(),
    PID = col_double(),
    CCID = col_double()
  )

  vbr_raw <- read_csv(path, col_types = colspec)

  vbr <- vbr_raw %>%
    mutate(lower = tolower(BusinessName)) %>%
    #dplyr::filter(FOLDERYEAR>=10) %>%
    mutate(across(NumberofEmployees, as.numeric),
      lat = str_extract(Geom, "(?<=\\[)-?\\d+\\.\\d+(?=,)"),
      lon = str_extract(Geom, "(?<=, )-?\\d+\\.\\d+(?=\\])"),
      across(c(lon, lat), as.numeric)
    ) %>%
    select(-Geom)

  if (save_to_pkg) {
    usethis::use_data(vbr, overwrite = TRUE)
  }
  invisible(vbr)
}

# run
data_vbr()

