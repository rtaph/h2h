# LOAD, TRANSFORM, AND SAVE DATA



#' Read-in the City-of-Vancouver Business License Data and Save to Package
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
#' @export
#'
#' @examples
#' \dontrun{
#' data_vbr(save_to_pkg = FALSE)
#' }
data_vbr <- function(path = "data-raw/business-licences.csv",
                     save_to_pkg = TRUE) {
  colspec <- cols(
    FOLDERYEAR = col_double(),
    # LicenceRSN = col_double(),
    IssuedDate = col_datetime(format = ""),
    ExpiredDate = col_date(format = ""),
    # Unit = col_double(),
    # House = col_double(),
    Status = col_factor(),
    LicenceRevisionNumber = col_factor(),
    # Country  = col_factor(),
    Province = col_factor(),
    LocalArea = col_factor(),
    UnitType = col_factor(),
    FeePaid = col_double(),
    ExtractDate = col_datetime(format = ""),
    .default = col_character()
  )

  vbr_raw <- read_csv2(path, col_types = colspec)

  vbr <- vbr_raw %>%
    replace_na(list(Country = "MISS")) %>%
    mutate(lower = tolower(BusinessName)) %>%
    mutate(across(NumberofEmployees, as.numeric),
      lat = str_extract(Geom, "(?<=\\[)-?\\d+\\.\\d+(?=,)"),
      lon = str_extract(Geom, "(?<=, )-?\\d+\\.\\d+(?=\\])"),
      across(c(lon, lat), as.numeric)
    ) %>%
    rowid_to_column("id") %>%
    add_column(source = "B")

  if (save_to_pkg) {
    usethis::use_data(vbr, overwrite = TRUE)
  }
  invisible(vbr)
}


#' Build the Business Graph
#'
#' @param vrb The vancouver business registry dataset (saved in the package as
#'    `vbr`)
#' @param save_to_pkg Whether or not the graph should be saved to the package
#'    data directory. Defaults to TRUE.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' build_graph(save_to_pkg = FALSE)
#' }
build_graph <- function(vrb = h2h::data_vbr(), save_to_pkg = TRUE) {
  byyear <- split(vbr, paste0("Y", vbr$FOLDERYEAR))

  edges <- tibble(id = numeric(0), id2 = numeric(0), rule = numeric(0))

  # set order of joins
  byvars <- list(
    c("PostalCode", "BusinessName", "BusinessType", "lat"),
    c("PostalCode", "BusinessName", "BusinessType"),
    c("BusinessName", "BusinessTradeName", "BusinessType"),
    c("BusinessName", "lat", "lon"),
    c("BusinessTradeName", "lat", "lon"),
    c("BusinessName", "BusinessType"),
    c("BusinessName")
  )

  for (i in seq_along(names(byyear))[-1]) {
    A <- byyear[[i - 1]]
    B <- byyear[[i]]
    B <- rename(B, id2 = id)

    for (j in seq_along(byvars)) {
      v <- byvars[[j]]
      .A <- tidyr::drop_na(A[!A$id %in% edges$id, c("id", v)])
      .B <- tidyr::drop_na(B[!B$id2 %in% edges$id2, c("id2", v)])
      r1 <- inner_join(.A, .B, by = v, na_matches = "never")
      r1$rule <- j
      edges <- bind_rows(edges, select(r1, id, id2, rule))
    }
  }

  # TODO: fuzzy join on remaining, as well as block candidates

  # generate an igraph object and save
  links <- rename(edges, from = id, to = id2) %>%
    mutate(across(c(to, from), ~ paste0("B", .)))
  g <- igraph::graph_from_edgelist(as.matrix(links[, c("from", "to")]))
  # TODO: Add singletons
  if (save_to_pkg) {
    usethis::use_data(g, overwrite = TRUE)
  }

  invisible(g)
}
