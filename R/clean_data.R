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
    City = col_skip()
  )

  vbr_raw <- read_csv(path, col_types = colspec)

  vbr <- vbr_raw %>%
    mutate(lower = tolower(BusinessName)) %>%
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

#' Extract Network Data for Statstcan Inter-Corporate Ownership
#'
#' @param path The path to the linked data.
#' @param path2 The path to the original statscan data.
#'
#' @return A list containing an edge list and a nodelist.
#' @export
data_statscan <- function(path = "data-processed/combined_data.csv",
                          path2 = "data-raw/linked_corp_data.csv") {

  # step1 : read-in the linked data
  sc_linked <- read_csv(path, guess_max = 40000)
  sc <- as_tibble(sc_linked) %>%
    select(id, PID, CCID, NAME) %>%
    mutate(across(c(PID, CCID), ~ paste0("C", .)))

  # Edgelist: COVL -> Statscan
  el1 <- select(sc, from = id, to = CCID)

  # Step 2: read-in the original data for the statstcan portion of the
  # network
  sc_raw <- read.csv(path2)
  sc_proc <- sc_raw %>%
    as_tibble() %>%
    select(CCID, PID, NAME) %>%
    mutate(across(c(PID, CCID), ~ paste0("C", .)))

  # Edgelist: Statscan -> Statscan
  # el2 <- sc_proc %>%
  #  select(from = CCID, to = PID) %>%
  #  filter(from %in% el1$to)

  # Nodelist: Statscan
  nl <- distinct(sc, id = CCID, BusinessName = NAME) %>%
    dplyr::filter(id %in% el1$to | id %in% el1$from)

  # combined edgelist
  # el <- bind_rows(el1, el2)
  el <- el1

  statscan <- list(el = el, nl = nl)
  statscan
}


#' Build the Business Graph
#'
#' @param vrb The Vancouver business registry dataset (saved in the package as
#'    `vbr`)
#' @param save_to_pkg Whether or not the graph should be saved to the package
#'    data directory. Defaults to TRUE.
#'
#' @return
#' @importFrom igraph V E vertex_attr graph_from_edgelist edge_attr
#' @export
#'
#' @examples
#' \dontrun{
#' build_graph(save_to_pkg = FALSE)
#' }
build_graph <- function(vrb = data_vbr(),
                        statscan = data_statscan(),
                        save_to_pkg = TRUE) {
  byyear <- split(vbr, paste0("Y", vbr$FOLDERYEAR))

  edges <- tibble(
    id = character(0),
    id2 = character(0),
    rule = numeric(0)
  )

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

  nodes <- h2h::vbr %>%
    select(
      id, BusinessName, FOLDERYEAR, LicenceRSN,
      LicenceNumber, BusinessTradeName, Status
    ) %>%
    mutate(across(where(is.factor), as.character)) %>%
    bind_rows(statscan$nl)


  # generate an igraph object and save
  links <- rename(edges, from = id, to = id2) %>%
    # mutate(across(c(to, from), ~ paste0("B", .))) %>%
    mutate(weight = max(rule) - rule + 2) %>%
    bind_rows(add_column(statscan$el, weight = 1L))

  # make the igraph object
  g <- graph_from_edgelist(as.matrix(links[, c("from", "to")]))
  igraph::edge.attributes(g) <- list(weight = links$weight)

  # Add names
  i <- match(V(g)$name, nodes$id)
  igraph::vertex_attr(g, "BusinessName") <- nodes[i, ]$BusinessName
  igraph::vertex_attr(g, "LicenceNumber") <- nodes[i, ]$LicenceNumber
  igraph::vertex_attr(g, "BusinessTradeName") <- nodes[i, ]$BusinessTradeName

  # TODO: Add singletons
  if (save_to_pkg) {
    usethis::use_data(g, overwrite = TRUE)
  }

  invisible(g)
}
