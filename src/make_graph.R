# make a network graph

library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
library(igraph, warn.conflicts = FALSE)

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
    select(CCID, PID, NAME, foreign_ctl = CCTL) %>%
    mutate(across(c(PID, CCID), list(C = ~ paste0("C", .)))) 

  # Edgelist: Statscan -> Statscan
  # el2 <- sc_proc %>%
  #  select(from = CCID, to = PID) %>%
  #  filter(from %in% el1$to)

  # Nodelist: Statscan
  nl <- distinct(sc_proc, id = CCID_C, BusinessName = NAME,
                 foreign_ctl) %>%
    dplyr::filter(id %in% el1$to | id %in% el1$from)%>%
    left_join(distinct(sc_proc, id=CCID_C, CCID, PID))

  # combined edgelist
  # el <- bind_rows(el1, el2)
  el <- el1

  statscan <- list(el = el, nl = nl)
  statscan
}


#' Phonetic fingerprinting
#'
#' @param x A string
#' @param algorithm A fingerprinting algorithm from the phonics package.
#'
#' @return
#' @export
#'
#' @examples
phonetic <- function(x, algorithm = phonics::metaphone) {
  x %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[[:punct:]]", " ") %>%
    stringr::str_squish() %>%
    stringi::stri_split_fixed(" ") %>%
    map(algorithm) %>%
    map(sort) %>%
    map_chr(paste, collapse = "-")
}



#' Build the Business Graph
#'
#' @param vbr_file A scalar character nameing the Rda file with the Vancouver
#'    business registry dataset.
#' @param save_to_pkg Whether or not the graph should be saved to the package
#'    data directory. Defaults to TRUE.
#'
#' @return
#' @importFrom igraph V E vertex_attr graph_from_edgelist edge_attr
#' @importFrom dplyr if_else case_when
#' @export
#'
#' @examples
#' \dontrun{
#' build_graph(save_to_pkg = FALSE)
#' }
build_graph <- function(vbr_file = "data/vbr.rda",
                        statscan = data_statscan(),
                        save_to_pkg = TRUE) {
  if (!fs::file_exists(vbr_file)) {
    rlang::abort("data/vbr.rda does not exists. Please run 'make all' to
                 build it.")
  } else {
    load(vbr_file)
  }

  # add fingerprints
  vbr$fp1 <- forcats::fct_relabel(vbr$BusinessName, phonetic)
  vbr$fp2 <- forcats::fct_relabel(vbr$BusinessTradeName, phonetic)

  vbr$year <- if_else(vbr$FOLDERYEAR > 90, vbr$FOLDERYEAR + 1900, vbr$FOLDERYEAR + 2000)
  vbr <- arrange(vbr, year)
  byyear <- split(vbr, paste0("Y", vbr$year))

  edges <- tibble(
    id = character(0),
    id2 = character(0),
    rule = numeric(0)
  )

  # set order of joins
  byvars <- list(
    R01 = c("PostalCode", "BusinessName", "BusinessType", "BusinessSubType"),
    R02 = c("PostalCode", "BusinessName", "BusinessType"),
    R03 = c("BusinessName", "BusinessTradeName", "BusinessType"),
    R04 = c("BusinessName", "lat", "lon"),

    R05 = c("BusinessTradeName", "lat", "lon"),

    R06 = c("BusinessName", "BusinessType"),
    R07 = c("BusinessName"),

    R08 = c("BusinessName" = "BusinessTradeName"),
    R08 = c("BusinessTradeName" = "BusinessName"),

    R09 = c("PostalCode", "fp1"),
    R10 = c("PostalCode", "fp2"),

    R11 = c("PostalCode", "fp2" = "fp1"),
    R11 = c("PostalCode", "fp1" = "fp2")
  )

  for (i in seq_along(names(byyear))[-1]) {
    A <- byyear[[i - 1]]
    B <- byyear[[i]]
    B <- rename(B, id2 = id)

    for (j in seq_along(byvars)) {
      v <- byvars[[j]]
      v <- set_names(v)
      .A <- tidyr::drop_na(A[!A$id %in% edges$id, c("id", names(v))])
      .B <- tidyr::drop_na(B[!B$id2 %in% edges$id2, c("id2", v)])
      r1 <- inner_join(.A, .B, by = v, na_matches = "never")
      r1$rule <- j
      edges <- bind_rows(edges, select(r1, id, id2, rule))
    }
  }

  nodes <- vbr %>%
    select(id, BusinessName, FOLDERYEAR, everything()) %>%
    mutate(year = dplyr::case_when(
      FOLDERYEAR > 90 ~ FOLDERYEAR + 1900,
      TRUE ~ FOLDERYEAR + 2000
    )) %>%
    mutate(across(where(is.factor), as.character)) %>%
    bind_rows(statscan$nl)


  # generate an igraph object and save
  links <- rename(edges, from = id, to = id2) %>%
    mutate(weight = as.numeric(stringr::str_remove(rule, "R"))) %>%
    mutate(across(weight, ~(max(.) - . + 1) / max(.))) %>%
    bind_rows(mutate(statscan$el, weight = 1L, rule = 12))

  # make the igraph object
  g <- igraph::graph_from_edgelist(as.matrix(links[, c("from", "to")]))
  igraph::edge.attributes(g) <- list(weight = links$weight,
                                     rule = paste0("R", links$rule))

  # Add node attributes
  i <- match(igraph::V(g)$name, nodes$id)
  variables <- c("BusinessName", "LicenceNumber", "BusinessTradeName",
                 "BusinessType", "BusinessSubType", "Status", "year",
                 "NumberofEmployees", "foreign_ctl", "perc_missing",
                 "PostalCode", "LocalArea", "Country", "lat", "lon",
                 "prov_cleaned", "CCID", "PID")
  for (x in variables) {
    igraph::vertex_attr(g, x) <- nodes[i, ][[x]]
  }

  # convert to tidygraph
  g <- tidygraph::as_tbl_graph(g)

  if (save_to_pkg) {
    usethis::use_data(g, overwrite = TRUE)
    devtools::document()
  }

  invisible(g)
}

coi_data <- arrow::read_feather(here::here("data-processed/filtered_hierarchy_data.feather"))
usethis::use_data(coi_data, overwrite = TRUE)
build_graph()
