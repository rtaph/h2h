



#' Compute a Risk Score for a Company
#'
#' @param bname The name of a business.
#'
#' @return
.profile_risk <- function(bname) {

  network <- get_ego(bname)
  nodes <- as_tibble(network)

  metrics <- list()


  # missing
  missing_count <- nodes %>%
    pull(perc_missing) %>%
    mean(na.rm = TRUE)
  metrics$missingdata <- missing_count >= 30

  # Age (number of licenses in the network)
  age_range <- nodes %>%
    summarise(age_range = n_distinct(year, na.rm = TRUE)) %>%
    pull(age_range)
  metrics$recent <- age_range < 4

  # Are they based in Canada or abroad?
  provinces <- nodes %>%
    summarise(provinces = unique(prov_cleaned, na.rm = TRUE)) %>%
    pull(provinces)
  metrics$foreign <- any(provinces == "Other", na.rm = TRUE)

  # Are any inactive?
  closed_status <- c("Gone Out of Business", "Cancelled", "Inactive")
  closed_in_last_year <- nodes %>%
    filter(year == max(year, na.rm = TRUE)) %>%
    mutate(closed = Status %in% closed_status,
           closed = factor(closed, levels = c(TRUE, FALSE))) %>%
    group_by(closed, .drop = FALSE) %>%
    count() %>%
    ungroup %>%
    mutate(p = n / sum(n)) %>%
    filter(closed == "TRUE")
  metrics$status <- closed_in_last_year$p == 1

  metrics
}

#' Compute a Risk Score for a Company
#'
#' @inheritParams .profile_risk
#'
#' @examples
#'
#' bname <- "MML Properties Ltd"
#' profile_risk(bname)
profile_risk <- memoise::memoise(
  f = .profile_risk,
  cache = cachem::cache_disk(rappdirs::user_cache_dir("h2h"))
)


#' Compute Risk
#'
#' @param bname The name of a business.
#'
#' @return
#' @export
risk_missingdata <- function(bname) {
  profile_risk(bname)$missingdata
}

#' Compute Risk
#'
#' @param bname The name of a business.
#'
#' @return
#' @export
risk_recent <- function(bname) {
  profile_risk(bname)$recent
}

#' Compute Risk
#'
#' @param bname The name of a business.
#'
#' @return
#' @export
risk_foreign <- function(bname) {
  if_else(profile_risk(bname)$foreign, "display", "block")
}

#' Compute Risk
#'
#' @param bname The name of a business.
#'
#' @return
#' @export
risk_status <- function(bname) {
  profile_risk(bname)$status
}




#' Compute a Risk Score for a Company
#'
#' @param bname The name of a business.
#'
#' @return
#' @export
#'
#' @examples
#'
#' bname <- "MML Properties Ltd"
#' profile_risk(bname)
calc_risk <- function(bname) {

  metrics <- profile_risk(bname)
  sum(unlist(metrics))
}




