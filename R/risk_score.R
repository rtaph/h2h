



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
profile_risk <- function(bname) {

  # TODO: find based on non-exact business name
  i <- match(bname, h2h::vbr$BusinessName)
  j <- match(bname, h2h::vbr$BusinessTradeName)
  k <- unique(i, j)[1]
  qp <- h2h::vbr$id[k]


  metrics <- list()

  # missing
  metrics$missingdata <- vbr$perc_missing[k] < 30

  # Age (number of licenses in the network)
  network <- get_ego(bname)
  igraph::edge_attr(network, "weight") <- 1
  metrics$recent <- igraph::diameter(network) < 4

  # Are they based in Canada or abroad?
  metrics$foreign <- vbr$prov_cleaned[k] == "Other"

  # Do they have employees?
  #metrics$has_employees <- vbr$NumberofEmployees[k] == 0

  # Are they based in Canada or abroad?
  x <- c("Gone Out of Business", "Cancelled", "Inactive")
  metrics$status <- vbr$Status[k] %in% x

  metrics
}


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




