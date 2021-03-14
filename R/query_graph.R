


#' Create an Ego Network Given a Query Point
#'
#' @param bname The name of the business to search
#' @param qp The query point (qp), expressed as an ID
#' @param order the size of the ego network. Defaults to 20.
#'
#' @return
#' @export
#' @importFrom igraph make_ego_graph
#'
#' @examples
#' network <- get_ego(qp = "B7")
#'
#' bname <- "MML Properties Ltd"
#' get_ego(bname)
get_ego <- function(bname, qp = NULL, order = 20) {
  if (is.null(qp)) {
    # TODO: find based on non-exact business name
    i <- which(h2h::vbr$BusinessName == bname)
    j <- which(h2h::vbr$BusinessTradeName == bname)
    qp <- paste0("B", h2h::vbr$id[unique(i, j)])
  }
  egos <- make_ego_graph(h2h::g, order = order, nodes = qp)
  purrr::reduce(egos, igraph::union)
}

#' Plot a Temporal Ego Network of a Business' Licenses
#'
#' @param bname The business name to search
#'
#' @return
#' @export
#' @importFrom igraph diameter
#' @importFrom dplyr mutate left_join
#' @importFrom stringr str_glue
#' @importFrom visNetwork visNetwork toVisNetworkData
#'
#' @examples
#' bname <- "MML Properties Ltd"
#' viz_graph(bname)
viz_graph <- function(bname = "Gyoza Bar Ltd") {
  network <- get_ego(bname)

  if (length(igraph::V(network)) > 100) {
    stop("The graph is too large to visualize")
  }

  d <- igraph::diameter(network)

  nodes <- h2h::vbr %>%
    select(
      id, BusinessName, FOLDERYEAR, LicenceRSN,
      LicenceNumber, BusinessTradeName, Status
    ) %>%
    mutate(across(id, ~ paste0("B", .)))

  data <- toVisNetworkData(network)

  s <- function(x) if_else(is.na(x), "", x)
  data$nodes <- data$nodes[, 1, drop = FALSE] %>%
    left_join(nodes, by = "id") %>%
    mutate(
      label = str_glue("{s(LicenceNumber)}
                        {s(BusinessName)}
                        {s(BusinessTradeName)}"),
      group = BusinessName
    )
  # TODO: Add weights

  viz <- visNetwork(
    nodes = data$nodes,
    edges = data$edges,
    main = str_glue("Business history: {d} years"),
    height = "500px",
    width = "100%"
  )
  viz
}
