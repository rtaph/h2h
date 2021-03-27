


#' Create an Ego Network Given a Query Point
#'
#' @param bname The name of the business to search
#' @param qp The query point (qp), expressed as an ID
#' @param order the size of the ego network. Defaults to 20.
#' @param mindist the minimum weight to be considered in the ego network.
#'
#' @return
#' @export
#' @importFrom igraph make_ego_graph
#' @import igraph
#'
#' @examples
#' network <- get_ego(qp = "B7")
#'
#' bname <- "MML Properties Ltd"
#' get_ego(bname)
get_ego <- function(bname, qp = NULL, order = 20, mindist = 1) {
  if (is.null(qp)) {
    # TODO: find based on non-exact business name
    i <- match(bname, h2h::vbr$BusinessName)
    j <- match(bname, h2h::vbr$BusinessTradeName)
    qp <- h2h::vbr$id[unique(i, j)]
  }
  egos <- make_ego_graph(h2h::g, order = order, nodes = qp, mindist = mindist)
  gnew <- purrr::reduce(egos, igraph::union)
  va <- vertex_attr(g)
  i <- match(V(gnew)$name, va$name)
  igraph::vertex_attr(gnew, "BusinessName") <- va$BusinessName[i]
  igraph::vertex_attr(gnew, "LicenceNumber") <- va$LicenceNumber[i]
  igraph::vertex_attr(gnew, "va$BusinessTradeName") <- va$BusinessTradeName[i]
  gnew
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
#' bname <- "Selarc Developments Ltd"
#' viz_graph(bname)
viz_graph <- function(bname = "Gyoza Bar Ltd") {
  network <- get_ego(bname)

  if (length(igraph::V(network)) > 200) {
    stop("The graph is too large to visualize")
  }
  igraph::edge_attr(network, "weight") <- 1
  d <- igraph::diameter(network)

  data <- toVisNetworkData(network)
  s <- function(x) if_else(is.na(x), "", x)
  data$nodes <- data$nodes %>%
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
    main = str_glue("Business history: {d + 1} years"),
    height = "500px",
    width = "100%"
  )
  viz
}
