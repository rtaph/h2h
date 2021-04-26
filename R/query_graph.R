#' Create an Ego Network Given a Query Point
#'
#' @param bname The name of the business to search
#' @param qp The query point (qp), expressed as an ID
#' @param order the size of the ego network. Defaults to 20.
#' @param mindist the minimum weight to be considered in the ego network.
#'
#' @return
#' @export
#' @importFrom stringr str_detect
#' @import tidygraph
#' @importFrom igraph make_ego_graph ego
#' @importFrom tidygraph activate
#'
#' @examples
#' network <- get_ego(qp = "B7")
#'
#' bname <- "MML Properties Ltd"
#' bname <- "Gyoza Bar Ltd"
#' get_ego(bname)
get_ego <- function(bname, regex = TRUE, ignore_case = TRUE,
                    order = 20, mindist = 1) {

  if (inherits(bname, "pattern")) {
    pattern <- bname
  } else if (regex) {
    pattern <- stringr::regex(bname, ignore_case = ignore_case)
  } else {
    pattern <- stringr::fixed(bname, ignore_case = TRUE)
  }

  # determine which nodes match the business name
  qp <- h2h::g %N>%
    filter(str_detect(BusinessName, pattern) |
             str_detect(BusinessTradeName, pattern)) %>%
    pull(name)

  # expand the set of nodes to include the neighbourhood
  egos <- ego_mem(g, order=order, nodes = qp, mindist = 0)

  # return a graph of the ego networks
  h2h::g %>%
    activate(nodes) %>%
    filter(name %in% unique(names(unlist(egos))))

}

ego_mem <- memoise::memoise(ego)



#' Plot a Temporal Ego Network of a Business' Licenses
#'
#' @param bname The business name to search
#'
#' @return
#' @export
#' @import igraph
#' @importFrom igraph diameter
#' @importFrom dplyr mutate left_join
#' @importFrom stringr str_glue
#' @importFrom visNetwork visNetwork toVisNetworkData visOptions
#'
#' @examples
#' # small networks
#' viz_graph("Gyoza Bar")
#' viz_graph("MML Properties Ltd")
#' viz_graph("Selarc Developments Ltd")
#' viz_graph("Westfair Foods Ltd")
#'
#' # Large network
#' viz_graph("Lawson Lundell LLP", rule_filters = "R5")
viz_graph <- function(bname = "Gyoza Bar", regex = TRUE, ignore_case = TRUE,
                      rule_filters = character(0), max_size = 2500) {
  network <- get_ego(bname, regex = regex, ignore_case = ignore_case)

  if (length(igraph::V(network)) > max_size) {
    warning("The graph is too large to visualize. Prunning back the order.")
    for (k in seq(19, 1)) {
      g <-  get_ego(bname, order = k)
      if (length(igraph::V(network)) <= max_size) {
        break()
      }
    }
  }

  # filter out edges
  network <- network %>%
    activate(edges) %>%
    filter(!rule %in% rule_filters)

  data <- toVisNetworkData(network)
  data$edges$value <- data$edges$weight
  data$edges$label <- data$edges$rule
  data$edges$arrows <- "to"
  s <- function(x) dplyr::if_else(is.na(x), "", x)
  data$nodes <- data$nodes %>%
    arrange(year, BusinessName) %>%
    mutate(
      label = str_glue("{year}
                        {s(BusinessName)}
                        {s(BusinessTradeName)}"),
      group = BusinessName,
      #shape = dplyr::case_when(Status %in% c("Issued", "Pending") ~ "dot",
      #                  TRUE ~ "text"),
      color = dplyr::case_when(!Status %in% c("Issued", "Pending") ~ "lightgrey",
                        TRUE ~ NA_character_),
      level = year,
      x = as.integer(forcats::fct_inorder(BusinessName)))

  d <- diff(range(data$nodes$year, na.rm = TRUE)) + 1L

  viz <- visNetwork(
    nodes = data$nodes,
    edges = data$edges,
    main = str_glue("Business history: {d + 1} years"),
    height = "600px",
    width = "100%"
  ) %>%
    visNetwork::visOptions(highlightNearest = TRUE) %>%
    visNetwork::visOptions(selectedBy = list(variable = "BusinessName",
                                             multiple = T)) %>%
    visNetwork::visHierarchicalLayout(direction = "LR",
                                      treeSpacing = 10,
                                      nodeSpacing = 5) %>%
    visNetwork::visPhysics(stabilization = FALSE)

  # reduce computational complexity of displaying if graph is large
  if (igraph::gsize(network) > 1500) {
    viz <- viz %>%
      visNetwork::visIgraphLayout(physics = FALSE, smooth = FALSE)
  }
  viz
}




#' Plot a Graph Using VisNetwork
#'
#' @param network The igraph object
#'
#' @return
#' @export
#' @import igraph
#' @importFrom igraph diameter
#' @importFrom dplyr mutate left_join
#' @importFrom stringr str_glue
#' @importFrom visNetwork visNetwork toVisNetworkData visOptions
#'
#' @examples
#' network <- filter(g, BusinessName == "Gyoza Bar Ltd")
#' viz(network)
viz <- function(network) {

  data <- toVisNetworkData(network)
  data$edges$value <- data$edges$weight
  data$edges$label <- data$edges$rule
  data$edges$arrows <- "to"

  s <- function(x) dplyr::if_else(is.na(x), "", x)
  data$nodes <- data$nodes %>%
    arrange(BusinessName) %>%
    mutate(
      label = str_glue("{year}
                        {s(BusinessName)}
                        {s(BusinessTradeName)}"),
      group = BusinessName,
      shape = case_when(Status %in% c("Issued", "Pending") ~ "dot",
                        TRUE ~ "text"),
      color = dplyr::case_when(!Status %in% c("Issued", "Pending") ~ "lightgrey",
                        TRUE ~ NA_character_),
      level = year,
      x = as.integer(forcats::fct_inorder(BusinessName)))

  d <- diff(range(data$nodes$year, na.rm = TRUE)) + 1L

  viz <- visNetwork(
    nodes = data$nodes,
    edges = data$edges,
    main = str_glue("Business history: {d + 1} years"),
    height = "500px",
    width = "100%"
  ) %>%
    visNetwork::visOptions(highlightNearest = TRUE) %>%
    visNetwork::visOptions(selectedBy = list(variable = "BusinessName", multiple = T)) %>%
    visNetwork::visHierarchicalLayout(direction = "LR",
                                      treeSpacing = 10, nodeSpacing = 5)
  viz
}

