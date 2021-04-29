
#' Generate a table of related company from stats Canada
#'
#' @param input_value str indicating Business Name
#'
#' @return
#'
#' @examples
#' input_value="Gyoza Bar Ltd"
#' make_related_co_table(input_value)
#'
make_related_co_table <- function(input_value) {
  input_value <- closest_business_name(input_value)
  network <- get_ego(input_value)
  selected_PID <- network %N>%
    as_tibble() %>%
    pull(PID) %>%
    unique()

  if (all(is.na(selected_PID))) {
    # if no related companies found, return empty list.
    data <- list()
  }
  else {
    data <- coi_data %>%
      filter(PID == selected_PID[1] & LEVEL > 0) %>%
      arrange(LEVEL) %>%
      dashTable::df_to_list()
  }
  list(data)
}



#' Qualifies most appropriate business type of the company
#'
#' @param input_value str indicating Business Name
#'
#' @return
#'
#' @examples
#' input_value="Gyoza Bar Ltd"
#' make_co_type(input_value)
#'
make_co_type <- function(input_value) {
  input_value <- closest_business_name(input_value)
  network <- get_ego(input_value)
  selected_PID <- network %N>%
    as_tibble() %>%
    arrange(desc(year)) %>%
    slice(1) %>%
    select(BusinessType) %>%
    dashTable::df_to_list()
  columns <- c("BusinessType") %>% purrr::map(function(col) list(name = "Primary Business Type", id = col))
  list(selected_PID, columns)


  # data <- vbr %>%
  #   filter((BusinessName == input_value)) %>%
  #   arrange(desc(FOLDERYEAR)) %>%
  #   top_n(n = 1, wt = FOLDERYEAR) %>%
  #   select(BusinessType) %>%
  #   unique() %>%
  #   dashTable::df_to_list()
  # columns <- c("BusinessType") %>% purrr::map(function(col) list(name = "Primary Business Type", id = col))
  # list(data, columns)
}

# plot number of employees for industry on "Business Details" tab
# app$callback(
#   output("num_emp_plot", "figure"),
#   list(input("input_bname", "value")),


#' Title Plotting the number of employees per year
#'
#' @param input_value str indicating Business Name
#'
#' @return
#'
#' @import ggplot2
#'
#' @examples
#' input_value="Gyoza Bar Ltd"
#' num_emp_plot(input_value)
#'
num_emp_plot <- function(input_value) {
  input_value <- closest_business_name(input_value)
  network <- get_ego(input_value)
  emp_plot <- network %N>%
    as_tibble() %>%
    distinct(year, LicenceNumber, NumberofEmployees)



  # emp_plot <- vbr %>%
  #   mutate(FOLDERYEAR = formatC(vbr$FOLDERYEAR, width = 2, format = "d", flag = "0")) %>%
  #   filter((BusinessName == input_value) & (FOLDERYEAR <= format(Sys.Date(), "%y"))) %>%
  #   group_by(FOLDERYEAR, LicenceNumber) %>%
  #   summarise(NumberofEmployees = mean(NumberofEmployees))

  # if no data available
  if (emp_plot %>% tidyr::drop_na() %>% nrow() < 1) {
    emp_plot <- ggplot() +
      geom_text() +
      theme_void() +
      annotate("text", label = "No data available.", x = 2, y = 15, size = 8) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
    return(plotly::ggplotly(emp_plot))
  }
  # if data available
  else {
    emp_plot <- emp_plot %>% ggplot(aes(
      x = year,
      y = NumberofEmployees
    )) +
      stat_summary(fun = "sum", geom = "bar", fill = "royalblue4") +
      labs(
        y = "Number of Employees Reported",
        x = "Year"
      ) +
      scale_y_continuous(expand = c(0, 0.1)) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 90),
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
      )
    return(plotly::ggplotly(emp_plot, tooltip = "y"))
  }
}


#' Find the closest matching Business
#'
#' @param input_value string pattern to search business name
#'
#' @return Closest matching business name
.closest_business_name <- function(input_value) {

  input_value <- unlist(input_value)

  temp <- unique(igraph::vertex_attr(g, "BusinessName"))
  # sanitized_input_value <- stringr::str_replace(temp, "[^a-zA-Z\\d\\s]", " ")
  # temp[stringr::str_match(temp, input_value)]

  # stringr::str_subset(temp, input_value)
  candidates <- grep(input_value, temp, value = TRUE, ignore.case = TRUE)
  # candidates <- agrep(input_value, candidates, value = TRUE, ignore.case = TRUE)

  candidates[which.min(stringdist::stringdist(input_value, candidates))[1]]
}

#' Find the closest matching Business
#'
#' @inheritParams .closest_business_name
#' @export
#'
#' @examples
#' input_value = "Gyoza Bar"
#' closest_business_name(input_value)
#'
closest_business_name <- memoise::memoise(
  f = .closest_business_name,
  cache = cachem::cache_disk(rappdirs::user_cache_dir("h2h"))
)

#' Calculates risk score
#'
#' @param bname str indicates business name
#'
#' @return
#'
#' @examples
#' bname = "Gyoza Bar Ltd"
#' get_overall_risk_score_card(bname)
#'
get_overall_risk_score_card <- function(bname) {
  bname <- closest_business_name(bname)
  r <- calc_risk(bname)
  # pr <- profile_risk(bname)

  lev <- case_when(
    r <= 1 ~ "low risk",
    r <= 3 ~ "medium risk",
    TRUE ~ "high risk"
  )

  color_back <- case_when(
    r <= 1 ~ "green",
    r <= 3 ~ "yellow",
    TRUE ~ "red"
  )

  list(str_glue("{r} / 4 ({lev})"), color_back)
}

#' Fetches comment about missing data
#'
#' @param bname str indicates business name
#'
#' @return
#'
#' @examples
#' bname = "Gyoza Bar Ltd"
#' get_missing_data_comment(bname)
#'
get_missing_data_comment <- function(bname) {
  bname <- closest_business_name(bname)
  pr <- profile_risk(bname)
  msg <- if_else(pr$missingdata,
                 str_glue("    ",
                          "\U274C",
                          "  has a lot of missing data"),
                 str_glue("    ",
                          "\U2705",
                          "  has few or no missing data"))

  msg
}

#' Fetches comment about history
#'
#' @param bname str indicates business name
#'
#' @return
#'
#' @examples
#' bname = "Gyoza Bar Ltd"
#' get_history_comment(bname)
#'
get_history_comment <- function(bname) {
  bname <- closest_business_name(bname)
  pr <- profile_risk(bname)
  msg <- if_else(pr$recent,
                 str_glue("    ", "\U274C", "  does not have a long history"),
                 str_glue("    ", "\U2705", "  has a long history"))
  msg
}

#' Fetches comment about location
#'
#' @param bname str indicates business name
#'
#' @return
#'
#' @examples
#' bname = "Gyoza Bar Ltd"
#' get_location_comment(bname)
#'
get_location_comment <- function(bname) {
  bname <- closest_business_name(bname)
  pr <- profile_risk(bname)

  msg <- if_else(pr$foreign,
                 str_glue("    ", "\U274C", "  is not located in Canada"),
                 str_glue("    ", "\U2705", "  is located in Canada"))

  msg
}

#' Fetches comment about operation
#'
#' @param bname str indicates business name
#'
#' @return
#'
#' @examples
#' bname = "Gyoza Bar Ltd"
#' get_operative_comment(bname)
#'
get_operative_comment <- function(bname) {
  bname <- closest_business_name(bname)
  pr <- profile_risk(bname)
  msg <- if_else(pr$status,
                 str_glue("    ", "\U274C", "  is inactive or shut"),
                 str_glue("    ", "\U2705", "  is operative"))
  msg
}


#' Calculates risk score
#'
#' @param bname str indicates business name
#'
#' @return
#'
#' @examples
#' bname = "Gyoza Bar Ltd"
#' make_score_card(bname)
#'
make_score_card <- function(bname) {
  bname <- closest_business_name(bname)
  r <- calc_risk(bname)
  pr <- profile_risk(bname)

  lev <- case_when(
    r <= 1 ~ "low risk",
    r <= 3 ~ "medium risk",
    TRUE ~ "high risk"
  )

  color_back <- case_when(
    r <= 1 ~ "green",
    r <= 3 ~ "yellow",
    TRUE ~ "red"
  )

  msg <- c()
  msg[1] <- if_else(pr$missingdata, "has a lot of missing data", NA_character_)
  msg[2] <- if_else(pr$recent, "does not have a long history", NA_character_)
  msg[3] <- if_else(pr$foreign, "is not located in Canada", NA_character_)
  msg[4] <- if_else(pr$status, "is inactive or shut", NA_character_)
  msg <- glue::glue_collapse(na.omit(msg), sep = ", ", last = " and ")

  list(str_glue("Risk Score: {r} / 4 ({lev}). This business {msg}."), color_back)
}
