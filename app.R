# Load libraries
library(dash)
library(dashHtmlComponents)
library(dashCoreComponents)
library(dashBootstrapComponents)
library(dashTable)
library(dplyr)
library(purrr)
library(stringr)
library(devtools)
library(here)



# Load custom functions and data
devtools::load_all(".")
combined_data <- read.csv(here("data-processed/combined_data.csv"), fileEncoding="latin1")
coi_data <- read.csv(here("data-processed/filtered_hierarchy_data.csv"), fileEncoding="latin1") %>%
  rename("COUNTRY_OF_CONTROL" = "CCTL")
table_columns <- c("NAME", "LEVEL", "COUNTRY_OF_CONTROL")


# Load CSS Styles
css <- custom_css()

# bnames <- as.character(unique(forcats::fct_c(h2h::vbr$BusinessName,
#                   h2h::vbr$BusinessTradeName)))
# bnames <- bnames[!is.na(bnames) & !bnames == ""]
# bnames <- c(head(bnames, 20))

# app layout
app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)
app$title("CoV Business Risk Dashboard")
app$layout(
  dbcContainer(
    list(
      htmlH1("Company Risk Dashboard",
        style = css$header
      ),
      dbcRow(
        list(
          dbcCol( # INPUT CONTROLS
            id = "options-panel",
            style = css$box,
            md = 4,
            list(
              htmlBr(),
              dbcLabel("Company Name:"),
              # dccDropdown(id = "input_bname",
              #             options = map(bnames, ~ list(label = ., value = .)),
              #             value = "Gyoza Bar Ltd",
              #             clearable = TRUE,
              #             searchable = FALSE # TODO: test this out for speed
              dccInput(
                id = "input_bname",
                value = "Listel Canada Ltd"         # for testing
                # value = "Gyoza Bar Ltd"           # for testing
                # value = "VLC Leaseholds Ltd"      # for testing
              ),
              htmlBr(),
              dbcLabel("Address:"),
              dccInput(),
              htmlHr()
            )
          ),
          htmlBr(),
          dbcCol( # PLOTTING PANEL
            list(
              dccTabs(id = "tabs", children = list(
                dccTab(label = "License History", children = list(
                  htmlDiv(
                    list(
                      dbcCard(
                        list(
                          dbcAlert(
                            #is_open = risk_score(bname) > 2,
                            id = "card_score",
                            color = "light"),
                          dbcCardBody(
                            htmlDiv(id = "network_div", children = list(
                              htmlIframe(
                                height = 500, width = 500,
                                id = "network_plot",
                                style = css$noborder
                              )
                            ))
                          )
                        )
                      ),
                      dbcCard(
                        list(
                          dbcCardBody()
                        )
                      )
                    )
                  )
                )),
                dccTab(label = "Business Details", children = list(
                  htmlDiv(
                    list(
                      dbcCard(
                        list(
                          dbcCardHeader('Business Summary'),
                          dbcCardBody(list(
                            dashDataTable(
                              id = 'co-type'
                            )
                          )
                        )
                      )),
                      dbcCard(
                        list(
                          dbcCardHeader('Inter-corporate Relationships'),
                          dbcCardBody(
                            list(
                            dashDataTable(
                              id = "related_co_table",
                              page_size = 10,
                              data = df_to_list(coi_data),
                              columns = lapply(table_columns,
                                               function(colName){
                                                 list(
                                                   id = colName,
                                                   name = colName
                              )
                              }),
                              fixed_columns = list(headers = TRUE),
                              style_cell_conditional = css$rc_tbl_colw,
                              style_as_list_view = TRUE,
                              style_cell = css$rc_tbl_fonts,
                              style_header = css$rc_tbl_hrow,
                              css = list(
                                  list(
                                    selector = '.dash-cell div.dash-cell-value',
                                    rule = 'display: inline; white-space: inherit; overflow: inherit; text-overflow: inherit;'
                                  )
                                )))
                            # style_data = list(
                            #   whiteSpace = "normal"
                            # ),
                            # ,

                          )
                        )
                      )
                    )
                  )
                ))
              ))
            ),
          )
        ),
        style = css$no_left_pad
      ),
      htmlBr()
    )
  )
)

# update related companies table on "Business Details" tab
app$callback(
  list(output("related_co_table", "data")),
  list(input("input_bname", "value")),
  function(input_value) {
    if (combined_data %>%
      filter(BusinessName == input_value) %>%
      select("PID") %>%
      unique() %>% nrow() < 1) {
      # if no related companies found, return empty list.
      data <- list()
    }
    else {
      selected_PID <- combined_data %>%
        filter(BusinessName == input_value) %>%
        select("PID") %>%
        unique()
      data <- df_to_list(coi_data %>%
        filter(PID == selected_PID[[1]] & LEVEL > 0) %>%
        arrange(LEVEL))
    }
    list(data)
  }
)

# update business summary on "Business Details" tab
app$callback(
  list(output("co-type", "data"),
       output("co-type", "columns")),
  list(input("input_bname", "value")),
  function(input_value) {
    data <- combined_data %>%
      filter((BusinessName == input_value) & (FOLDERYEAR <= format(Sys.Date(), "%y"))) %>%
      filter(FOLDERYEAR == max(FOLDERYEAR)) %>%
      arrange(desc(FOLDERYEAR)) %>%
      top_n(n = 1, wt = FOLDERYEAR) %>%
      select(BusinessType) %>%
      unique %>%
      df_to_list()
    columns <- c("BusinessType") %>% purrr::map(function(col) list(name = col, id = col))
    list(data, columns)
})




# update network plot on "License History" tab
app$callback(
  output("network_plot", "srcDoc"),
  list(
    input("input_bname", "value")
  ),
  memoise::memoize(function(x) {
    viz <- viz_graph(x)

    # workaround
    tempfile <- here::here("network.html")
    htmlwidgets::saveWidget(viz, file = tempfile)
    paste(readLines(tempfile), collapse = "")
  })
)

app$callback(
  output("card_score", "children"),
  list(
    input("input_bname", "value")
  ),
  memoise::memoize(function(bname) {
    r <- calc_risk(bname)
    pr <- profile_risk(bname)

    lev <- case_when(r <= 1 ~ "low risk",
                     r <= 3 ~ "medium risk",
                     TRUE ~ "high risk")

    msg <- c()
    msg[1] <- if_else(pr$missingdata, "has a lot of missing data", NA_character_)
    msg[2] <- if_else(pr$recent, "does not have a long history", NA_character_)
    msg[3] <- if_else(pr$foreign, "is not located in Canada", NA_character_)
    msg[4] <- if_else(pr$status, "is inactive or shut", NA_character_)
    msg <- glue::glue_collapse(na.omit(msg), sep = ", ", last = " and ")

    str_glue("Risk Score: {r} / 4 ({lev}). This business {msg}.")
    })
)

if (Sys.getenv("DYNO") == "") {
  app$run_server(
    debug = FALSE,
    dev_tools_hot_reload = FALSE
  )
} else {
  app$run_server(host = "0.0.0.0")
}
