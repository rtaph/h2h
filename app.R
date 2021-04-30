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

# define column names
tbl_col_ids <- c("NAME", "LEVEL", "CRES")
tbl_col_names <- c("Name", "Level", "Country")

# Load CSS Styles
css <- custom_css()

legend <- "
**Join Rules for Graph Links**

- **R1:** Exact match on postal code, business name, business type, and business sub-type.
- **R2:** Exact match on postal code, business name, and business type.
- **R3:** Exact match on business name, business type, and business sub-type.
- **R4:** Exact match on business name and GPS coordinates.
- **R5:** Exact match on trade name and GPS coordinates.
- **R6:** Exact match on business name and trade name.
- **R7:** Exact match on business name only.
- **R8:** Business name in one year matches trade name in the next year (or vice versa).
- **R9:** Exact match on postal code with a phonetic match on the businesss name.
- **R10:** Exact match on postal code with a phonetic match on the trade name.
- **R11:** Exact match on postal code, phonetic match between business name and trade name.
- **R12:** Match to the StatsCan Inter-Company Ownership database.
"

# set cache
h2h_cache <- cachem::cache_disk(rappdirs::user_cache_dir("h2h"))

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
              # dccDropdown(id = "typed_bname",
              #             options = map(bnames, ~ list(label = ., value = .)),
              #             value = "Gyoza Bar Ltd",
              #             clearable = TRUE,
              #             searchable = FALSE # TODO: test this out for speed
              dccInput(
                id = "typed_bname",
                # value = "Listel Canada Ltd"         # for testing
                # value = "Westfair Foods Ltd"
                value = "Gyoza Bar Ltd"          # for testing
                # value = "VLC Leaseholds Ltd"      # for testing
              ),
              htmlBr(),
              htmlButton("Submit", n_clicks = 0, id="btn_submit"),
              htmlBr(),
              # dbcLabel("Address:"),
              # dccInput(),
              htmlDiv(id="input_bname", children = "Gyoza Bar Ltd", hidden = TRUE),
              htmlHr(),
              dccMarkdown(legend, style = css$small)
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
                          dbcRow(
                            list(
                              dbcCol(
                                list(
                                  htmlH5("Risk Score", style=list("text-align"="center")),
                                  dbcAlert(id = "overall_risk_score", style=list("text-align"="center"))
                                )
                              ),
                              dbcCol(
                                list(
                                  htmlH5(id = "card_score", style=list("text-align"="center")),
                                  htmlDiv(id = "missing_data_comment"),
                                  htmlDiv(id = "history_comment"),
                                  htmlDiv(id = "location_comment"),
                                  htmlDiv(id = "operative_score")
                                )
                              )
                            )
                          ),


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
                      htmlDiv(list(
                        dbcCard(
                          list(
                            dbcCardHeader('Business Summary'),
                            dbcCardBody(list(
                              # Business Type Table
                              dashDataTable(
                                id = 'co-type',
                                columns = list(label = "Primary Business Type", value = "PrimaryBusinessType"),
                                page_size = 10,
                                style_cell = css$tbl_fonts,
                                style_header = css$tbl_hrow,
                                style_cell_conditional = css$bs_tbl_align,
                                style_as_list_view = TRUE
                              )
                            )
                            )
                          ))), style=css$horiz_split),
                      htmlDiv(list(
                        dbcCard(
                          list(
                            dbcCardHeader("Company Size"),
                            dbcCardBody(list(
                              dccGraph(id = "num_emp_plot")
                            ))
                          )
                        )
                      ), style=css$horiz_split),
                      htmlDiv(list(
                        dbcCard(
                          list(
                            dbcCardHeader('Inter-corporate Relationships'),
                            dbcCardBody(
                              list(
                                dashDataTable(
                                  id = "related_co_table",
                                  page_size = 10,
                                  data = df_to_list(coi_data),
                                  columns = map2(tbl_col_ids, tbl_col_names, function(col, col2) list(id = col, name = col2)),
                                  style_cell_conditional = css$rc_tbl_colw,
                                  fixed_columns = css$fixed_headers,
                                  css = css$tbl_ovrflw,
                                  style_data = css$ovrflow_ws,
                                  style_as_list_view = TRUE,
                                  style_header = css$tbl_hrow,
                                  style_cell = css$tbl_fonts
                                ))
                            )
                          )
                        )
                      ))),
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
# Implement Submit button

app$callback(
  output("input_bname", "children"),
  list(
    input("btn_submit", "n_clicks"),
    state("typed_bname", "value")
  ),
  function (n_click, value) {
    closest_business_name(value) 
  }
)

# update related companies table on "Business Details" tab
app$callback(
  list(output("related_co_table", "data")),
  list(input("input_bname", "children")),
  memoise::memoize(
    make_related_co_table,
    cache = h2h_cache
  )
)

# update business summary on "Business Details" tab
app$callback(
  list(
    output("co-type", "data"),
    output("co-type", "columns")
  ),
  list(input("input_bname", "children")),
  memoise::memoize(
    make_co_type,
    cache = h2h_cache
  )
)

# plot number of employees for industry on "Business Details" tab
app$callback(
  output("num_emp_plot", "figure"),
  list(input("input_bname", "children")),
  num_emp_plot
)


# update network plot on "License History" tab
app$callback(
  output("network_plot", "srcDoc"),
  list(
    input("input_bname", "children")
  ),
  memoise::memoize(function(x = "Gyoza Bar Ltd") {
    viz <- viz_graph(x)

    # workaround
    tempfile <- here::here("network.html")
    htmlwidgets::saveWidget(viz, file = tempfile)
    paste(readLines(tempfile), collapse = "")
  },
  cache = h2h_cache)
)

app$callback(
  output("card_score", "children"),
  list(
    input("input_bname", "children")
  ),
  closest_business_name
)

app$callback(
  output("missing_data_comment", "children"),
  list(
    input("input_bname", "children")
  ),
  memoise::memoize(get_missing_data_comment,
                   cache = h2h_cache)
)

app$callback(
  output("history_comment", "children"),
  list(
    input("input_bname", "children")
  ),
  memoise::memoize(
    get_history_comment,
    cache = h2h_cache
  )
)
app$callback(
  output("location_comment", "children"),
  list(
    input("input_bname", "children")
  ),
  memoise::memoize(
    get_location_comment,
    cache = h2h_cache
  )
)

app$callback(
  output("operative_score", "children"),
  list(
    input("input_bname", "children")
  ),
  memoise::memoize(
    get_operative_comment,
    cache = h2h_cache
  )
)

app$callback(
  list(
    output("overall_risk_score", "children"),
    output("overall_risk_score", "color")
  ),
  list(
    input("input_bname", "children")
  ),

  memoise::memoize(
    get_overall_risk_score_card,
    cache = h2h_cache
  )
)



if (Sys.getenv("DYNO") == "") {
  app$run_server(
    debug = FALSE,
    dev_tools_hot_reload = FALSE
  )
} else {
  app$run_server(host = "0.0.0.0")
}
