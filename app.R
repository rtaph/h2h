# Load libraries
library(dash)
library(dashHtmlComponents)
library(dashCoreComponents)
library(dashBootstrapComponents)
library(dplyr)
library(purrr)
library(stringr)
library(devtools)

# Load custom functions and data
devtools::load_all(".")

# Load CSS Styles
css <- custom_css()

bnames <- unique(c(h2h::vbr$BusinessName,
                   h2h::vbr$BusinessTradeName))
bnames <- bnames[!is.na(bnames) & !bnames == ""]

#app layout
app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)
app$title("Company Risk Dashboard")
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
              dccDropdown(id = "input_bname",
                          options = head(
                            map(bnames, ~ list(label = ., value = .)),
                            n = 20
                            ),
                          value = "Gyoza Bar Ltd",
                          clearable = TRUE,
                          searchable = FALSE # TODO: test this out for speed
              ),
              #dccInput(id = "input_bname",
              #        value = "MML Properties Ltd"
              #),
              htmlBr(),
              dbcLabel("Address:"),
              dccInput(
              ),
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
                          dbcCardBody(
                            htmlDiv(id = "network_div", children = list(
                                htmlIframe(height = 500, width = 500,
                                  id = "network_plot"
                                )
                              )
                            )
                          )
                        )
                      ),
                      dbcCard(
                        list(
                          dbcCardBody(
                          )
                        )
                      )
                    )
                  )
                )),
                dccTab(label = "Tab2", children = list(
                  htmlDiv(
                    list(
                      dbcCard(
                        list(
                          dbcCardBody(

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

app$callback(
  output("network_plot", "srcDoc"),
  list(
    input("input_bname", "value")
  ),
  function(x) {
    viz <- viz_graph(x)

    # workaround
    tempfile <- here::here("outputs", "network.html")
    htmlwidgets::saveWidget(viz, file = tempfile)
    paste(readLines(tempfile), collapse = "")
  }
)

if (Sys.getenv("DYNO") == "") {
  app$run_server(
    debug = FALSE,
    dev_tools_hot_reload = FALSE
  )
} else {
  app$run_server(host = "0.0.0.0")
}


