# Load libraries
library(dash)
library(dashHtmlComponents)
library(dashCoreComponents)
library(dashBootstrapComponents)
library(tidyverse)
library(devtools)

# Load custom functions and data
devtools::load_all(".")

# Load CSS Styles
css <- custom_css()

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
              dccDropdown(
              ),
              htmlBr(),
              dbcLabel("Address:"),
              dccDropdown(
              ),
              htmlHr()
            )
          ),
          htmlBr(),
          dbcCol( # PLOTTING PANEL
            list(
              dccTabs(id = "tabs", children = list(
                dccTab(label = "Risk Tab", children = list(
                  htmlDiv(
                    list(
                      dbcCard(
                        list(
                          dbcCardBody(
                            dccGraph(
                              id = "network_plot",
                              config = list("displayModeBar" = FALSE)
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

app$run_server(debug = T)


