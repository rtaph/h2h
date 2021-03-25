#' List of Custom CSS Specs
#'
#' @return A named list of lists containing CSS specifications
#' @export
custom_css <- function() {
  css <- list()

  # Input parameter box
  css$box <- list(
    "border" = "1px solid #d3d3d3",
    "border-radius" = "10px",
    "margin" = "0px",
    "background-color" = "rgba(220, 220, 220, 0.5)"
  )

  css$noborder <- list(
    "border-width" = "0"
  )

  # Dashboard header parameters
  css$header <- list(
    "background-color" =  "gray",
    "padding" = 20,
    "color" = "white",
    "margin-top" = 20,
    "margin-bottom" = 20,
    "font-size" = "48px",
    "border-radius" = 3
  )

  # Drop-down choices
  css$dd <- list("font-size" = "smaller")

  # Footnote text
  css$sources <- list("font-size" = "xx-small")

  css$no_left_pad <- list("margin-left" = "0px")

  # Plotly fonts
  css$plotly <- list("family" = paste0(
    "-apple-system,",
    "BlinkMacSystemFont,",
    "'Segoe UI',",
    "Roboto,",
    "'Helvetica Neue',",
    "Arial"
  ))

  # radio-buttons
  css$radio_buttons <- list(
    "margin-left" = "10px",
    "margin-right" = "5px"
  )

  # related companies table: relative column widths
  css$rc_tbl_colw <- list(
    list(
      "if" = list("column_id" = "NAME"),
      width = "60%"
    ),
    list(
      "if" = list("column_id" = "LEVEL"),
      width = "20%"
    )
  )

  # Return CSS
  css
}
