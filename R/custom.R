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

  # related companies table: relative column widths, alignment
  css$rc_tbl_colw <- list(
  list(
    "if" = list("column_id" = "NAME"),
    width = "250px",
    textAlign = "left"
    ),
  list(
    "if" = list("column_id" = "LEVEL"),
    width = "100px",
    textAlign = 'center'
  ),
  list(
    "if" = list("column_id" = "CCTL"),
    width = "100px",
    textAlign = 'center'
  )
  )

  # tab 2 table: overflow text
  css$tbl_ovrflw <- list(
    list(
      selector = '.dash-cell div.dash-cell-value',
      rule = 'display: inline; white-space: inherit; overflow: inherit; text-overflow: inherit;'
    )
  )

  css$ovrflow_ws <- list(whiteSpace = "normal", height = "auto")

  css$fixed_headers <- list(headers = TRUE)

  # related companies table: header row styling
  css$tbl_hrow <- list(
    backgroundColor = "white",
    fontWeight = 'bold'
  )

  # co summary table: alignment
  css$bs_tbl_align <- list(
    list(
      'if' = list('column_id' = "BusinessType"),
      textAlign = 'center'
    )
  )

  # tab 2 tables: style fonts
  css$tbl_fonts <- list('fontSize' = 12, 'font-family' = 'sans-serif', 'whiteSpace' = "normal")

  # tab 2 layout: share row 50/50 split
  css$horiz_split <- list('display' = 'inline-block', 'width' = '50%')
  # Return CSS
  css
}
