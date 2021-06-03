#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny
#'
#' @noRd
app_server <- function(input, output, session) {
  # List reactive input values
  rvals <- reactiveValues(
    geneInput = NULL,
    annoInput = NULL,
    sourceInput = NULL,
    sizeInput = NULL
  )

  # Sidebar input module
  mod_input_server("input", rvals)

  # Main panel module
  mod_main_server("main", rvals)
}
