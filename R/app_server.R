#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny
#'
#' @noRd
app_server <- function(input, output, session) {

  # List reactive input values
  rvals <- reactiveValues(
    fs_input = NULL,
    fs_anno = NULL,
    fs_source = NULL,
    fs_size = NULL,
    fs_run = NULL,
    fs_file = NULL,
    fs_set = NULL,
    fs_results = NULL,
    fs_fdr = NULL,
    fs_xvar = NULL,
    fs_fill = NULL,
    fs_point = NULL,
    fs_redraw = NULL
  )

  # Sidebar input module
  mod_input_server("input", rvals)

  # Main panel module
  mod_main_server("main", rvals)

  # Sidebar output module
  mod_output_server("output", rvals)
}
