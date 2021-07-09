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
    fs_sig_results = NULL,
    fs_plot = NULL,
    fs_fdr = NULL,
    fs_xvar = NULL,
    fs_xvar_lab = NULL,
    fs_fill = NULL,
    fs_fill_lab = NULL,
    fs_point = NULL,
    fs_point_lab = NULL,
    fs_redraw = NULL
  )

  # Sidebar input module
  mod_input_server("input", rvals)

  # Main panel module
  mod_main_server("main", rvals)

  # Sidebar output module
  mod_output_server("output", rvals)
}
