#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.#'
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @export
run_app <- function(
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server
    ),
    golem_opts = list(...)
  )
}
