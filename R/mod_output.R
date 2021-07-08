#' sidebar UI Function
#'
#' @description A shiny Module.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom shiny NS tagList
#'
#' @noRd
mod_output_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Enrichment FDR input
    sliderInput(
      inputId = ns("fs_fdr"),
      label = "Select enrichment FDR threshold:",
      min = 0,
      max = 1,
      value = 0.05
    ),
    # Plot x-axis variable input
    radioButtons(
        inputId = ns("fs_xvar"),
        label = "Select x-axis variable",
        choices = list("fold_enrichment", "log10qvalue"),
        selected = "log10qvalue"
    ),
    # Plot point fill variable input
    radioButtons(
        inputId = ns("fs_fill"),
        label = "Select point fill variable",
        choices = list("type", "status"),
        selected = "type"
    ),
    # Plot point size variable input
    radioButtons(
        inputId = ns("fs_point"),
        label = "Select point size variable",
        choices = list("fold_enrichment", "log10qvalue"),
        selected = "fold_enrichment"
    ),
    # Action button input
    actionButton(
      inputId = ns("fs_redraw"),
      label = "Redraw the plot!",
      icon = icon("project-diagram"),
      width = "200px"
    )
  )
}

#' sidebar Server Functions
#'
#' @noRd
mod_output_server <- function(id, rvals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Store input variables in rvals object to share between modules
    # Reactive annotation input
    observeEvent(input$fs_fdr, {
      rvals$fs_fdr = input$fs_fdr
    })
    # Reactive xvariable input
    observeEvent(input$fs_xvar, {
      rvals$fs_xvar = input$fs_xvar
    })
    # Reactive fill input
    observeEvent(input$fs_fill, {
      rvals$fs_fill = input$fs_fill
    })
    # Reactive fill input
    observeEvent(input$fs_point, {
      rvals$fs_point = input$fs_point
    })
    # Reactive action button
    observeEvent(input$fs_redraw, {
      rvals$fs_redraw = input$fs_redraw
    })
  })
}
