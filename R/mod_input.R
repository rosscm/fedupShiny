#' sidebar UI Function
#'
#' @description A shiny Module.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom shiny NS tagList
#'
#' @noRd
mod_input_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Gene input
    fileInput(
      inputId = ns("fs_input"),
      label = "Select file with gene input table:",
      multiple = FALSE,
      accept = c(".txt", ".csv", ".xlsx", ".xls"),
      buttonLabel = "Browse"
    ),
    # Annotation type input
    radioButtons(
      inputId = ns("fs_anno"),
      label = "Select annotation type:",
      choices = list("Human", "Mouse"),
      selected = "Human"
    ),
    # Annotation fs_source(s) input
    checkboxGroupInput(
      inputId = ns("fs_source"),
      label = "Select annotation source(s):",
      choices = list("GOBP", "REACTOME", "MSIGDB_C2", "HUMANCYC", "PANTHER", "IOB", "NETPATH"),
      selected = "REACTOME"
    ),
    # Annotation size slider input
    sliderInput(
      inputId = ns("fs_size"),
      label = "Select annotation size range:",
      min = 0,
      max = 1000,
      value = c(10, 500)
    ),
    # Action button input
    actionButton(
      inputId = ns("fs_run"),
      label = "Analyze my data!",
      icon = icon("project-diagram"),
      width = "200px"
    )
  )
}

#' sidebar Server Functions
#'
#' @noRd
mod_input_server <- function(id, rvals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Store input variables in rvals object to share between modules
    # Reactive gene input
    observeEvent(input$fs_input, {
      rvals$fs_input = input$fs_input
    })
    # Reactive annotation input
    observeEvent(input$fs_anno, {
      rvals$fs_anno = input$fs_anno
    })
    # Reactive fs_source input
    observeEvent(input$fs_source, {
      rvals$fs_source = input$fs_source
    })
    # Reactive size input
    observeEvent(input$fs_size, {
      rvals$fs_size = input$fs_size
    })
    # Reactive action button
    observeEvent(input$fs_run, {
      rvals$fs_run = input$fs_run
    })
  })
}
