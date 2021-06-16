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
      inputId = ns("geneInput"),
      label = "Select file with gene input table:",
      multiple = FALSE,
      accept = c(".txt", ".csv", ".xlsx", ".xls"),
      buttonLabel = "Browse"
    ),
    # Annotation type input
    radioButtons(
      inputId = ns("annoInput"),
      label = "Select annotation type:",
      choices = list("Human", "Mouse"),
      selected = "Human"
    ),
    # Annotation source(s) input
    checkboxGroupInput(
      inputId = ns("sourceInput"),
      label = "Select annotation source(s):",
      choices = list("GOBP", "REACTOME", "MSIGDB_C2", "HUMANCYC", "PANTHER", "IOB", "NETPATH"),
      selected = NULL
    ),
    # Annotation size slider input
    sliderInput(
      inputId = ns("sizeInput"),
      label = "Select annotation size range:",
      min = 0,
      max = 1000,
      value = c(10, 300)
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
    observeEvent(input$geneInput, {
      rvals$geneInput = input$geneInput
    })
    # Reactive annotation input
    observeEvent(input$annoInput, {
      rvals$annoInput = input$annoInput
    })
    # Reactive source input
    observeEvent(input$sourceInput, {
      rvals$sourceInput = input$sourceInput
    })
    # Reactive size input
    observeEvent(input$sizeInput, {
      rvals$sizeInput = input$sizeInput
    })
  })
}
