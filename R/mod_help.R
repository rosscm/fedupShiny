#' help UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom shiny NS tagList
#' @importFrom shinydashboardPlus accordion accordionItem attachmentBlock
#'
#' @noRd
mod_help_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Welcome to fedupShiny!"),
    shinydashboardPlus::box(
      title = "Introduction",
      width = NULL,
      status = "primary",
      solidHeader = TRUE,
      HTML("<b>fedupShiny</b> is an R <mark>Shiny</mark> implementation of the
      Bioconductor <mark>fedup</mark> package
      (check out the links in the sidebar to view related information). This package
      tests for enrichment and depletion of user-defined
      pathways using a Fisher's exact test. The method is designed for versatile
      pathway annotation formats (eg. gmt, txt, xlsx) to allow the user to run
      pathway analysis on custom annotations. This package is also
      integrated with <mark>Cytoscape</mark> to provide network-based pathway
      visualization that enhances the interpretability of the results.")
    ),
    shinydashboardPlus::box(
      title = "Input Configuration",
      width = NULL,
      status = "primary",
      solidHeader = TRUE,
      accordion(
        id = "accordion1",
        accordionItem(
          title = "Gene input table",
          collapsed = TRUE,
          "This is some text!",
          attachmentBlock(
            image = "http://kiev.carpediem.cd/data/afisha/o/2d/c7/2dc7670333.jpg",
            title = "Test",
            href = "http://google.com",
            "This is the content"
          ),
        ),
        accordionItem(
          title = "Annotation type",
          collapsed = TRUE,
          HTML("Select annotation size range <br>
          (min/max number of genes per pathway):")
        ),
        accordionItem(
          title = "Annotation source(s)",
          collapsed = TRUE,
          "This is some text!"
        ),
        accordionItem(
          title = "Annotation size range",
          collapsed = TRUE,
          "This is some text!"
        )
      )
    )
  )
}

#' help Server Functions
#'
#' @noRd
mod_help_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}
