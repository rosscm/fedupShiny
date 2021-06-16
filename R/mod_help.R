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
          collapsed = FALSE,
          HTML("<b>REQUIRED BY USER</b><br>Text file (excel, txt, or csv format) with
          genes to test for enrichment per row and unique test sets per column.
          The first column of this file <b>must</b> be labelled 'background'
          (case sensitive) with any additional number of subsequent columns
          named according to your test set(s).<br>A sample input table can be
          downloaded from
          <a href='https://github.com/rosscm/fedupShiny/blob/main/inst/extdata/sample_gene_input.txt'>Github.</a>")
        ),
        accordionItem(
          title = "Annotation type",
          collapsed = FALSE,
          HTML("Option to select relevant pathway annotations for input gene
          data.<br>The annotation files used in this app can be downloaded from
          <a href='https://github.com/rosscm/fedupShiny/tree/main/inst/extdata'>Github.</a>
          Helpful information about these files can also be found at the
          Bader Lab GeneSets <a href='http://baderlab.org/GeneSets'>page.</a>
          <br><br>Current options: Human, Mouse<br>Default: Human")
        ),
        accordionItem(
          title = "Annotation source(s)",
          collapsed = FALSE,
          HTML("Option to select any number of annotation sources for enrichment
          analysis. Again, helpful information about these annotations can be
          found at the Bader Lab GeneSets page linked above.<br><br>
          Current options: GOBP, REACTOME, MSIGDB_C2, HUMANCYC, PANTHER, IOB, NETPATH
          <br>Default: None")
        ),
        accordionItem(
          title = "Annotation size range",
          collapsed = FALSE,
          HTML("Slider option to select minimum and maximum number of genes per
          pathway annotation.<br><br>Current option: 0 to 1000<br>
          Default: 10 to 300")
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
