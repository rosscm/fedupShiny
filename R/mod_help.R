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
      HTML("<b>fedupShiny</b> is an R Shiny implementation of the
      Bioconductor <b>fedup</b> package
      (check out the links in the sidebar to view related information). This package
      tests for enrichment and depletion of user-defined
      pathways using a Fisher's exact test. The method is designed for versatile
      pathway annotation formats (eg. gmt, txt, xlsx) to allow the user to run
      pathway analysis on custom annotations. This package is also
      integrated with <b>Cytoscape</b> to provide network-based pathway
      visualization that enhances the interpretability of the results.")
    ),
    shinydashboardPlus::box(
      title = "Input Configuration",
      width = NULL,
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,
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
          downloaded from Github
          <a href='https://github.com/rosscm/fedupShiny/blob/main/inst/extdata/sample_gene_input.txt'>here.</a>")
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
          <br>Default: REACTOME")
        ),
        accordionItem(
          title = "Annotation size range",
          collapsed = FALSE,
          HTML("Slider option to select minimum and maximum number of genes per
          pathway annotation.<br><br>Current option: 0 to 1000<br>
          Default: 10 to 500")
        )
      ),
      HTML("After you have provided your input gene table and tweaked the other
      parameters to your liking, hit the <b>Analyze my data!'</b> button at the
      bottom of the <mark>Input Parameters</mark> tab and head over
      to the <mark>Results Dashboard</mark>.")
    ),
    shinydashboardPlus::box(
      title = "Results Dashboard",
      width = NULL,
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,
      accordion(
        id = "accordion2",
        accordionItem(
          title = "Test sets",
          collapsed = FALSE,
          HTML("Gives the total number of gene sets that were tested for pathway
          enrichment and depletion.")
        ),
        accordionItem(
          title = "Tested pathways",
          collapsed = FALSE,
          HTML("Gives the total number of pathways that were tested after configuring
          the annotation source(s) and annotation size range.")
        ),
        accordionItem(
          title = "Enriched pathways",
          collapsed = FALSE,
          HTML("Gives the total number of significantly enriched and depleted pathways.
          The results can be tweaked further in the <mark>Output Configuration</mark>
          tab (learn more about this in the box below).")
        ),
        accordionItem(
          title = "Enrichment Dot Plot",
          collapsed = FALSE,
          HTML("Visualizes fedup enrichment and depletion results using ggplot.
          Variables are defined in the <mark>Enrichment Table</mark> section below.")
        ),
        accordionItem(
          title = "Enrichment Table",
          collapsed = FALSE,
          HTML("Gives a tabular format of fedup results.
          Rows represent significant pathways. Columns represent:<br>
          <b>pathway</b> -- name of the pathway<br>
          <b>size</b> -- size of the pathway<br>
          <b>real_frac</b> -- fraction of test gene members in pathway<br>
          <b>expected_frac</b> -- fraction of background gene members in pathway<br>
          <b>fold_enrichment</b> -- fold enrichment measure, evaluates as real_frac / expected_frac<br>
          <b>status</b> -- indicator that pathway is enriched or depleted for test gene members<br>
          <b>real_gene</b> -- vector of test gene members annotated to pathways<br>
          <b>pvalue</b> -- enrichment p-value calculated via Fisher's exact test<br>
          <b>qvalue</b> -- BH-adjusted p-value (ie FDR)<br>
          <b>log10qvalue</b> -- -log10-transformed qvalue")
        ),
        accordionItem(
          title = "EnrichmentMap (button)",
          collapsed = FALSE,
          HTML("Generates an EnrichmentMap of the results shown in <mark>Enrichment
          Dot Plot</mark>. As a note, Cytoscape must be downloaded and running on
          your computer for this to work. Check out the <mark>EnrichmentMap
          Pipeline</mark> link in the sidebar for download info.")
        )
      )
    ),
    shinydashboardPlus::box(
      title = "Output Configuration",
      width = NULL,
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,
      accordion(
        id = "accordion3",
        accordionItem(
          title = "Select enrichment FDR threshold",
          collapsed = FALSE,
          HTML("FDR (qvalue) threshold to define significantly enriched and
          depleted pathways.<br><br>Current option: 0 to 1<br>Default: 0.05")
        ),
        accordionItem(
          title = "Select plot x-axis variable",
          collapsed = FALSE,
          HTML("Variable to plot on the x-axis.
          <br><br>Current options: fold_enrichment, log10qvalue<br>Default: log10qvalue")
        ),
        accordionItem(
          title = "Plot x-axis label",
          collapsed = FALSE,
          HTML("Label for the x-axis<br><br>Default:'-log10(FDR)'")
        ),
        accordionItem(
          title = "Select point fill variable",
          collapsed = FALSE,
          HTML("Variable to use to fill plot points.
          <br><br>Current options: type, status<br>Default: type")
        ),
        accordionItem(
          title = "Point fill legend title",
          collapsed = FALSE,
          HTML("Legend title for point fill variable<br><br>Default: 'Genetic interaction'")
        ),
        accordionItem(
          title = "Select point size variable",
          collapsed = FALSE,
          HTML("Variable to use to adjust point size.
          <br><br>Current options: fold_enrichment, log10qvalue<br>Default: log10qvalue")
        ),
        accordionItem(
          title = "Point size legend title",
          collapsed = FALSE,
          HTML("Legend title for point size variable <br><br>Default: 'Fold enrichment'")
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
