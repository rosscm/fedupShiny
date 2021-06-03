#' main UI Function
#'
#' @description A shiny Module.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom shiny NS tagList
#' @importFrom DT DTOutput
#' @importFrom shinyjs hidden
#'
#' @noRd
mod_main_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      valueBoxOutput(ns("nPath")),
      valueBoxOutput(ns("nBack")),
      valueBoxOutput(ns("nTest"))
    ),
    actionButton(
      inputId = ns("plotEM"),
      label = "EnrichmentMap",
      icon = icon("project-diagram")
    ),
    rep_br(1),
    fluidRow(
      column(width = 12,
        box(
          title = "Enrichment Dot Plot", width = NULL, status = "success",
          solidHeader = TRUE, collapsed = FALSE, collapsible = TRUE,
          plotOutput(ns("resPlot")), uiOutput(ns("getPlot"))
        )
      )
    ),
    rep_br(1),
    fluidRow(
      column(width = 12,
        box(
          title = "Enrichment Table", width = NULL, status = "success",
          solidHeader = TRUE, collapsed = FALSE, collapsible = TRUE,
          div(style = 'overflow-x: scroll', DTOutput(ns("resTable"))),
          uiOutput(ns("getData"))
        )
      )
    )
  )
}

#' main Server Functions
#' @import openxlsx
#' @import fedup
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import RCy3
#' @import waiter
#' @importFrom DT renderDT DTOutput
#' @importFrom utils write.csv
#' @importFrom shinyalert shinyalert
#' @importFrom stats na.omit
#' @importFrom utils read.delim
#' @importFrom data.table fwrite
#'
#' @noRd
mod_main_server <- function(id, rvals, dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({

      ######
      # DATA INPUT
      ######

      # Read in gene input file
      if (is.null(rvals$geneInput)) {
        return(NULL)
      }

      file <- rvals$geneInput
      ext <- tools::file_ext(file$datapath)
      filename <- gsub(paste0(".", ext), "", file)

      if (ext == "txt" || ext == "csv") {
        dat <- read.delim(file$datapath, header = TRUE)
      } else if (ext == "xlsx") {
        dat <- read.xlsx(file$datapath)
      }

      # Read in annotation file
      anno_files <- list.files(pattern = "gmt", path = file.path("inst", "extdata"), full.names = TRUE)
      anno_read <- grep(rvals$annoInput, anno_files, value = TRUE)
      anno <- readPathways(anno_read, minGene = min(rvals$sizeInput), maxGene = max(rvals$sizeInput))

      # Subset annotation source(s)
      anno_source <- paste0("%", rvals$sourceInput, collapse = "|")
      anno_final <- anno[grep(anno_source, names(anno))]

      ######
      # DATA PREP
      ######

      # Prepare gene input for fedup
      genes <- as.list(dat)
      genes <- lapply(genes, function(x) {
        x <- as.character(na.omit(x))
        return(x)
      })

      if (names(genes)[1] != "background") {
        shinyalert(
          text = "Input gene table must contain 'background' as the first column!
            Take a look at the sample data provided in the Introduction and
            Usage section.",
          type = "error", animation = TRUE, size = "xs"
        )
        return(NULL)
      }

      ######
      # ANALYSIS
      ######

      # Run fedup with selected inputs
      waiter_show(
        html = tagList(
          spin_fading_circles(),
          "Analyzing your data... head to the Output Dashboard to take a look at the results."
        )
      )

      fedup_res <- runFedup(genes, anno_final)
      waiter_hide()

      # Prepare results for plotting
      fedup_plot <- fedup_res %>%
        bind_rows(.id = "set") %>%
        separate(col = "set", into = c("set", "sign"), sep = "_") %>%
        subset(qvalue < 0.05) %>%
        mutate(log10qvalue = -log10(qvalue)) %>%
        mutate(pathway = gsub("\\%.*", "", pathway)) %>%
        mutate(status = factor(status, levels = c("enriched", "depleted"))) %>%
        as.data.frame()

      # Plot
      p <- plotDotPlot(
        df = fedup_plot,
        xVar = "log10qvalue",
        yVar = "pathway",
        xLab = "-log10(qvalue)",
        fillVar = "sign",
        fillLab = "Genetic interaction",
        fillCol = c("#6D90CA", "#F6EB13"),
        sizeVar = "fold_enrichment",
        sizeLab = "Fold enrichment"
      ) +
      facet_grid("sign", scales = "free", space = "free") +
      theme(strip.text.y = element_blank())

      # Get rid of check notes
      pathway=qvalue=status=NULL

      ######
      # OUTPUT RENDERING
      ######

      # Pathway number value box
      output$nPath <- renderValueBox({
        valueBox(
          length(anno_final), "Pathways", icon = icon("list"), color = "purple"
        )
      })

      # Background gene number value box
      output$nBack <- renderValueBox({
        valueBox(
          length(genes[[1]]), "Background genes", icon = icon("list"), color = "purple"
        )
      })

      # Test set number value box
      output$nTest <- renderValueBox({
        valueBox(
          length(genes)-1, "Test sets", icon = icon("list"), color = "purple"
        )
      })

      # Plot output
      output$resPlot <- renderPlot({
        if (!nrow(fedup_plot)) {
          shinyalert(
            text = "Nothing to plot! Try out another input configuration.",
            size = "xs", type = "warning"
          )
          return(NULL)
        }
        return(p)
      })

      # Table output
      output$resTable <- renderDT({
        if (!nrow(fedup_plot)) {
          return(NULL)
        }
        return(fedup_plot)
      })

      ######
      # OUTPUT DOWNLOADING
      ######

      # Download plot
      output$getPlot <- renderUI({
        req(fedup_plot)
        downloadButton(ns("downloadPlot"), label = "Download plot")
      })

      output$downloadPlot <- downloadHandler(
        filename = function() {
          paste0(filename, "_fedupPlot", ".png")
        },
        content = function(file) {
          device <- function(..., width, height) grDevices::png(..., width = 9,
            height = 5.5, res = 300, units = "in")
          ggsave(file, plot = p, device = device)
      })

      # Download table
      output$getData <- renderUI({
        req(fedup_plot)
        downloadButton(ns("downloadData"), label = "Download table")
      })

      output$downloadData <- downloadHandler(
       filename = function() {
         paste0(filename, "_fedupTable", ".txt")
       },
       content = function(con) {
         fwrite(fedup_plot, con, sep="\t", sep2=c("", " ", ""))
      })




      # NOTE testing Cytoscape functionality
      observeEvent(input$plotEM, {
        tryCatch({
          cytoscapePing()
        },
        error = function(x) {
          shinyalert(
            text = "Cytoscape needs to be downloaded and running on your computer
              for this to work! Check the Introduction and Usage section for help.",
            type = "warning", size = "xs"
          )
          return(NULL)
        })

        #results_folder <- tempdir()
        #writeFemap(fedup_res, results_folder)
        #gmt_file <- tempfile("pathwaysGMT", fileext = ".gmt")
        #writePathways(anno_final, gmt_file)
        #net_file <- tempfile("fedupEM", fileext = ".png")

        #plotFemap(
        #  gmtFile = gmt_file,
        #  resultsFolder = results_folder,
        #  qvalue = 0.05,
        #  chartData = "DATA_SET",
        #  hideNodeLabels = TRUE,
        #  netName = "fedupEM",
        #  netFile = net_file
        #)
      })
    })
  })
}
