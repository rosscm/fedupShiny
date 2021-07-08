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

    ######
    # STEP 1
    ######

    # Run fedup and render outputs using **default** settings
    observeEvent(rvals$fs_run, {

      ######
      # DATA INPUT
      ######

      # Read in gene input file
      if (is.null(rvals$fs_input)) {
        return(NULL)
      }

      file <- rvals$fs_input
      ext <- tools::file_ext(file$datapath)
      filename <- gsub(paste0(".", ext), "", file)

      # Store filename in rvals object
      observeEvent(filename, {
        rvals$fs_file = filename
      })

      if (ext == "txt" || ext == "csv") {
        dat <- read.delim(file$datapath, header = TRUE)
      } else if (ext == "xlsx") {
        dat <- read.xlsx(file$datapath)
      }

      # Read in annotation file
      annoFiles <- list.files(pattern = "gmt", path = file.path("inst", "extdata"), full.names = TRUE)
      annoRead <- grep(rvals$fs_anno, annoFiles, value = TRUE)
      anno <- readPathways(annoRead, minGene = min(rvals$fs_size), maxGene = max(rvals$fs_size))

      print(annoRead)

      # Subset annotation source(s)
      annoSource <- paste0("%", rvals$fs_source, collapse = "|")
      annoFinal <- anno[grep(annoSource, names(anno))]

      # Store annotations in rvals object
      observeEvent(annoFinal, {
        rvals$fs_set = annoFinal
      })

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

      # Waiter animation as results are being calculated
      waiter_show(
        html = tagList(
          spin_fading_circles(),
          "Analyzing your data... head to the Results Dashboard to take a look."
        )
      )

      # Run fedup with selected inputs
      fedupRes <- runFedup(genes, annoFinal)
      waiter_hide()

      # Store results in rvals object
      observeEvent(fedupRes, {
        rvals$fs_results = fedupRes
      })

      # Get rid of check notes
      pathway=qvalue=status=NULL

      # Prepare results for plotting
      fedupPlot <- fedupRes %>%
        bind_rows(.id = "set") %>%
        separate(col = "set", into = c("set", "sign"), sep = "_") %>%
        subset(qvalue < 0.05) %>%
        mutate(log10qvalue = -log10(qvalue)) %>%
        mutate(pathway = gsub("\\%.*", "", pathway)) %>%
        mutate(status = factor(status, levels = c("enriched", "depleted"))) %>%
        as.data.frame()

      # Plot
      p <- plotDotPlot(
        df = fedupPlot,
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

      ######
      # OUTPUT RENDERING
      ######

      # Pathway number value box
      output$nPath <- renderValueBox({
        valueBox(
          length(annoFinal), "Tested pathways", icon = icon("list"), color = "purple"
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
        if (!nrow(fedupPlot)) {
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
        if (!nrow(fedupPlot)) {
          return(NULL)
        }
        return(fedupPlot)
      })

      ######
      # OUTPUT DOWNLOADING
      ######

      # Download plot
      output$getPlot <- renderUI({
        req(fedupPlot)
        downloadButton(ns("downloadPlot"), label = "Download plot")
      })

      output$downloadPlot <- downloadHandler(
        filename = function() {
          paste0(fs_file, "_fedupPlot", ".png")
        },
        content = function(file) {
          device <- function(..., width, height) grDevices::png(..., width = 9,
            height = 5.5, res = 300, units = "in")
          ggsave(file, plot = p, device = device)
      })

      # Download table
      output$getData <- renderUI({
        req(fedupPlot)
        downloadButton(ns("downloadData"), label = "Download table")
      })

      output$downloadData <- downloadHandler(
       filename = function() {
         paste0(fs_file, "_fedupTable", ".txt")
       },
       content = function(con) {
         fwrite(fedupPlot, con, sep="\t", sep2=c("", " ", ""))
      })
    })


    ######
    # STEP 2
    ######

    # Render outputs using **custom** settings
    observeEvent(rvals$fs_redraw, {

      # Prepare results for plotting
      fedupRes <- rvals$fs_results
      fedupPlot <- fedupRes %>%
        bind_rows(.id = "set") %>%
        separate(col = "set", into = c("set", "type"), sep = "_") %>%
        subset(qvalue < rvals$fs_fdr) %>%
        mutate(log10qvalue = -log10(qvalue)) %>%
        mutate(pathway = gsub("\\%.*", "", pathway)) %>%
        mutate(status = factor(status, levels = c("enriched", "depleted"))) %>%
        as.data.frame()

      # Plot
      p <- plotDotPlot(
        df = fedupPlot,
        xVar = rvals$fs_xvar,
        yVar = "pathway",
        xLab = rvals$fs_xvar,
        fillVar = rvals$fs_fill,
        fillCol = c("#6D90CA", "#F6EB13"),
        sizeVar = rvals$fs_point
      ) +
      facet_grid(rvals$fs_fill, scales = "free", space = "free") +
      theme(strip.text.y = element_blank())

      ######
      # OUTPUT RENDERING
      ######

      # Plot output
      output$resPlot <- renderPlot({
        if (!nrow(fedupPlot)) {
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
        if (!nrow(fedupPlot)) {
          return(NULL)
        }
        return(fedupPlot)
      })

      ######
      # OUTPUT DOWNLOADING
      ######

      # Download plot
      output$getPlot <- renderUI({
        req(fedupPlot)
        downloadButton(ns("downloadPlot"), label = "Download plot")
      })

      output$downloadPlot <- downloadHandler(
        filename = function() {
          paste0(rvals$fs_file, "_fedupPlot", ".png")
        },
        content = function(file) {
          device <- function(..., width, height) grDevices::png(..., width = 9,
            height = 5.5, res = 300, units = "in")
          ggsave(file, plot = p, device = device)
      })

      # Download table
      output$getData <- renderUI({
        req(fedupPlot)
        downloadButton(ns("downloadData"), label = "Download table")
      })

      output$downloadData <- downloadHandler(
       filename = function() {
         paste0(rvals$fs_file, "_fedupTable", ".txt")
       },
       content = function(con) {
         fwrite(fedupPlot, con, sep = "\t", sep2 = c("", " ", ""))
      })
    })


    ######
    # STEP 3
    ######


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

      resultsFolder <- tempdir()
      writeFemap(rvals$fs_results, resultsFolder)
      gmtFile <- tempfile("pathwaysGMT", fileext = ".gmt")
      writePathways(rvals$fs_set, gmtFile)



      ##### NOTE find way to deal with this .... ####
      cmd <- paste0("rm ", resultsFolder, "/*/0.txt")
     # system(cmd)


      plotFemap(
        gmtFile = gmtFile,
        resultsFolder = resultsFolder,
        qvalue = rvals$fs_fdr,
        chartData = "DATA_SET",
        hideNodeLabels = TRUE,
        netName = "test"
      )
    })
  })
}
