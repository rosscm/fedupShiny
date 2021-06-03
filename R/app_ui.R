#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @import shiny
#' @import shinydashboard
#' @importFrom shinydashboardPlus userDescription userBox
#'
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    dashboardPage(
      skin = "purple",
      dashboardHeader(
        title = "fedupShiny",
        titleWidth = 320
      ),
      dashboardSidebar(
        width = 320,
        sidebarMenu(
          menuItem("Introduction and Usage", tabName = "usage", icon = icon("question-circle"), badgeLabel = "README"),
          menuItem("Input Configuration", mod_input_ui("input"), icon = icon("upload")),
          menuItem("Output Dashboard", tabName = "output", icon = icon("tachometer-alt")),
          rep_br(2),
          col_2(
            h5("External links")
          ),
          rep_br(2),
          menuItem("Full Vignettes", href = "https://bioconductor.org/packages/release/bioc/html/fedup.html", icon = icon("file-alt")),
          menuItem(HTML("&nbsp;&nbsp;Source Code"), href = "https://github.com/rosscm/fedup", icon = icon("github")),
          menuItem("Cytoscape", href = "https://cytoscape.org/", icon = icon("project-diagram")),
          rep_br(9),
          userBox(
            title = userDescription(
              title = "Catherine Ross",
              subtitle = HTML("fedup v1.0 &middot; 2021"),
              type = 2,
              image = "http://1.gravatar.com/avatar/1e7a5aca287c8586a981246519fcb969.jpg",
            ),
            width = NULL,
            height = "1px",
            background = "purple",
            boxToolSize = "sm",
            collapsible = FALSE
          )
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName = "usage",
            mod_help_ui("help")
          ),
          tabItem(tabName = "output",
            mod_main_ui("main")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom waiter use_waiter
#' @importFrom shinyalert useShinyalert
#' @noRd
golem_add_external_resources <- function() {

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'fedupShiny'
    ),
    use_waiter(),
    useShinyalert(),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "www/styles.css"
    )
  )
}
