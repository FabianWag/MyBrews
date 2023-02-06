#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import ggplot2
#' @import dplyr
#' @import forcats
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      dashboardHeader(title = "MyBrews"),
      dashboardSidebar(
        menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
        menuItem("Efficiency", tabName = "breweff", icon = icon("th")),
        menuItem("Brews", tabName = "indbrew", icon = icon("th"))
      ),
      dashboardBody(
        tabItems(
          #First tab content
          tabItem(tabName = "overview",
                    mod_brew_ui("brew_1")),

          tabItem(tabName = "breweff"),

          tabItem(tabName = "indbrew")
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
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "MyBrews"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
